"""HTTP integration test against a disposable REST service (stdlib only)."""
import argparse
import http.client
import json
import os
from pathlib import Path
import time
import urllib.error
import urllib.request
import uuid


def multipart(parts):
    boundary = "mslm-" + uuid.uuid4().hex
    body = bytearray()
    for name, filename, value in parts:
        body.extend((f'--{boundary}\r\nContent-Disposition: form-data; name="{name}"; '
                     f'filename="{filename}"\r\nContent-Type: application/octet-stream\r\n\r\n').encode())
        body.extend(value)
        body.extend(b"\r\n")
    body.extend(f"--{boundary}--\r\n".encode())
    return bytes(body), f"multipart/form-data; boundary={boundary}"


def mztab_fixture():
    rows = [["MTD", "mzTab-version", "2.0.0-M"]]
    for i in range(1, 5):
        rows.append(["MTD", f"assay[{i}]", f"S{i}"])
    for i, group in enumerate(["Control", "Treatment"], 1):
        rows += [["MTD", f"study_variable[{i}]", group],
                 ["MTD", f"study_variable[{i}]-assay_refs", f"assay[{2*i-1}]|assay[{2*i}]"],
                 ["MTD", f"study_variable[{i}]-description", group]]
    rows.append(["SMH", "SML_ID", "chemical_name", "chemical_formula", "theoretical_neutral_mass",
                 *[f"abundance_assay[{i}]" for i in range(1, 5)], "opt_global_Ontology"])
    rows += [["SML", "1", "PC 16:0_18:1", "C42H82NO8P", "759.5778", "10", "12", "20", "24", "PC"],
             ["SML", "2", "PE 18:0_18:1", "C41H80NO8P", "745.5621", "8", "9", "14", "16", "PE"]]
    return ("\n".join("\t".join(row) for row in rows) + "\n").encode()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--url", default="http://127.0.0.1:8000")
    parser.add_argument("--upload-limit-mb", type=int)
    parser.add_argument("--no-auth", action="store_true", help="Test the lab-network service without a token")
    parser.add_argument("--output", default=".api-validation")
    parser.add_argument("--jsonld-only", action="store_true", help="Exercise JSON-LD jobs without PDF/browser rendering")
    args = parser.parse_args()
    token = "" if args.no_auth else os.environ["MSLIPIDMAPPER_API_TOKEN"]
    root = Path(__file__).resolve().parents[2]
    output = Path(args.output)
    output.mkdir(parents=True, exist_ok=True)

    def request(path, method="GET", parts=None, auth=True, expected=200, raw=None, content_type=None):
        headers = {"Authorization": f"Bearer {token}"} if auth and token else {}
        if parts is not None:
            raw, content_type = multipart(parts)
        if content_type:
            headers["Content-Type"] = content_type
        req = urllib.request.Request(args.url.rstrip("/") + path, data=raw,
                                     headers=headers, method=method)
        try:
            response = urllib.request.urlopen(req, timeout=30)
        except urllib.error.HTTPError as exc:
            response = exc
        with response:
            body = response.read()
            assert response.status == expected, (path, response.status, body[:500])
            if expected == 200 and path.endswith("-measurements.json"):
                assert response.headers.get_content_type() == "application/ld+json"
            if response.headers.get_content_type() == "application/json":
                return json.loads(body)
            return body

    deadline = time.monotonic() + 120
    while True:
        try:
            assert request("/healthz", auth=False)["ok"] is True
            break
        except (urllib.error.URLError, http.client.RemoteDisconnected, ConnectionError):
            if time.monotonic() > deadline:
                raise
            time.sleep(1)
    # The browser console is public (it holds no data) and serves only bundled files.
    page = request("/", auth=False)
    assert page.startswith(b"<!doctype html>") and b"MSLipidMapper API Console" in page
    assert b"js-yaml" in request("/ui/js-yaml.min.js", auth=False)[:200]
    request("/ui/rest_api.R", auth=False, expected=404)
    request("/ui/%2e%2e%2fDESCRIPTION", auth=False, expected=404)
    print("PASS: browser console", flush=True)
    unknown = "/v1/jobs/job_" + "0" * 32
    request(unknown, auth=False, expected=404 if args.no_auth else 401)
    request(unknown, expected=404)
    request("/v1/jobs", method="POST", raw=b"{}", content_type="application/json", expected=415)
    request("/v1/jobs", method="POST", parts=[("input", "x.csv", b"x")], expected=400)
    request("/v1/jobs", method="POST", parts=[("input", "x.csv", b"x"),
            ("input", "y.csv", b"y")], expected=400)
    request("/v1/jobs", method="POST", parts=[("input", "x.rds", b"x"),
            ("config", "config.yml", b"{}")], expected=400)
    request("/v1/jobs", method="POST", parts=[("input", "x.csv", b""),
            ("config", "config.yml", b"{}")], expected=400)
    request("/v1/jobs", method="POST", parts=[("input", "x.csv", b"x"),
            ("config", "config.yml", b"x" * (1024**2 + 1))], expected=413)
    for config in [b"pathway:\n  browser: /bin/sh", b"pathway:\n  network: /etc/passwd",
                   b"analysis:\n  acyl_chain_filter:\n    rules: /etc/passwd", b"[unterminated",
                   b"output:\n  format: rds", b'measurements:\n  include_subsets: "true"',
                   b'measurements:\n  acyl_chains: ["18:0"]']:
        request("/v1/jobs", method="POST", parts=[("input", "x.csv", b"x"),
                ("config", "config.yml", config)], expected=400)
    if args.upload_limit_mb:
        request("/v1/jobs", method="POST", parts=[("input", "big.csv", b"x" * (args.upload_limit_mb * 1024**2)),
                ("config", "config.yml", b"{}")], expected=413)
    print("PASS: authentication, missing jobs, multipart validation and unsafe settings", flush=True)

    def submit(filename, data, config):
        job = request("/v1/jobs", method="POST", parts=[("input", filename, data),
                      ("config", "config.yml", config)], expected=202)
        assert isinstance(job["job_id"], str) and job["status"] == "queued", job
        print("Accepted", filename, job["job_id"], flush=True)
        return job

    def wait(job, expected):
        deadline = time.monotonic() + 600
        last = None
        while time.monotonic() < deadline:
            status = request(job["status_url"])
            if status["status"] != last:
                print(job["job_id"], status["status"], flush=True)
                last = status["status"]
            if status["status"] in ("failed", "succeeded"):
                assert status["status"] == expected, status
                return status
            # Health remains responsive even while R is rendering PDFs.
            assert request("/healthz", auth=False)["ok"] is True
            time.sleep(2)
        raise AssertionError("Job timed out: " + job["job_id"])

    bad = submit("broken.csv", b"not a lipidomics table", b"{}")
    request(bad["status_url"] + "/artifacts", expected=409)
    wait(bad, "failed")

    examples = root / "inst/extdata/examples"
    if args.jsonld_only:
        base = b"input:\n  format: mztab-m\n  use_rgoslin_if_no_ontology: false\noutput:\n  format: jsonld\n"
        variants = [
            ("whole", "fixture.mzTab", mztab_fixture(), base, False),
            ("subsets", "fixture.mzTab", mztab_fixture(), base +
             b'measurements:\n  include_subsets: true\n  acyl_chains: ["18:0"]\n', True),
            ("auto", "fixture.mzTab", mztab_fixture(), base +
             b'measurements:\n  include_subsets: true\n', True),
            ("csv-whole", "alignment.csv", (examples / "Lipid_AlignmentTable.csv").read_bytes(),
             b"output:\n  format: jsonld\n", False)
        ]
        for label, filename, data, config, subsets in variants:
            status = wait(submit(filename, data, config), "succeeded")
            assert status["result"]["measurements"]["include_subsets"] is subsets
            listing = request(status["artifacts_url"])
            assert {a["name"] for a in listing["artifacts"]} == {
                "remodeling-measurements.json", "ceramide-measurements.json", "global-measurements.json"}
            for artifact in listing["artifacts"]:
                payload = request(artifact["url"])
                doc = json.loads(payload)
                assert "@context" in doc and "@graph" in doc and "elements" not in doc
                scopes = [n for n in doc["@graph"] if n["@type"] == "mslm:LipidMeasurementScope"]
                if scopes:
                    assert any(n["scopeType"] == "subclass" for n in scopes)
                    assert any(n["scopeType"] == "acyl_subset" for n in scopes) is subsets
                else:
                    pathway = artifact['name'].split('-')[0]
                    assert status['result']['measurements']['pathways'][pathway]['measured_nodes'] == 0
                assert len(payload) == artifact["size_bytes"]
                (output / f"{label}-{artifact['name']}").write_bytes(payload)
            request(status["artifacts_url"] + "/worker.log", expected=404)
        print("PASS: CSV/mzTab-M JSON-LD jobs, disabled/selected/automatic subsets and 12 downloads", flush=True)
        llm = wait(submit("alignment.csv", (examples / "Lipid_AlignmentTable.csv").read_bytes(),
                          b"output:\n  format: [llm]\n"), "succeeded")
        listing = request(llm["artifacts_url"])
        assert {a["name"] for a in listing["artifacts"]} == {
            "remodeling-llm-summary.json", "ceramide-llm-summary.json", "global-llm-summary.json"}, listing
        for artifact in listing["artifacts"]:
            summary = request(artifact["url"])  # served as application/json, parsed by request()
            pathway = artifact["name"].split("-")[0]
            assert summary["format"] == "mslipidmapper-llm-summary/1" and summary["pathway"] == pathway
            assert "features" not in summary and llm["result"]["llm"][pathway]["reactions"] == len(summary["reactions"])
            for lipid in summary["lipids"].values():
                assert "status" in lipid or len(lipid["values"]) == len(summary["samples"]["ids"])
            assert summary["reactions"] and not any("enzymes" in r for r in summary["reactions"])
        print("PASS: per-pathway LLM summaries with reaction nodes", flush=True)
        return
    csv = submit("Lipid_AlignmentTable.csv", (examples / "Lipid_AlignmentTable.csv").read_bytes(),
                 (examples / "pathway-api.yml").read_bytes())
    mz = submit("fixture.mzTab", mztab_fixture(),
                b"input:\n  format: mztab-m\n  use_rgoslin_if_no_ontology: false\nanalysis:\n  group_column: class\n"
                b"  exclude_groups: [Blank]\n")
    results = []
    for label, job in [("csv", csv), ("mztab", mz)]:
        status = wait(job, "succeeded")
        assert any(v > 0 for v in status["result"]["mapped_nodes"].values()), status
        if label == "mztab":
            # Blank is absent from the fixture, so excluding it is ignored rather than an error.
            assert status["result"]["ignored_exclude_groups"] in ("Blank", ["Blank"]), status["result"]
        listing = request(status["artifacts_url"])
        assert {a["name"] for a in listing["artifacts"]} == {"remodeling.pdf", "ceramide.pdf", "global.pdf"}
        for artifact in listing["artifacts"]:
            request(artifact["url"], auth=False, expected=200 if args.no_auth else 401)
            data = request(artifact["url"])
            assert data.startswith(b"%PDF-") and len(data) == artifact["size_bytes"] and len(data) > 1000
            (output / f"{label}-{artifact['name']}").write_bytes(data)
        request(status["artifacts_url"] + "/worker.log", expected=404)
        request(status["artifacts_url"] + "/%2e%2e%2frequest.rds", expected=404)
        results.append(status)
    (output / "http-results.json").write_text(json.dumps(results, indent=2), encoding="utf-8")
    print("PASS: failed jobs, queued CSV/mzTab-M analyses, responsive health, six PDF downloads", flush=True)


if __name__ == "__main__":
    main()
