# Standalone REST API

The REST service accepts an MS-DIAL Alignment CSV or mzTab-M file and a YAML
configuration, then exports the bundled remodeling, ceramide and global pathways
as PDFs, JSON-LD measurements, or both. It runs independently of Shiny. Generic CSV, custom networks and other
Shiny analysis outputs are not exposed by this first API version.

## Start with Docker Compose

From the repository directory on the server:

```bash
unset MSLIPIDMAPPER_API_TOKEN
docker compose -f compose.api.yml up -d --build
curl http://127.0.0.1:8000/healthz
```

The default Compose setup publishes port 8000 on all server interfaces and
requires no token. From another lab computer, call `http://SERVER_LAN_IP:8000`.
For example, if the server's LAN IP is `192.168.1.100`, the endpoint is
`http://192.168.1.100:8000/v1/jobs`. Replace this example IP with your server's
actual address. The host firewall must allow port 8000 from the lab network.

To publish only on a specific LAN interface, set
`export MSLIPIDMAPPER_BIND_ADDRESS=192.168.1.100` before starting Compose.
The container still listens internally on `0.0.0.0`; the bind address selects
the host interface. Clients use the actual server IP, never `0.0.0.0`.

In PowerShell, clear the token with `$env:MSLIPIDMAPPER_API_TOKEN = ''` and set
an optional interface with `$env:MSLIPIDMAPPER_BIND_ADDRESS = '192.168.1.100'`.
If an earlier `.env` file defines a token, remove that entry or make it empty too.

The `api-jobs` named volume persists inputs, sanitized settings, status, results
and worker logs. `docker compose -f compose.api.yml down` keeps this volume;
do not use `down -v` if you need the saved jobs.

The API uses the existing Dockerfile and replaces its Shiny startup command.
The Compose healthcheck targets port 8000, not Shiny's port 3838. The container
restarts automatically after a process failure or server reboot when Docker
starts. No Shiny browser session is required.

## Start an installed R package

Install the updated package, then run:

```r
MSLipidMapper::run_mslipidmapper_api(
  host = "0.0.0.0",
  port = 8000,
  data_dir = "/srv/mslipidmapper/jobs",
  token = ""
)
```

An empty or unset `MSLIPIDMAPPER_API_TOKEN` disables authentication. Set a token
of at least 16 characters only if you want to enable Bearer authentication.
The R function and installed launcher default to loopback; use `host = "0.0.0.0"`
or `MSLIPIDMAPPER_API_HOST=0.0.0.0` for direct LAN access outside Compose.
Chrome/Chromium is required for PDF rendering; the Docker image
already installs it. The current pathway renderer loads Cytoscape.js from
`unpkg.com`, so workers require outbound HTTPS access to that host.

The installed launcher `scripts/mslipidmapper-api.R` also accepts these
environment variables:

| Variable | Default | Meaning |
|---|---|---|
| `MSLIPIDMAPPER_API_HOST` | `127.0.0.1` | Listening interface |
| `MSLIPIDMAPPER_API_PORT` | `8000` | Listening port |
| `MSLIPIDMAPPER_API_TOKEN` | Empty | No authentication; non-empty enables a shared Bearer token |
| `MSLIPIDMAPPER_JOB_DIR` | `mslipidmapper-jobs` | Persistent job directory |
| `MSLIPIDMAPPER_MAX_UPLOAD_MB` | `100` | Total HTTP request size limit in MiB |
| `MSLIPIDMAPPER_JOB_TIMEOUT` | `900` | Maximum worker runtime in seconds |
| `MSLIPIDMAPPER_MAX_PENDING` | `32` | Maximum queued plus running jobs |

Compose sets explicit values for these variables; edit its environment entries
to change the resource limits.

## Browser console

Open `http://SERVER_LAN_IP:8000/` in a browser for a GUI that talks to the API.
It lets you choose the input file, load and edit the YAML parameters as text
(with a preview of the server's validation rules), submit the job, follow its
status, and preview or download the results (PDF and/or JSON-LD). The editor starts from a default
mzTab-M configuration with `output.format: both` (PDF plus JSON-LD). For each action it also shows the equivalent `curl`
command (bash or PowerShell) with the current URL, file names and job ID filled
in. If you edit the parameters, use "Save YAML" so the saved `config.yml`
matches the generated command.

The page is served by the API itself (`/` and the bundled `/ui/` assets), so no
CORS setup is needed; opening `inst/api-ui/index.html` directly from disk does
not work. The page holds no data and is public even when a token is enabled;
enter the token in the page to authenticate its API calls. Job history is kept
only in that browser.

## Submit files

In the default lab-network configuration, no Authorization header is needed.
Anyone who can reach the API can submit analyses and retrieve saved results.

```bash
export MSLM_URL=http://192.168.1.100:8000
curl --fail-with-body \
  -F 'input=@inst/extdata/examples/Lipid_AlignmentTable.csv' \
  -F 'config=@inst/extdata/examples/pathway-api.yml' \
  "$MSLM_URL/v1/jobs"
```

In Windows PowerShell, use `curl.exe`; set `$env:MSLM_URL` and use it in URLs.
`-F` selects POST automatically. Do not manually set Content-Type: curl supplies
the multipart boundary. Exactly two non-empty file parts, `input` and `config`,
are required. The configuration may be at most 1 MiB.

Response: HTTP `202 Accepted`, with a `Location` header and JSON:

```json
{
  "job_id": "job_0123456789abcdef0123456789abcdef",
  "status": "queued",
  "status_url": "/v1/jobs/job_0123456789abcdef0123456789abcdef"
}
```

Acceptance means the files were saved and basic request/configuration validation
passed. The worker validates the actual lipidomics contents; an invalid table
can therefore become a failed job after a successful POST.

For mzTab-M, upload the `.mzTab` or `.mztab` file and use, for example:

```yaml
input:
  format: mztab-m
analysis:
  normalization: none
  aggregation: sum
  group_column: class
plot:
  type: dot
output:
  paper_size: A4
  orientation: landscape
```

Choose `group_column` for the metadata in your file. Groups listed in
`exclude_groups` that do not occur in the data are ignored (reported as
`ignored_exclude_groups` in the job result); unknown `include_groups` are still
an error. If `input.format` is
omitted it is inferred from the uploaded extension. The API accepts the CLI's
analysis and plot settings, including group and acyl-chain filters.
`input.file`, `input.lipidomics`, `input.mztab`, `output.directory` and
`output.pdf` are discarded: all paths are allocated by the server.
`pathway.browser`, `pathway.network`, custom acyl-chain rules and unknown
configuration fields are rejected. Set the browser on the server using
`MSLIPIDMAPPER_BROWSER`. Individual pathway timeouts are limited to 1–300 seconds
and SVG dimensions to 0.5–20 inches.

## Check progress and download

### JSON-LD output and optional acyl subsets

Set `output.format: jsonld` for RDF/JSON-LD only, `both` for PDF plus JSON-LD,
or `pdf` (default) for the existing PDF behavior. JSON-LD-only jobs require no
Chrome/Chromium and no Cytoscape CDN access.

```yaml
input:
  format: mztab-m
analysis:
  normalization: none
  aggregation: sum
  group_column: class
output:
  format: jsonld
measurements:
  include_subsets: true
  acyl_chains: ["18:0", "24:1"]
  unit: arbitrary
```

`measurements.include_subsets` defaults to `false`: only whole-subclass sample
aggregates are produced. `true` adds independent acyl-chain subset aggregates.
When `acyl_chains` is omitted, all parsed chains are selected; when supplied,
only those chains are used. Supplying chains with subsets disabled is rejected.
This option controls sample-level aggregation, not p-values or FC. Whole-subclass
values for matched lipid nodes and all original molecular sample values are
retained in JSON-LD. A pathway with no matching lipid node has no node aggregates.

Analysis normalization, aggregation and group/feature filters apply to both
PDF and JSON-LD exports. `analysis.acyl_chain_filter` removes molecules before
export, whereas `measurements.acyl_chains` defines additional subsets without
removing molecules from the whole-subclass values.

The artifacts are `remodeling-measurements.json`, `ceramide-measurements.json`
and `global-measurements.json`, served as `application/ld+json`. Download using
the existing artifact route, for example:

```bash
curl --fail-with-body "$MSLM_URL/v1/jobs/$JOB_ID/artifacts/remodeling-measurements.json" \
  -o remodeling-measurements.json
```

The completed job's `result.measurements` reports sample/feature counts,
normalization, aggregation, mapped-node counts and the selected subset chains.
See [the JSON-LD schema](pathway-measurements.md) and
`inst/extdata/examples/pathway-api-jsonld.yml`.

```bash
JOB_ID=job_0123456789abcdef0123456789abcdef
curl --fail-with-body \
  "$MSLM_URL/v1/jobs/$JOB_ID"

curl --fail-with-body \
  "$MSLM_URL/v1/jobs/$JOB_ID/artifacts"

curl --fail-with-body \
  "$MSLM_URL/v1/jobs/$JOB_ID/artifacts/remodeling.pdf" -o remodeling.pdf
```

Job status is `queued`, `running`, `succeeded` or `failed`. Successful job status
includes a result summary, mapping counts and artifact filenames. The artifact
listing includes sizes and download URLs. All returned URLs are relative to the
API origin. The download endpoint returns `application/pdf` or `application/ld+json` bytes.

| Response | Meaning |
|---|---|
| 200 | Health, status, artifact list or download succeeded |
| 202 | Job accepted |
| 400 | Invalid upload or unsupported configuration |
| 401 | Missing or incorrect token when optional authentication is enabled |
| 404 | Unknown job or artifact |
| 409 | Results requested before successful completion |
| 413 | Request or configuration exceeds the size limit |
| 415 | Request is not multipart/form-data |
| 429 | Queue full; retry later |
| 500 | Server error; inspect API logs |

Errors are JSON objects with an `error` field. Repeating a POST creates a new
job; this version does not implement an idempotency key.

## Optional token authentication

To enable authentication later, set `MSLIPIDMAPPER_API_TOKEN` to a shared token
of at least 16 characters and recreate the Compose service. Add
`-H "Authorization: Bearer $MSLIPIDMAPPER_API_TOKEN"` to each curl request.
All `/v1/` endpoints then require that token; `/healthz` remains public.
The shared token grants access to all jobs, not per-user ownership. A short
non-empty token is rejected at startup rather than silently disabling auth.

## Operations

One separate R worker runs at a time. Requests stay responsive during analysis;
the queue advances without clients polling. A process lock prevents two API
instances using the same data directory. Deploy one replica per job volume.

Queued jobs survive restart. Running jobs interrupted by shutdown or a crash
are marked failed; submit them again. A crashed worker or elapsed timeout also
marks its job failed, and the next queued job starts.

For diagnostics, read `<job directory>/<job_id>/worker.log` on the server, or
use `docker compose -f compose.api.yml logs api` for API errors. Logs and inputs
are not publicly served. Job directories are retained indefinitely. Monitor
disk usage, back up the volume as needed, and remove completed job directories
according to your lab's retention policy while the service is stopped.

Set an upload limit at the reverse proxy too (for nginx, `client_max_body_size
100m;`). The application checks request size before multipart parsing, but the
HTTP stack already buffers the request body. Configure server/container memory
and disk limits for your dataset sizes. HTTPS and network access restrictions
belong at the proxy/host layer.

## Developer verification

`tests/testthat/test-rest-api.R` covers configuration restrictions, queue
capacity, persistent states, locking, worker failures and timeout handling.
`tests/rest/test_http.py` exercises the actual HTTP API and downloads the three
PDFs from the bundled CSV example, then checks mzTab-M processing. Run it against
a test service (it creates persistent test jobs):

```bash
python tests/rest/test_http.py --url http://127.0.0.1:8000 --no-auth
```

To verify JSON-LD uploads, subset options and downloads without PDF rendering:

```bash
python tests/rest/test_http.py --url http://127.0.0.1:8000 --no-auth --jsonld-only
```

For an authenticated service, omit `--no-auth`; the test reads the token from
`MSLIPIDMAPPER_API_TOKEN`. The optional
`tests/rest/serve.R` helper is for a disposable Linux development container with
the source checkout mounted read-only at `/src`; it installs the current package
inside that container and starts a service with a 2 MiB upload limit.
