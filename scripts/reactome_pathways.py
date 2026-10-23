"""Collect Reactome lipid pathways and build organism-specific RDF/search data.

Examples: python scripts/reactome_pathways.py --list-species
          python scripts/reactome_pathways.py --taxon 9606 10116
"""
import argparse
import json
import re
from datetime import datetime, timezone
from pathlib import Path
from reactome_species import list_species, normalize_taxon, MissingLipidPathway

KNOWLEDGE = Path(__file__).resolve().parents[1] / 'inst/extdata/pathway_knowledge'


def collect(*args):
    from collect_reactome import collect as implementation
    return implementation(*args)


def build(*args):
    from build_reactome_catalog import build as implementation
    return implementation(*args)


def parser():
    p = argparse.ArgumentParser(description=__doc__)
    choice = p.add_mutually_exclusive_group(required=True)
    choice.add_argument('--list-species', action='store_true', help='Fetch supported Reactome main species and Taxonomy IDs')
    choice.add_argument('--taxon', nargs='+', type=normalize_taxon, help='One or more NCBI Taxonomy IDs')
    choice.add_argument('--all-species', action='store_true', help='Process every Reactome main species, skipping species without the lipid pathway')
    p.add_argument('--run-id', default=datetime.now(timezone.utc).date().isoformat(), help='Snapshot label; default: current UTC date. Use a new label for a new release.')
    p.add_argument('--classifier', type=Path, default=KNOWLEDGE / 'snapshots/lipid-census-2026-10-15', help='Archived ChEBI/LIPID MAPS classification snapshot')
    p.add_argument('--snapshot-root', type=Path, default=KNOWLEDGE / 'snapshots')
    p.add_argument('--output-root', type=Path, default=KNOWLEDGE / 'drafts', help='Default drafts are automatically discovered by the package GUI')
    p.add_argument('--snapshot', type=Path, help='Explicit snapshot directory (single taxon only)')
    p.add_argument('--output', type=Path, help='Explicit output directory (single taxon only)')
    p.add_argument('--offline', action='store_true', help='Build from a completed local snapshot without any network access')
    return p


def run(args):
    if args.list_species:
        if args.offline:
            raise ValueError('--list-species requires network access; it cannot be combined with --offline')
        rows = list_species()
        print('taxon_id\tscientific_name')
        for row in rows:
            print(row['taxon_id'] + '\t' + row['scientific_name'])
        return []
    try:
        import pathway_knowledge as kb
    except ModuleNotFoundError as exc:
        if exc.name != 'rdflib':
            raise
        raise ValueError('RDF dependencies are missing. Run: python -m pip install -r scripts/requirements-knowledge.txt') from exc
    if not re.fullmatch(r'[A-Za-z0-9][A-Za-z0-9._-]{0,79}', args.run_id):
        raise ValueError('--run-id must be a short directory label containing only letters, digits, dot, hyphen or underscore')
    if args.all_species and args.offline:
        raise ValueError('--all-species needs the online species list; use --taxon IDs for offline builds')
    taxa = list(dict.fromkeys([s['taxon_id'] for s in list_species()] if args.all_species else args.taxon))
    if (args.snapshot or args.output) and (len(taxa) != 1 or not (args.snapshot and args.output)):
        raise ValueError('--snapshot and --output must be specified together for one taxon')
    for name in ('manifest.json', 'chebi-all-is-a.json', 'chebi-all-chemical-forms.json', 'chebi-lipidmaps-xrefs-v3.json', 'lipidmaps-vocabulary.json'):
        if not (args.classifier / name).is_file():
            raise ValueError('Missing classification snapshot file: ' + str(args.classifier / name))
    results, skipped, failed = [], [], []
    for taxon in taxa:
        label = 'reactome-' + taxon + '-' + args.run_id
        snapshot = args.snapshot or args.snapshot_root / label
        output = args.output or args.output_root / label
        try:
            if snapshot.resolve() == output.resolve():
                raise ValueError('Snapshot and output must be separate directories')
            for path, metadata in ((snapshot, 'manifest.json'), (output, 'report.json')):
                if (path / metadata).exists() and str(kb.read_json(path / metadata)['taxon_id']) != taxon:
                    raise ValueError('Directory belongs to another taxon: ' + str(path))
            if not args.offline:
                collect(snapshot, taxon)
            if not (snapshot / 'manifest.json').is_file():
                raise ValueError('No completed snapshot found: ' + str(snapshot))
            if str(kb.read_json(snapshot / 'manifest.json')['taxon_id']) != taxon:
                raise ValueError('Snapshot taxon differs from --taxon')
            build(snapshot, args.classifier, output)
            report = kb.read_json(output / 'report.json')
            results.append({'taxon_id': taxon, 'scientific_name': report['scientific_name'],
                            'snapshot': str(snapshot.resolve()), 'output': str(output.resolve()),
                            'event_count': report['candidate_count'], 'pathway_count': report['pathway_count']})
        except MissingLipidPathway as exc:
            skipped.append({'taxon_id': taxon, 'reason': str(exc)})
            print('SKIPPED ' + taxon + ': ' + str(exc), flush=True)
        except (ValueError, OSError, KeyError) as exc:
            if len(taxa) == 1:
                raise
            failed.append({'taxon_id': taxon, 'reason': str(exc)})
            print('FAILED ' + taxon + ': ' + str(exc), flush=True)
    summary = {'run_id': args.run_id, 'requested_taxa': taxa, 'completed': results, 'skipped': skipped, 'failed': failed}
    summary_path = (args.output.parent if args.output else args.output_root) / ('reactome-run-' + args.run_id + '.json')
    kb.write_json(summary_path, summary)
    print(json.dumps(summary, indent=2, ensure_ascii=False), flush=True)
    print('Run report: ' + str(summary_path.resolve()), flush=True)
    if failed:
        raise ValueError(str(len(failed)) + ' species failed; other species were processed. See ' + str(summary_path))
    return results


def main(argv=None):
    p = parser()
    args = p.parse_args(argv)
    try:
        run(args)
    except (ValueError, OSError, KeyError) as exc:
        p.exit(1, 'Reactome import failed: ' + str(exc) + '\n')


if __name__ == '__main__':
    main()
