"""Backward-compatible mouse entry point; use reactome_pathways.py for any taxon."""
import argparse
from collect_reactome import (BASE, object_index, root_records, request, index_hierarchy,
                              collect as collect_species)


def collect(output):
    return collect_species(output, "10090")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", required=True)
    collect(parser.parse_args().output)
