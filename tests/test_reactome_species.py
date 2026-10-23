"""Run with python -S to verify discovery needs no installed packages."""
import contextlib
import io
import json
import sys
import unittest
from pathlib import Path
from unittest.mock import patch

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / 'scripts'))
import reactome_pathways as command
import reactome_species as species


class SpeciesDiscoveryTests(unittest.TestCase):
    def test_listing_without_rdf_dependencies_and_with_integer_references(self):
        tree = [{'dbId': 1, 'taxId': 9606, 'displayName': 'Homo sapiens',
                 'nested': {'dbId': 2, 'taxId': 10090, 'displayName': 'Mus musculus'}}, 2]
        output = io.StringIO()
        with patch.object(species, 'urlopen', return_value=io.BytesIO(json.dumps(tree).encode())), contextlib.redirect_stdout(output):
            command.main(['--list-species'])
        self.assertIn('9606\tHomo sapiens', output.getvalue())
        self.assertIn('10090\tMus musculus', output.getvalue())

    def test_build_dependency_error_contains_install_command(self):
        original = __import__
        def missing(name, *args, **kwargs):
            if name == 'pathway_knowledge':
                raise ModuleNotFoundError("No module named 'rdflib'", name='rdflib')
            return original(name, *args, **kwargs)
        with patch('builtins.__import__', side_effect=missing):
            with self.assertRaisesRegex(ValueError, 'python -m pip install -r scripts/requirements-knowledge.txt'):
                command.run(command.parser().parse_args(['--taxon', '9606']))


if __name__ == '__main__': unittest.main()
