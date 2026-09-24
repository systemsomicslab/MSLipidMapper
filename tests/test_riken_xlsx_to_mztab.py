import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "scripts" / "riken_xlsx_to_mztab.py"
SPEC = importlib.util.spec_from_file_location("riken_converter", SCRIPT)
converter = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules[SPEC.name] = converter
SPEC.loader.exec_module(converter)


class RikenConverterTest(unittest.TestCase):
    def make_workbook(self, path: Path):
        wb = converter.openpyxl.Workbook()
        ws = wb.active
        ws.title = "alignment"
        labels = ["Public/Private", "Category", "Tissue/Species", "Genotype/Background",
                  "Perturbation", "Diet/Culture", "Biological replicate", "Technical replicate", "Unit"]
        for row, label in enumerate(labels, 1):
            ws.cell(row, 5, label)
        ws.cell(1, 6, "Public"); ws.cell(1, 8, "Private")
        ws.cell(2, 6, "Mouse"); ws.cell(2, 8, "Mouse")
        ws.cell(3, 6, "Liver"); ws.cell(3, 8, "Liver")
        ws.cell(9, 6, "Height"); ws.cell(9, 8, "pmol/mg tissue")
        headers = ["Alignment ID", "Average Mz", "Metabolite name", "Ontology", "MS/MS spectrum",
                   "raw_public", "Lipid IS", "normalized_private"]
        for column, header in enumerate(headers, 1): ws.cell(10, column, header)
        values = [1, 760.6, "PC 34:1", "PC", "184.07:100", 1000, None, 2.5]
        for column, value in enumerate(values, 1): ws.cell(11, column, value)
        wb.save(path)

    def test_boundaries_and_public_filter(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "input.xlsx"
            self.make_workbook(source)
            raw = converter.parse_workbook(source, abundance="raw", privacy="public")[0]
            self.assertEqual(raw.sample_names, ["raw_public"])
            self.assertEqual(raw.abundance_columns, [5])
            with self.assertRaisesRegex(ValueError, "no samples remain"):
                converter.parse_workbook(source, abundance="normalized", privacy="public")

    def test_written_table_is_rectangular(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "input.xlsx"
            output = Path(tmp) / "output.mzTab"
            self.make_workbook(source)
            parsed = converter.parse_workbook(source, abundance="normalized", privacy="all")[0]
            converter.write_mztab(parsed, output, ["category", "tissue/species"])
            rows = [line.rstrip("\n").split("\t") for line in output.read_text(encoding="utf-8").splitlines()]
            header = next(row for row in rows if row[0] == "SMH")
            molecules = [row for row in rows if row[0] == "SML"]
            self.assertTrue(molecules)
            self.assertTrue(all(len(row) == len(header) for row in molecules))
            self.assertIn("abundance_assay[1]", header)


if __name__ == "__main__":
    unittest.main()
