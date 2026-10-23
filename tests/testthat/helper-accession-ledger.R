# Keep tests isolated from the user's persistent experimental accession ledger.
options(MSLipidMapper.id_registry = file.path(tempdir(), "mslm-test-accessions.sqlite"))
