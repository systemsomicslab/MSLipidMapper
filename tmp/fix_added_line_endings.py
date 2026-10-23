import difflib
import subprocess
from pathlib import Path
for name in ['R/Mod_cyto.R', 'R/cyto_js_deps.R', 'R/package_paths.R']:
    original = subprocess.check_output(['git', 'show', 'HEAD:' + name]).splitlines(keepends=True)
    current = Path(name).read_bytes().splitlines(keepends=True)
    matcher = difflib.SequenceMatcher(None, [x.rstrip(b'\r\n') for x in original],
                                    [x.rstrip(b'\r\n') for x in current], autojunk=False)
    result=[]
    for tag, a, b, c, d in matcher.get_opcodes():
        if tag == 'equal':
            result.extend(original[a:b])
        else:
            result.extend(line.rstrip(b'\r\n') + b'\n' for line in current[c:d])
    Path(name).write_bytes(b''.join(result))
