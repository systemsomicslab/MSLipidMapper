import json
from pathlib import Path

root = Path(__file__).resolve().parents[1]
source = root / 'inst/examples/remodeling_lipidonly.cyjs'
raw = json.loads(source.read_text(encoding='utf-8'))['elements']
nodes = [{'id': n['data']['id'], 'label': n['data'].get('label', ''), **n['position']} for n in raw['nodes']]
edges = [{'source': e['data']['source'], 'target': e['data']['target']} for e in raw['edges']]
template = Path(__file__).with_name('pathway_demo_template.html').read_text(encoding='utf-8')
out = root / 'output/pathway-viewer-demo.html'
out.parent.mkdir(exist_ok=True)
out.write_text(template.replace('__NETWORK__', json.dumps({'nodes': nodes, 'edges': edges})), encoding='utf-8')
print(out)
