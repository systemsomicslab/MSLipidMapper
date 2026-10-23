import json
from pathlib import Path
root=Path(__file__).resolve().parents[1]
raw=json.loads((root/'inst/examples/remodeling_lipidonly.cyjs').read_text(encoding='utf-8'))['elements']
network={'nodes':[{'id':n['data']['id'],'label':n['data'].get('label',''),**n['position']} for n in raw['nodes']], 'edges':[{'source':e['data']['source'],'target':e['data']['target']} for e in raw['edges']]}
out=root/'output/pathway-figure-composer-demo.html'
out.write_text(Path(__file__).with_name('pathway_composer.html').read_text(encoding='utf-8').replace('__NETWORK__',json.dumps(network)),encoding='utf-8')
print(out)
