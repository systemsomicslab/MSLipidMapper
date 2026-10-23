# Reactomeを生物種ごとに取得・構築するコマンド

リポジトリのルートで実行します。`--list-species`と`--help`はPythonの標準ライブラリだけで動きます。
実際にRDFを構築する前に、同じPythonへビルド用依存関係を導入してください。

```powershell
python -m pip install -r scripts/requirements-knowledge.txt
```

## 対応生物種を調べる

```powershell
python scripts/reactome_pathways.py --list-species
```

Reactome APIから、パスウェイを持つ主要生物種のTaxonomy IDと学名を取得します。
この一覧にある生物種でも、対象の脂質代謝ルートが存在しなければスキップします。

## 取得からRDF構築まで実行する

```powershell
# ヒト
python scripts/reactome_pathways.py --taxon 9606

# ラット
python scripts/reactome_pathways.py --taxon 10116

# ゼブラフィッシュ
python scripts/reactome_pathways.py --taxon 7955

# 複数種を順番に実行
python scripts/reactome_pathways.py --taxon 9606 10116 7955

# 対応生物種すべてをまとめて実行
python scripts/reactome_pathways.py --all-species --run-id all-species-first
```

`--taxon`はNCBI Taxonomy IDです。マウスは10090です。各種の公式
`Metabolism of lipids`階層から、パスウェイ・反応イベント・SBMLを取得し、
RDF/JSON-LD、脂質クラスのRDF、検索用SQLite/CSVと検討待ち一覧を構築します。
LLMを使用しません。取得後のデータセットはGUIに自動検出されます。

デフォルトの保存先（`<実行日>`はUTCの日付）：

```text
inst/extdata/pathway_knowledge/snapshots/reactome-<Taxonomy ID>-<実行日>/
inst/extdata/pathway_knowledge/drafts/reactome-<Taxonomy ID>-<実行日>/
```

RでGUIを起動または再起動し、Organism / datasetから対象種を選びます。
実行中のGUIは起動時にデータセット一覧を読み込むため、新規追加後は再起動してください。

```r
MSLipidMapper::run_pathway_database_app()
```

## 再開・更新・オフライン再構築

同じTaxonomy ID・同じ`--run-id`で再実行すると、URL・リクエスト・ハッシュが一致する
キャッシュを再利用します。既存のスナップショットは更新取得用に使い回さず、新しい
`--run-id`を指定します。取得開始・終了のReactomeリリースが違う場合は停止します。

```powershell
python scripts/reactome_pathways.py --taxon 9606 --run-id 2026-10-15-r97

# 同じスナップショットからネット接続なしで再構築
python scripts/reactome_pathways.py --taxon 9606 --run-id 2026-10-15-r97 --offline

# 次回の更新取得には別のラベルを指定
python scripts/reactome_pathways.py --taxon 9606 --run-id next-update
```

`--offline`は完成済みの`manifest.json`を必要とします。未取得の生物種のデータは生成しません。
脂質代謝ルートが存在しない種はスキップします。複数種の処理中に取得・構築エラーがあっても、
残りの種の処理を続けます。完了・スキップ・失敗は画面と
`drafts/reactome-run-<run-id>.json`に記録します。実際の失敗が残った場合は終了コード1、
スキップのみなら終了コード0です。単一種の実際のエラーはその場で停止します。
それ以前に完成した生物種の成果物は保持されます。
同じコマンドで再実行すると、完了済みの取得キャッシュも再利用できます。

既存マウスデータのように保存先の名前が異なる場合は、単一種に対して両方を指定します。

```powershell
python scripts/reactome_pathways.py --taxon 10090 --offline --snapshot inst/extdata/pathway_knowledge/snapshots/reactome-mouse-2026-10-15 --output inst/extdata/pathway_knowledge/drafts/mouse-reactome-2026-10-15
```

## 分類辞書と保存先

既定では、取得済みのChEBI/LIPID MAPS辞書
`snapshots/lipid-census-2026-10-15`を全生物種で共有します。
辞書を別途更新した場合は、同じ形式のスナップショットを`--classifier`で指定します。
辞書のファイルとハッシュを検証してから処理します。

```powershell
python scripts/reactome_pathways.py --taxon 9606 --classifier path/to/classification-snapshot
python scripts/reactome_pathways.py --taxon 9606 --snapshot-root path/to/snapshots --output-root path/to/drafts
python scripts/reactome_pathways.py --help
```

独自の`--output-root`を指定した成果物はパッケージGUIの自動検出対象外です。
GUIで閲覧するには通常の`inst/extdata/pathway_knowledge/drafts/`を使用します。
ビルドには書き込み可能なソースチェックアウトと、共有の
`source/id-registry.json`が必要です。ID台帳は保持し、分類ノードIDを生物種間で共有します。
生物種ごとのデータセット・投影反応は別々に管理します。

## 根拠と限界

Reactomeの推定フラグは種ごとの元データを保持し、true/false/不明を区別します。
他種への実験的根拠を自動で付与しません。イベントのtaxonが指定種と一致しない場合は
停止します。集合・複合体の構成分子を遊離基質として展開しない分類ルール、未対応情報の
保持、データライセンスと出典の記録はマウス版と同じです。

検証は、実APIからの対応種一覧取得、既存マウス全711イベントのオフライン再構築、
ヒト・ラットを模した固定データによる取得からRDF構築までのテストで実施しています。
この変更ではヒト・ラット等の実データ全件取得は実行していません。

マウス収録内容の詳細：[Reactome catalog](reactome-pathway-catalog.md)。
