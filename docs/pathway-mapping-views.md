# パスウェイ別の脂質クラスマッピング

生物種ごとの原データを保持し、パスウェイと測定クラスで表示用の反応を抽出できます。
取得済み14種のReactomeデータ、以前のマウスReactomeデータ、マウス統合カタログに
グループ情報を生成済みです。今後のReactome取得・統合コマンドでも自動生成します。

## GUIで使う

GUIを再起動します。

```r
MSLipidMapper::run_pathway_database_app()
```

1. **Organism / dataset**：対象種とデータセットを選択します。
2. **Pathways**：名前またはReactome IDから1つ以上選びます。空ならパスウェイで絞りません。
   親パスウェイを選ぶと子孫の反応も含みます。複数指定は和集合です。
3. **Reaction view**：既定のLipid class mappingは、完全に分類済みで、両側に異なる
   脂質クラス集合がある生化学反応に絞ります。All source eventsで元のイベントを閲覧できます。
4. 必要に応じて**Measured lipid classes**で測定クラスを選びます。
5. CSVまたは**Download selected class RDF**で選択結果だけを保存します。
   元のDownload class pathway RDF / source reaction RDFは全件のままです。

測定クラスの接続には、少なくとも片側が測定済み、両側が測定済み、中間クラスを介して接続、
の3モードがあります。中間接続は最大1〜4反応の範囲で、測定クラス同士を結ぶ単純経路を選びます。
この接続は無向で、生理的反応方向・フラックス・酵素活性を推定しません。クラス集合の
組み合わせから作る表示用の接続であり、原子追跡や正味の化学的到達可能性は保証しません。
単一の測定クラスだけで中間接続を選ぶと結果は空になります。

既定条件では輸送、結合、制御・省略・未知カテゴリ、同一クラス内変換、不完全な分類、
片側だけのクラス割当を表示対象から外します。原データの削除ではありません。
反応カテゴリはReactomeのsource category（transition / binding / omittedなど）を使い、
Rheaは生化学反応として扱って既知のtransportを除外します。タイトルからの推測やLLMは使いません。
未知イベントが生化学反応として扱われることはありません。
Reactomeの推定反応は既定で含みます。Include source-inferred eventsを外すと除外します。
不明フラグを実験的根拠ありとは扱いません。

## Rheaのパスウェイ所属

Reactomeの元の所属と、Rheaの所属候補を区別します。

|根拠|グループの種別|既定で含む|
|---|---|---|
|Reactomeの直接所属|direct|はい|
|親パスウェイへの所属|ancestor|はい|
|確定した同一反応からの所属|verified-derived|はい|
|完全化学式の一致|equation-candidate|いいえ|
|出典参照だけの一致|reference-candidate|いいえ|
|脂質クラス集合の一致|class-candidate|いいえ|

Rhea pathway candidatesで候補を追加できます。反応表のpathway_membership_evidence列に
根拠を表示します。候補を追加しても元のReactome所属という事実には変更しません。
同じ反応が複数のパスウェイに属せます。所属がないRhea反応も元のカタログには残ります。
パスウェイ未指定なら未分類反応も他の選択条件に応じて表示されます。

## 実データでの例

マウスの統合データは原レコード3,661件から、パスウェイ未指定のマッピング条件で1,231件になります。

|パスウェイ|既定のマッピング表示|化学式一致候補も含む|クラス等の候補も含む|
|---|---:|---:|---:|
|Glycerophospholipid biosynthesis (R-MMU-1483206)|25|25|106|
|Sphingolipid metabolism (R-MMU-428157)|21|27|38|

既定のパスウェイ選択での反応は上記の例ではReactome由来です。候補追加後の件数は、
同一反応の確定件数でも、重複排除済み反応数でもありません。

## コマンドで抽出

リポジトリのルートで実行します。既存のPythonビルド用依存関係を使い、ネットワーク接続は不要です。

```powershell
# マウスのグリセロリン脂質生合成だけを抽出（25反応）
python scripts/pathway_mapping.py --catalog inst/extdata/pathway_knowledge/drafts/mouse-integrated-2026-10-15 --pathway R-MMU-1483206 --output inst/extdata/pathway_knowledge/views/mouse-glycerophospholipid-mapping

# スフィンゴ脂質代謝：化学式一致のRhea候補も追加
python scripts/pathway_mapping.py --catalog inst/extdata/pathway_knowledge/drafts/mouse-integrated-2026-10-15 --pathway R-MMU-428157 --candidates equation --output inst/extdata/pathway_knowledge/views/mouse-sphingolipid-mapping

# PCとPSを測定した例：未測定の中間クラスを介した最大2反応の接続
python scripts/pathway_mapping.py --catalog inst/extdata/pathway_knowledge/drafts/mouse-integrated-2026-10-15 --pathway R-MMU-1483206 --candidates all --measured-class GP0101 GP0301 --measured-mode bridge --bridge-depth 2 --output inst/extdata/pathway_knowledge/views/mouse-pc-ps-mapping

# 既存カタログすべての所属情報・選択条件を再生成
python scripts/pathway_mapping.py --prepare-all
```

`--pathway`と`--measured-class`は複数指定できます。測定クラスはLIPID MAPS分類コード
または正規クラスURIで指定します。`--profile all`で分類条件を解除し、
`--exclude-inferred`でsource-inferred反応を除外します。他種にも同じコマンドを使えますが、
パスウェイIDは対象種のPathwaysタブにあるIDを指定してください。

出力はselected-reactions.json / CSV、selected-class-pathways.ttl / JSON-LD、
選択条件と入力ハッシュを保存したselection.jsonです。コマンドのRDFには選択範囲の
所属根拠も保持します。GUIの選択RDFはクラス投影・出典・生物種・分類ノードを出力します。

## 保存形式

各カタログ内の追加ファイル：

* pathway-groups.ttl：永続IDを持つ所属根拠。候補と正式所属を別の値で記録。
* pathway-groups.csv：反応、パスウェイ、所属根拠、対応元の反応と対応根拠URI。
* mapping-reactions.csv：表示対象判定と除外理由。
* mapping-groups-report.json：入力ハッシュ、種、件数、フィルタ方針。

同じ情報をsearch.sqliteのpathway_groups / mapping_featuresにも収録します。
RDFの追加スキーマはontology/mslm-pathway-groups.ttlです。
原カタログのRDF・JSON・出典は書き換えません。コマンド抽出時は入力ハッシュを検証し、
所属情報が古い場合には再生成を要求します。GUIでカタログを更新した場合は所属情報も再生成し、
GUIを再起動してください。
