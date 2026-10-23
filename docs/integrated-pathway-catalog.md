# 生物種別のRhea / Reactome統合カタログ

マウス（NCBI Taxonomy 10090）について、出典を保持したRDFの統合と横断検索を実装しています。
対象はRhea/UniProt由来の`mouse-lipid-census-2026-10-15`と、Reactome由来の
`reactome-10090-all-species-first`です。別の生物種のデータを混ぜる入力は拒否します。

## 今回のデータ

|項目|件数|
|---|---:|
|Rhea由来の反応レコード|2,950|
|Reactome由来のイベント|711|
|統合検索の出典別レコード合計|3,661|
|LIPID MAPS分類コード|117|
|生化学的クラスノード|119|
|Reactomeパスウェイ|117|
|完全な化学反応式が一致する対応|185組|
|脂質クラスの関係だけが一致する対応|864組|
|同一反応として確定した対応|0組|

3,661件は重複排除済みの生化学反応数ではありません。119ノードと117コードの違いは、
同じ分類コードでもPE/PMME/PDMEなどの区別を保持するためです。
RheaとReactomeの元データセット、反応、参加分子、出典、ライセンス記述、
UniProtのreviewed状態、Reactomeの推定フラグを保持します。
今回のマウスReactomeイベント711件はすべて種間推定です。

## 対応付けのルール

`cross-source-matches.json`とRDFの`CrossSourceReactionMatch`に根拠を記録します。

* `verified-equivalence`：指定生物種のReactome反応への出典の明示的参照があり、
  全参加物のChEBI ID・係数・式の両辺も一致するもの。共通の`VerifiedReactionGroup`を作ります。
* `exact-chemical-equation`：全参加物のChEBI ID・係数・両辺が一致するもの。
  出典の明示的対応が不足するため、同一反応とは確定しません。
* `source-cross-reference`：同じ生物種のReactome反応への明示的参照だけがあるもの。
* `class-signature`：両辺の正規クラスIDの集合が一致するもの。反応の同一性を表しません。

対応は優先度順で1組につき1行とし、化学式一致・出典参照・クラス一致の各フラグも残します。
式の向きは両方向を比較しますが、生理的な反応方向やフラックスを推定しません。
クラス一致の候補は完全に対応済みで、両辺が非空かつ異なるクラス集合の場合に限定します。
係数や補因子を省略した一致は完全な化学式一致に含めません。
集合・複合体・未知係数・輸送・複数化学参照のRhea参加物は、化学式一致の判定から外します。

RheaのReactome参照にはヒトのIDが含まれます。ヒトの`R-HSA-...`をマウスの
`R-MMU-...`へ文字列置換して対応付ける処理はありません。このため、今回のマウスでは
同一反応の確定件数は0件です。`owl:sameAs`も付与しません。

## 再構築

リポジトリのルートで実行します。ネットワークやLLMは使用せず、既存カタログから構築します。

```powershell
python scripts/integrate_pathway_catalogs.py --rhea inst/extdata/pathway_knowledge/drafts/mouse-lipid-census-2026-10-15 --reactome inst/extdata/pathway_knowledge/drafts/reactome-10090-all-species-first --output inst/extdata/pathway_knowledge/drafts/mouse-integrated-2026-10-15
```

他種の同形式のRheaカタログを用意すれば、同じコマンドで同種のReactomeカタログと統合できます。
今回の作業では、Rheaの他種データ取得は実装・実行していません。
入力ファイルのハッシュは`report.json`に記録し、入力間の分類IDの競合は停止します。
内部の新規データセット・対応根拠・確定グループには共有ID台帳から短い永続IDを割り当てます。

出力先には、元RDFを保持した`candidates.ttl`と等価な`pathways.jsonld`、
クラス投影の`class-pathways.ttl`、JSON/CSV/SQLiteの検索用データ、出典間対応表を生成します。
SQLiteの`source_records`と`cross_source_matches`で出典・対応種別を検索できます。

## GUI

```r
MSLipidMapper::run_pathway_database_app()
```

`mouse-integrated-2026-10-15 ... [integrated / source evidence]`を選びます。
統合カタログがある場合は起動時に優先選択します。

* SourceのAll / Rhea / Reactomeで出典を切り替えます。
* PCやGP0101から、両データベースの反応を横断検索できます。
* 出典IDでも検索でき、対応する他方のレコードも検索対象になります。
* Cross-source matchesタブで化学式一致・クラス関係一致を確認できます。
* 反応を選ぶと、元の参加分子・生物種の根拠と出典間対応を閲覧できます。

既存の出典別データセットも引き続き選択できます。実行中のGUIにはコード変更が反映されないため、
更新後は再起動してください。
