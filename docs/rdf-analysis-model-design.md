# MS-LipidMapper RDF解析モデル設計案

状態: 設計ドラフト。現行出力の仕様ではなく、次期モデルの提案。

## 目的と境界

mzTab-M由来の定量情報を、分子・クラス・アシル鎖サブセットの粒度で保持し、
ユーザー指定の比較結果から代謝反応・タンパク質の知識まで、根拠を追跡可能にする。
測定RDFの生成時には群間比較を実行しない。すべての定量値・統計結果をRDFで表現する。
性能用の索引や派生テーブルは許すが、独立した意味定義を持たせない。

JSON-LD 1.1を既存アプリとの交換に使い、名前付きグラフを含む交換にはTriGまたは
JSON-LDの名前付きグラフ構造を使う。保存エンジンとLLM向け要約形式は別の関心事とする。

## 設計原則

1. 測定フィーチャーと化学物質を同一視しない。
2. 生物学的な集合の定義、研究内での集合メンバー、集約処理を分離する。
3. 定量値のIDにパスウェイや表示ノードを含めない。
4. 元値、正規化値、変換値、集約値を上書きせず、値の系列を区別する。
5. 観測、注釈、対応付け、統計結果、反応活性の仮説を区別する。
6. 未測定・欠測・不明・統計計算不能を、ゼロや非有意に置換しない。
7. 解析は不変の入力スナップショットと設定に対して実行する。
8. 表示名ではなくURIで結合し、出典と知識リリースを保持する。

## リソースと責務

以下のクラス・プロパティ名は候補。既存語彙との衝突確認後に確定する。

| リソース | 意味と主な関係 |
|---|---|
| Study | 公共研究・ローカル研究。レポジトリアクセッションと出典 |
| DatasetSnapshot | 入力ファイル群・メタデータ・注釈の不変な版。Studyに所属 |
| SourceArtifact | 元ファイルの識別子、ハッシュ、媒体型、取得情報 |
| BiologicalSample | 生物学的サンプル。生物種・組織・処置など |
| Assay | 測定単位。サンプル、run、測定モード、技術反復との対応 |
| MolecularFeature | データセット内の測定行。元行ID、m/z、RTなど |
| LipidEntity | 化学的な脂質の記述。構造粒度と外部ID |
| LipidAnnotation | FeatureからEntityへの注釈。候補、方法、根拠、確度 |
| LipidScopeDefinition | クラスや鎖条件などの再利用可能な対象定義 |
| ScopeMaterialization | 特定スナップショットで評価した集合と構成Feature |
| MembershipAssessment | 集合への所属判定。included / excluded / indeterminate |
| QuantitationSeries | 定量値系列。尺度、単位、正規化・変換、対象の種類 |
| QuantitativeObservation | Assay × 対象 × Seriesの一つの定量値と状態 |
| ProcessingActivity | 正規化・集約・変換などの処理と入力・出力 |
| SampleSet | 選択条件と、確定した生物学的サンプルのメンバー |
| Contrast | numerator / denominatorを持つ比較定義 |
| AnalysisRun | Contrast、入力Series、対象群、統計仕様、ソフトウェア版 |
| StatisticalResult | Run × 対象の結果、使用観測値、各群の有効n |
| Statistic | FC、log2FC、p値、補正p値などの値・種別・計算状態 |
| AdjustmentFamily | 補正手法、実際に補正対象となった検定集合 |
| KnowledgeMapping | 測定対象から知識エンティティへの対応、関係・根拠・版 |
| KnowledgeSnapshot | 反応・タンパク質などの知識リリースと出典 |

LipidScopeDefinitionは測定範囲を定義し、化学的なクラス概念とは別物にする。
「この研究で観測されたPCの合計」は、生体内の全PC量と同義ではない。

## 定量と構造のモデル

基本経路:

```text
QuantitativeObservation → Assay → BiologicalSample
                       → measuredTarget → MolecularFeature または ScopeMaterialization
                       → QuantitationSeries → ProcessingActivity

MolecularFeature → LipidAnnotation → LipidEntity
ScopeMaterialization → LipidScopeDefinition
                     → memberFeature → MolecularFeature
                     → DatasetSnapshot
```

- Featureには候補注釈を複数許す。同じ脂質名の行も、異なるFeatureなら別IDを維持する。
- LipidEntityの構造粒度をclass / sum composition / molecular species / sn-position等で明示する。
- クラスコードと個別構造アクセッションを別プロパティにする。自動的なowl:sameAsは禁止する。
- 鎖は文字列だけでなく、炭素数、不飽和度、結合種別、酸素情報、位置既知性を表せる構造にする。
- アシル鎖、アルキル鎖、アルケニル鎖、長鎖塩基を区別する。「18:0」だけでは結合種別は決まらない。
- PC 36:2から18:0含有を推定しない。候補間で鎖条件の真偽が分かれる場合もindeterminate。
- クラス集約は、曖昧な鎖注釈を含むFeatureでもクラス所属が確定していれば可能とする。
- Feature重複、アダクト、異性体の扱いを集約方針に記録する。重複排除を暗黙に行わない。

## 集合の定義と集約

例: 「PCかつアシル鎖18:0を少なくとも1本持つ」をScopeDefinitionとする。
鎖の本数条件、any / all、結合種別、必要な同定解像度を機械可読で保存する。
表示用ラベルだけを条件の正本にしない。

Materializationには入力スナップショット、注釈版、評価ルール版、構成Featureを保存する。
集合は重複を許す。18:0集合と18:1集合の値を足してクラス全体にはしない。
一つのFeatureが同じ鎖を2本持っていても、分子量の集合では一度だけ集約する。
鎖の本数で重み付けした量を作る場合は、別の処理・定量対象として定義する。

集約はサンプルごとの群間比較より前に、Assay単位で行う。
入力単位・尺度の互換性を検証し、sum / mean / median、欠測方針、期待Feature数と
有効Feature数を保持する。異なる尺度や測定モードを暗黙に合算しない。
技術反復から生物学的サンプルへの集約は別のProcessingActivityとする。

## 値と欠測

Availableの場合だけ有限な数値を一つ持つ。ゼロは有効な数値。
Missingの場合は数値トリプルを持たず、可能なら理由を保持する。
NotMeasuredは対象が測定範囲にない状態。BelowDetectionLimitは根拠がある場合のみ使い、
ゼロやMissingから推定しない。空集合と全欠測集合は区別する。

Seriesにはlinear / log2 / log10などの尺度、単位、正規化方法、処理順を持たせる。
不明は明示し、変換や比計算に必要な情報が不足する場合は解析を停止または
計算不能として記録する。正規化の記述だけで実処理済みとはみなさない。
尺度・単位は自由記述を残しつつ、既知の場合は管理語彙のURIで表す。

## サンプル条件と比較

任意の元メタデータはJSONリテラルとして保持し、検索に使う生物種、組織、疾患、処置、
時点、用量などは明示的なRDFにも昇格する。自動で意味付けした項目は対応根拠を保持する。
数量条件には値と単位を持たせ、未知のラベルを無理にオントロジーへ対応付けない。

SampleSetには選択条件と確定メンバーの両方を保存する。条件だけから過去の比較を再評価しない。
Contrastではnumeratorとdenominatorを必須とし、群名のソート順で方向を決めない。
対応あり比較ではペアID、共変量を使う解析ではモデル式と値のスナップショットを必要とする。
最初の実装は研究内・独立2群に限定してよいが、未対応のデザインを黙って独立検定しない。

## 統計結果

```text
StatisticalResult → AnalysisRun → Contrast → SampleSet
                  → target → MolecularFeature / ScopeMaterialization
                  → usedObservation → QuantitativeObservation
                  → Statistic（FC / log2FC / pValue / adjustedPValue）
```

Statisticを独立させ、FCは計算可能だが検定は不能、といった状態を表せるようにする。
Statisticには種別、計算状態、値、不能理由を持たせ、SPARQL用の種別URIを固定する。
FC・log2FCの計算式、平均方法、入力尺度、疑似カウントとその尺度をAnalysisRunに記録する。
log値に対して再度比を取らない。分母ゼロや負値などの扱いも仕様化する。

p値には検定、片側／両側、対応の有無を記録する。p値がないことをp=1に置換しない。
補正p値はAdjustmentFamilyを参照する。Familyには対象結果ID、補正対象数、方法、
計算不能検定の除外方針を記録する。フィルタ後の表示件数を補正対象数にしない。
同一Featureが複数Scopeに含まれることは、独立した測定の追加を意味しない。

各結果には群ごとの有効な生物学的nを持たせる。技術反復を生物学的nとして数えない。
「変化なし」は非有意と同義にしない。等価性の主張は専用の手法と閾値を必要とする。
統計的変動から酵素活性・フラックスを直接断定するトリプルは生成しない。

## 反応・タンパク質との接続

KnowledgeMappingを独立した根拠付きアサーションにする。
sourceTarget、knowledgeEntity、mappingRelation、resolution、method、evidence、
KnowledgeSnapshotを持たせる。未対応を許す。

- class correspondence: 測定クラスと知識クラスの対応。
- compatible participant: 構造解像度の範囲で反応参加物と整合する対応。
- exact identity: 同一性を裏付ける根拠がある対応。

これらを同一関係へ潰さず、検索で許容する対応範囲を指定する。
サブセットがPCクラスに属することから「18:0を保持する反応」を推定しない。
反応側は参加物の役割・係数・区画・方向・生物種・酵素注釈・出典を保持する。
タンパク質と遺伝子を区別し、アクセッションは可能な範囲でURI参照にする。
その生物種の酵素注釈は、測定組織での発現や活性の証明ではない。
パスウェイ表示ノードは知識エンティティへの投影としてのみ接続する。

## 識別子・版・グラフ

現行の短い固定アクセッションと台帳を利用する。ラベルやローカル行名をグローバルIDにしない。
独立環境の台帳を併合する際の衝突検知・名前空間方針を実装前に確認する。
同じ試料、同じ分子名、同じファイル名という理由だけで研究間のリソースを統合しない。

Snapshot、Series、Materialization、AnalysisRun、Mappingは意味が変われば新IDにする。
解析の再利用キーには入力スナップショット、使用値、対象集合、サンプル集合、全設定、
ソフトウェア・ルール版を含める。実行イベントのIDと再利用キーは分ける。
旧結果は上書きせず、置換・失効の関係を記録する。

名前付きグラフを、測定スナップショット、知識リリース、対応付け版、解析実行ごとに分離する。
グラフURIに出典・版を付けるメタデータグラフを設ける。暗黙の全グラフunionに依存せず、
検索サービスは使用するグラフ集合を明示する。

## 語彙と検証

RDF/RDFS/XSDを基礎とし、履歴にはPROV-O、出典・識別情報にはDublin Core Terms等の
採用を検討する。外部語彙への対応は公式仕様確認後に確定し、名前が似ているだけで再利用しない。
独自語彙はMS-LipidMapper固有のScope・対応解像度・統計契約に集中する。
語彙版、JSON-LD context版、データスキーマ版、知識版、ソフトウェア版を分ける。

JSON Schemaだけでなく、RDF展開後のSHACLを検証の中心にする。
SHACLで表せない算術・集合整合性は検証コードで補う。

必須検証:

- Available値は有限な数値が一つ、Missing値には数値がない。
- AssayとSampleの対応、対象とSnapshotの整合、Series内の観測キーの一意性。
- Scope構成Featureの入力版が一致し、indeterminateが暗黙にincludedにならない。
- 集約値と入力観測値・有効数の一致。全欠測と空集合の区別。
- Contrastの方向、サンプル集合の非空・重複条件、技術反復の扱い。
- p値・補正p値の範囲、Statisticの状態と値の整合、補正Familyの実メンバー。
- 反応Mappingの生物種・知識版・対応解像度・根拠。
- JSON-LDとTriGのラウンドトリップでRDFデータセットが同型。

## 検索による受入条件

固定データと期待結果を持つSPARQLテストを用意する。

1. パスウェイに存在しないTGでも、分子・クラス・サンプル別値を取得できる。
2. PC全体と18:0含有PCの値、構成Feature、集約根拠を別々に取得できる。
3. PC 36:2が18:0集合に誤って含まれず、判定不能理由を取得できる。
4. 元値と正規化値を混同せず、使用Seriesを指定して取得できる。
5. 指定した同一Contrast・Run内でPC増加かつPE減少を検索できる。
6. 結果から使用サンプル、入力値、補正Familyまで追跡できる。
7. 結果から許容したMapping種別を通じて反応・タンパク質・出典を取得できる。
8. 別Run、別研究、別生物種、別知識版が意図せず混ざらない。

LLMには、これらを基にした検証済み検索テンプレートと引数契約を公開する。
検索結果の要約にも結果・対象・Run・出典のURIを残す。

## 現行からの移行

| 現行 | 次期モデルでの変更 |
|---|---|
| Dataset | Study / DatasetSnapshot / Seriesの責務を分離 |
| Sample | BiologicalSampleとAssayを分離。追加条件をRDF化 |
| MolecularFeatureに脂質名・鎖 | Featureを維持し、注釈とLipidEntityを追加 |
| LipidMeasurementScopeがonPathwayNode必須 | ScopeDefinition / Materializationへ分離し、パスウェイ依存を除去 |
| SampleMeasurement | SeriesとAssayを明示したObservationへ対応 |
| canonicalEntity | 根拠・粒度付きKnowledgeMappingへ移行 |
| 独立した全ペア統計RDF | 同じ定量対象・観測値を参照するRun / Resultへ統合 |
| sampleMetadata JSON | 元情報として保持し、検索条件をRDFへ昇格 |

既存IDを新しい意味へ流用しない。旧出力は保持し、移行時には旧新対応表を出す。
旧データにない情報を捏造しない。Assay対応などが不明なら明示し、解析可能範囲を制限する。
現行JSON Schemaはタイトルv4とDataset制約2.0.0が一致していないため、次期実装で整合させる。

## 実装順序と未確定事項

1. 本設計のクラス・関係・ID契約を具体的なTTLとSHACLへ落とす。
2. 小さな架空データで、定量・集合・欠測・曖昧注釈・知識Mappingの参照例を作る。
3. SPARQL受入テストを実行し、検索が複雑すぎる部分を見直す。
4. パスウェイ非依存の定量エクスポーターを実装する。
5. RDFからの指定2群比較と統計結果出力を実装する。
6. 既存反応カタログとの統合検索、LLM向けツール契約を追加する。

実装前に決める事項: 既存Assay情報の回収可能範囲、曖昧注釈の取り込み規則、
アダクト重複の扱い、初期対応の定量尺度・検定、最初のトリプルストア、
独立台帳の併合規則。自動推測を既定にしない。
