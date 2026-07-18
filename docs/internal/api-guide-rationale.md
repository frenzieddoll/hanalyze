# api-guide の設計根拠 / roadmap 退避 (内部メモ)

公開 API リファレンス ([../api-guide/](../api-guide/README.md)) から外した**将来計画 (roadmap)**
と**設計判断の根拠**をここに退避する。読者向けの「現状の挙動」は公開 doc に、なぜそうなって
いるか / 今後どうするかはここに置く (公開 carve-out 対象外)。

plot 側の同種メモは [`hgg/docs/internal/api-guide-rationale.md`](../../../hgg/docs/internal/api-guide-rationale.md)。

## DOE (09-doe) から退避した roadmap

公開 doc には「現状の挙動」だけを残し、以下の将来計画はここへ移した (2026-07-04・Phase 78)。

### 一部実施要因 (`fractionalDesign`) の formula
- **現状 (公開)**: v1 の formula は主効果のみ。
- **なぜ**: 一部実施では 2 因子交互作用が主効果や他の交互作用と交絡するため、v1 では
  交互作用項を formula に含めない。
- **今後**: MVP が固まってから、交絡構造 (alias) を踏まえた 2 因子交互作用 formula を
  足す (Phase 78 G-c)。

### factorial / 直交表の水準数・カテゴリ因子
- **現状 (公開・Phase 78.G-b2 済)**: 因子は `contFactor` (連続 2 水準) / `catFactor` (カテゴリ m 水準)。
  `DesignFactor = { dfName, dfKind }` / `FactorKind = Cont Double Double | Cat [Text]` の**因子レベル和型**
  (要素レベルでなく因子を和型化し混在不正状態を型排除・識別子と性質を分離)。 factorial はカテゴリ m 水準を
  総当り。 `fractionalDesign` は **binary カテゴリのみ** (coded ±1 に写す)。 RSM / Box-Behnken / Taguchi は連続専用。
- **今後 (G-a2)**: 3 水準/カテゴリ Taguchi (L9 / L18 / L27)・混合水準 OA。 dsCoded の水準 index 表現は流用可。

### 最適計画 (`optimalDesign`) の因子型
- **現状 (公開・Phase 78.G-b2 済)**: 連続 + カテゴリ因子。 カテゴリは候補格子で全水準を展開し、
  `designMatrixF` の contrast 経路で設計行列に載せて最適点を選ぶ。

### R-formula の character→factor 自動判定 (Phase 78.G-b2 の派生・formula engine 改修)
- **背景**: DOE カテゴリ fit の実装中、`designModel` の R-formula `y ~ temp * cat` が `cat` (Text 列) を
  **連続変数として探し error** になることが判明 (`Frame.hs` の factor 判定が「`!` 添字の使われ方」限定で、
  R 本来の「character 列は factor」意味論を持っていなかった)。
- **判断**: DOE 内で `designFormula` を native factor 構文に書き換える対症療法 (A) でなく、**engine の穴を塞ぐ (B)**
  を採用。 `buildFrame` で「`!` 無しの非数値 (Text) 列」を factor に含め、`classify` で裸 factor 項を主効果
  (`LFactor [(x,Treatment)]`) に落とす。 変更は「従来 error だった経路」だけを成功に変える**単調変更**
  (数値列は連続・`!` 添字は従来どおり) ゆえ formula engine 76 test・本体 1249 test 無回帰で確認。
  これで DOE に限らず R 形式 `y ~ x + group` (group が Text) が全 fit で通る。

### 高レベル DOE モデルの当てはめ
- **現状 (公開)**: v1 のモデルは LM (一般線型)。
- **今後**: GP / RFF・HBM 化 (ベイズ DOE / ガウス過程応答曲面)。
