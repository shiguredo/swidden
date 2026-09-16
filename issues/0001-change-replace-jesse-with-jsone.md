# JSON Schema 検証を shiguredo_jesse から jsone の jesse 互換 API へ移行する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/change-replace-jesse-with-jsone
- Polished: {YYYY-MM-DD}

## 目的

JSON Schema 検証の依存を shiguredo_jesse から jsone の jesse 互換 API へ移行し、swidden が jesse なしで動作することを確認する。

jsone の `feature/add-json-schema` ブランチに、swidden が必要とする API だけを提供する jesse 互換層 (`jesse` / `jesse_error` / `jesse_database`) と draft 6 のバリデータ (`jsone_schema`) が追加された。jsone の CHANGES.md にも「swidden が必要とする jesse 互換 API を追加する」と明記されている。移行できれば JSON 関連の依存が jsone ひとつにまとまり、swidden が jesse のメンテナンスを引きずらなくなる。

## 現状

- swidden は JSON Schema 検証に shiguredo_jesse (2025.1.0) を使っている
  - `rebar.config` の deps に `{jesse, "2025.1.0", {pkg, shiguredo_jesse}}` がある
  - `src/swidden_json_schema.erl` の `add_schema/2` が `jesse:add_schema/3` を、`validate/2` が `jesse:validate/3` を、いずれも `{parser_fun, parse_fun()}` 付きで呼ぶ
  - `src/swidden_json_schema.erl` の `load_schemas/2` が `jesse:add_schema/3` の戻り値を jesse 固有の形 (`[{[], [], []}]` / `[{_, _, {error, invalid_json, LineNumber}}]`) でパターンマッチしている
  - `src/swidden_api_handler.erl` の `validate_json/5` が `{error, {data_error, _}}` を MalformedJSON、`{error, {database_error, _Key, schema_not_found}}` を SchemaNotFound、それ以外を `swidden_json_schema:to_json/1` で組み立てた InvalidJSON として 400 で返す
- jsone の `feature/add-json-schema` ブランチに jesse 互換 API がある
  - `jesse:add_schema/3` / `jesse:validate/3` と、型参照のための `jesse_error` / `jesse_database`
  - 検証本体は `jsone_schema` の draft 6 実装。データ表現は map のみで、スキーマは persistent_term に保存する
  - CLI と http/https によるスキーマ取得は提供しない
  - `shiguredo_jesse` と同時に依存させると `jesse` モジュールが衝突する
  - `minimum_otp_vsn` は 28.1。swidden は 29.0 のため問題ない
  - ブランチは未リリース (CHANGES.md 上は develop)
- swidden のスキーマ (`priv/swidden/schemas/<service>/<version>/<operation>.json`) はすべて draft-06 で、shiguredo_jesse も draft 6 のみの対応 (2023.4.0 の変更履歴) のため draft の後退はない
- 互換層の戻り値は jesse と完全には一致しない
  - `jesse:add_schema/3` のエラーは `{error, [{schema_error, {parse_error, Reason}}]}` で返るため、`load_schemas/2` の既存のパターンマッチと一致せず、そのままでは case_clause になる
  - `jesse:validate/3` のエラーは `{data_invalid, Schema, Error, Data, Path}` / `{data_error, {parse_error, _}}` の形で `swidden_json_schema:to_json/1` に合わせてある。実サーバ経由の比較では `$id` の有無以外は jesse と一致した (「動作確認結果」参照)

## 設計方針

- `rebar.config` の jsone を `feature/add-json-schema` の git 参照に差し替え、jesse 依存を削除する (`rebar.lock` も更新する)
- `src/swidden_json_schema.erl` を互換層に合わせて修正する。特に `load_schemas/2` の `jesse:add_schema/3` のエラー分岐は互換層の戻り値に合わせて整理し、不正なスキーマで case_clause にならないようにする
- `swidden_json_schema:to_json/1` と `swidden_api_handler.erl` の 400 応答は、移行前と同じ内容を維持することを基本とする。差分が出た場合は、swidden 側で吸収するか jsone 側の互換層を直すかを切り分けて判断する
- 検証はモックやスタブを使わず、実際に HTTP サーバを起動する既存の eunit (`test/swidden_api_handler_tests.erl`) と `make ci` (xref / dialyzer / eunit + cover) で行う
- jsone の当該ブランチは未リリースのため、依存は git 参照のままとする。リリース後に hex のバージョン指定へ切り替えるのは別途対応とする

## 完了条件

- `rebar.config` から jesse 依存が消え、jsone が `feature/add-json-schema` を指している
- `make ci` (xref / dialyzer / eunit + cover) が通る
- 移行前後で同じリクエストを送ったときの 400 応答 (MalformedJSON / SchemaNotFound / InvalidJSON) が `$id` を除いて一致する
- 不正なスキーマファイルを読み込んだときの `swidden_json_schema:load_schemas/2` の挙動が、互換層の戻り値に対して定義されている
- `rebar.lock` が更新されている

## 動作確認結果

2026-09-16 に `feature/add-json-schema` (0b0a146) を使って確認した。

- `rebar.config` の jsone を git 参照に差し替えて jesse を削除するだけでビルドとテストが通り、互換層のために必要だったソースコードの修正は `load_schemas/2` のエラー分岐のみだった
  - xref / dialyzer / efmt-check は警告なし
  - eunit は 9 tests, 0 failures (cover 83%)
  - jsone 側の `rebar3 as test eunit` は 747 tests, 0 failures (JSON-Schema-Test-Suite の draft 6 全ケースを含む)
- 実 HTTP サーバに同じリクエストを送り、移行前後で 400 応答を比較した
  - 10 ケース中 8 ケースは `error_type` / `error` / `data` / `path` / `schema` まで完全一致した
  - 2 ケースは `error_reason[].schema` の `$id` の有無のみ差分だった
    - jesse は `jesse_database:add/3` の `replace_schema_id/3` で格納時に `$id` へ `file://<プロセスの CWD>/<キー>` を書き込む
    - そのため移行前の応答にはサーバの CWD が絶対パスで現れており、環境依存かつ情報漏洩になっていた (jesse 側のバグではなく、URI でないキーを渡していた swidden 側の問題)
    - jsone の互換層は `$id` を注入しないため、この差分は許容する
- 不正なスキーマファイルを読み込んだときの `load_schemas/2` は、移行前後とも case_clause で起動に失敗していた (移行前からの潜在バグ)。互換層の戻り値に合わせて修正し、JSON として壊れている場合は `{error, {invalid_json, FileName, Reason}}`、JSON だがスキーマでない場合は `{error, {invalid_schema, FileName, Reason}}` で起動に失敗するようにした
- jsone の `rebar.config` の deps にある `eqwalizer_support` (whatsapp/eqwalizer の git_subdir) が swidden の `rebar.lock` に流入する。リリースされた jsone を依存させるときにも付いてくるため、jsone 側で profile に移すのが望ましい (swidden 側では対応しない)
