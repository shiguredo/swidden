# JSON Schema 検証を shiguredo_jesse から jsone の JSON Schema 実装へ移行する

- Created: 2026-09-16
- Completed: {YYYY-MM-DD}
- Branch: feature/change-replace-jesse-with-jsone
- Polished: {YYYY-MM-DD}

## 目的

JSON Schema 検証の依存を shiguredo_jesse から jsone の JSON Schema 実装 (`jsone_schema`) へ移行し、swidden が jesse なしで動作することを確認する。

jsone の `feature/add-json-schema` ブランチに draft 6 のバリデータ (`jsone_schema`) が追加された。移行できれば JSON 関連の依存が jsone ひとつにまとまり、swidden が jesse のメンテナンスを引きずらなくなる。

## 現状

- swidden は JSON Schema 検証に shiguredo_jesse (2025.1.0) を使っている
  - `rebar.config` の deps に `{jesse, "2025.1.0", {pkg, shiguredo_jesse}}` がある
  - `src/swidden_json_schema.erl` の `add_schema/2` が `jesse:add_schema/3` を、`validate/2` が `jesse:validate/3` を、いずれも `{parser_fun, parse_fun()}` 付きで呼ぶ
  - `src/swidden_api_handler.erl` の `validate_json/5` が `{error, {data_error, _}}` を MalformedJSON、`{error, {database_error, _Key, schema_not_found}}` を SchemaNotFound、それ以外を `swidden_json_schema:to_json/1` で組み立てた InvalidJSON として 400 で返す
- jsone の `feature/add-json-schema` ブランチに `jsone_schema` がある
  - `jsone_schema:add_schema/2,3` でスキーマを登録し、`jsone_schema:validate/2,3` / `jsone_schema:validate_key/2,3` で検証する
  - スキーマのパースは `jsone_schema:add_schema/3` が `jsone:decode/1` で行い、不正な場合は `{error, {parse_error, Reason}}` / `{error, {invalid_schema, Value}}` を返す
  - データはパース済みの map を前提とし、JSON のパースは呼び出し側で行う
  - 検証エラーは `{error, [jsone_schema_error:reason()]}` の map で返る
  - `minimum_otp_vsn` は 28.1。swidden は 29.0 のため問題ない
  - ブランチは未リリース (CHANGES.md 上は develop)
- swidden のスキーマ (`priv/swidden/schemas/<service>/<version>/<operation>.json`) はすべて draft-06 で、shiguredo_jesse も draft 6 のみの対応 (2023.4.0 の変更履歴) のため draft の後退はない
- 移行前の jesse には次の問題があった
  - `jesse_database:add/3` の `replace_schema_id/3` が格納時に `$id` へ `file://<プロセスの CWD>/<キー>` を書き込むため、400 応答の `error_reason` の `schema` にサーバの絶対パスが現れていた
  - JSON として不正な Body は `{error, [{data_error, {parse_error, _}}]}` で返るため `validate_json/5` の `{error, {data_error, _}}` 分岐が一致せず、MalformedJSON ではなく InvalidJSON になっていた
  - `load_schemas/2` が `jesse:add_schema/3` の戻り値を jesse 固有の形 (`[{[], [], []}]` / `[{_, _, {error, invalid_json, LineNumber}}]`) でパターンマッチしており、不正なスキーマファイルで case_clause になっていた

## 設計方針

- `rebar.config` の jsone を `feature/add-json-schema` の git 参照に差し替え、jesse 依存を削除する (`rebar.lock` も更新する)
- `src/swidden_json_schema.erl` は jesse 互換層をやめて `jsone_schema` を直接使う
  - スキーマの登録は `jsone_schema:add_schema/3`
  - 検証は `jsone:decode/1` してから `jsone_schema:validate_key/2`
  - エラー理由は jesse のタプルではなく `jsone_schema_error:reason()` の map を応答の形へ変換する
- `src/swidden_api_handler.erl` の 400 応答の分岐を `jsone_schema` の戻り値に合わせる
- JSON として不正な Body は MalformedJSON として返す (移行前は `validate_json/5` の分岐が一致せず InvalidJSON になっていた)
- 検証はモックやスタブを使わず、実際に HTTP サーバを起動する既存の eunit (`test/swidden_api_handler_tests.erl`) と `make ci` (xref / dialyzer / eunit + cover) で行う
- jsone の当該ブランチは未リリースのため、依存は git のコミット ref を固定する。リリース後に hex のバージョン指定へ切り替えるのは別途対応とする

## 完了条件

- `rebar.config` から jesse 依存が消え、jsone が `feature/add-json-schema` を指している
- `make ci` (xref / dialyzer / eunit + cover) が通る
- 移行前後で同じリクエストを送ったときの 400 応答の差分が、`$id` の消失と MalformedJSON 化だけで説明できる
- 不正なスキーマファイルを読み込んだときに case_clause で落ちず、ファイルパスと理由が分かるエラーで起動に失敗する
- `rebar.lock` が更新されている

## 動作確認結果

2026-09-16 に `feature/add-json-schema` (4f9d703) を使って確認した。

- `rebar.config` の jsone を差し替えて jesse を削除し、`swidden_json_schema.erl` と `swidden_api_handler.erl` を `jsone_schema` 直利用に変更した
  - xref / dialyzer / efmt-check / elint は警告なし
  - eunit は 9 tests, 0 failures (cover 83%)
  - jsone 側は EUnit 744 tests, 0 failures (JSON-Schema-Test-Suite の draft 6 全ケースを含む)、PropEr 15/15 properties passed
- 実 HTTP サーバに同じリクエストを送り、移行前後で 400 応答を比較した
  - 差分は次の 2 点だけで、それ以外の 9 ケースは `error_type` / `error` / `data` / `path` / `schema` まで一致した
    - `error_reason` の `schema` から `$id` が消えた (jesse が書き込んでいたサーバの絶対パスが漏れなくなった)
    - JSON として不正な Body の `error_type` が InvalidJSON から MalformedJSON に変わった (`validate_json/5` の分岐が意図どおり機能するようになった)
- 不正なスキーマファイルは、移行前は case_clause で起動に失敗していたが、`{FilePath, {parse_error, Reason}}` / `{FilePath, {invalid_schema, Value}}` で起動に失敗するようになった
- swidden が jesse 互換層を使わなくなったため、jsone 側の jesse 互換層は削除した (shiguredo/jsone#2)
- スキーマ間の `$ref` (キー形式) とローカル `$ref` の両方が解決できることを確認した
- 壊れたスキーマ 20 パターンを与えて挙動を確認した。`schema_invalid` / `wrong_type_items` / `invalid_dependency` / `wrong_multiple_of` などのスキーマのエラーとして返り、クラッシュしたのは正規表現が不正な `pattern` / `patternProperties` の 2 件だけだった
  - この 2 件は jsone_schema 側で `re:run/3` の例外を捕捉して `schema_invalid` を返すように修正した (移行前の jesse も同じくクラッシュしており、swidden は 500 を返していた。修正後は 400 で `invalid: schema` を返す)
- 次の項目は jesse と jsone_schema で挙動が一致することを実測で確認した
  - draft-03 / draft-04 の `$schema` はどちらも `schema_unsupported` で拒否する
  - 検証エラーはどちらも既定で 1 件までしか返さない
  - `format` はどちらも `wrong_format` を返す
- jsone の `rebar.config` の deps にある `eqwalizer_support` は、git 参照で依存している間だけ swidden の `rebar.lock` に流入する。公開済みの hex パッケージ (2025.1.0) の requirements は空のため、hex のバージョン指定に切り替われば流入しなくなる

## 残タスク

- jsone を先にリリースしてから本 issue の変更をマージする
  1. shiguredo/jsone#2 をマージする
  2. `## develop` をリリースの節にし、`src/jsone.app.src` の `vsn` を上げる
  3. タグを打って hex に公開する
  4. swidden の jsone 依存を git 参照から hex のバージョン指定に切り替える
  5. `feature/change-replace-jesse-with-jsone` の PR をマージする
