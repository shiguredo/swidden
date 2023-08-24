# CHANGES

## develop

- [CHANGE] jesse を時雨堂フォークの shiguredo_jesse に変更する
  - jesse 本家は OTP master で警告なしでビルドできなくなっているため
  - @sile

## 2023.2.1

- [FIX] swidden.app.src のライセンス表記を Hex 向けに修正する
  - @voluntas
- [FIX] Hex 公開用に gun のバージョンを 2.0.1 に固定する
  - @voluntas
## 2023.2.0

- [CHANGE] swidden_client を hackney から gun へ切り替える
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を shiguredo 26.0.2 (OpenSSL 3.1.1) に上げる
  - @voluntas
- [UPDATE] jsone を 1.8.0 にアップデートする
  - @voluntas
- [UPDATE] rebar3 を 3.22.1 にアップデートする
  - @voluntas
- [ADD] gun を追加する
  - ref 指定
  - @voluntas

## 2023.1.0

- [CHANGE] rebar3 の minimum_otp_vsn を 26.0 にする
  - @voluntas
- [UPDATE] rebar.conf の dialyzer の設定を変更する
  - TODO: もっと細かい設定を有効にしていく
  - @voluntas
- [UPDATE] rebar3 を 3.20.0 にアップデートする
  - @voluntas
- [UPDATE] jesse を 1.7.11 にアップデートする
  - @voluntas
- [UPDATE] hackney を 1.18.1 にアップデートする
  - @voluntas
- [UPDATE] cowboy を 2.10.0 にアップデートする
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 26.0-rc3 (OpenSSL 3.1.0) に上げる
  - @voluntas
- [UPDATE] GitHub Actions checkout を v3 に上げる
  - @voluntas

## 2022.12.0

- [UPDATE] jesse を 1.7.9 にアップデートする
  - @voluntas
- [CHANGE] rebar3 の minimum_otp_vsn を 24.2 にする
  - @voluntas
- [UPDATE] GitHub Actions の docker の OTP を 24.2 に上げる
  - @voluntas
- [ADD] slack 通知を secrets.SLACK_INTERNAL_CHANNEL に変更
  - @voluntas
- [FIX] swidden_interceptor の callback のタイポを修正する
  - @voluntas

## 2021.11.0

- [UPDATE] CI のイメージを OTP-24.1.7 にアップデートする
  - @voluntas
- [UPDATE] jesse を 1.6.1 にアップデートする
  - @voluntas
- [ADD] make publish コマンドを追加する
  - @voluntas

## 2021.10.0

- [CHANGE] loopback_address_only を削除
  - @voluntas
- [ADD] ip を追加
  - @voluntas

## 2021.9.0

- [ADD] app.src に licenses を追加する
  - @voluntas
- [ADD] app.src に links を追加する
  - @voluntas

## 2021.8.0

- [CHANGE] バージョン番号 を YYYY.RELEASE.FIX に統一する
  - @voluntas

## 2021.7

- [ADD] rebar3_plugin hex を追加する
- [CHANGE] hex package を利用するようにする
- [CHANGE] covertool 削除

## 2021.6

- [UPDATE] 依存を git@ から https に変更する
  - @voluntas
- [UPDATE] OTP 最小を 24.1 に上げる
  - @voluntas
- [UPDATE] CI のイメージを OTP-24.1.4 にアップデートする
  - @voluntas
- [UPDATE] jsone を 1.7.0 にアップデートする
  - @voluntas
- [UPDATE] hackney を 1.18.0 にアップデートする
  - @voluntas

## 2021.5

- [UPDATE] 依存を git: から git@ に変更する
  - @shino

## 2021.4

- [CHANGE] cowboy 2.0 で onresponse オプションが無くなったため削除する
  - @voluntas
- [ADD] interceptor オプションを追加する
  - dispatch 前に interceptor モジュールを呼び出し、JSON の変更、およびリクエストの中断を
    可能にする
    - @voluntas
  - dispatch 後に interceptor モジュールを呼び出し、JSON の変更を可能にする
    - @voluntas

## 2021.3

- [ADD] swidden:redirect/1 を追加
  - HTTP Status Code 307 でリダイレクトされる
- [UPDATE] rebar3 を 3.16.1 にアップデートする
- [UPDATE] jsone を 1.6.1 にアップデートする
- [UPDATE] CI のイメージを OTP-24.0.5 にアップデートする

## 2021.2

- [UPDATE] cowboy を 2.9.0 にアップデートする
- [UPDATE] CI のイメージを OTP-24.0 にアップデートする
- [UPDATE] jsone を 1.5.7 にアップデートする

## 2021.1

- [ADD] covertool を追加してみる
- [UPDATE] CI のイメージを OTP-24.0-rc1 にアップデートする
- [UPDATE] rebar3 の minimum_otp_vsn を 24.0 にする
- [UPDATE] jsone を 1.5.6 にアップデートする
- [UPDATE] rebar3 を 3.14.4 にアップデートする
- [UPDATE] hackney を 1.17.0 にアップデートする

## 2020.5.1

- [FIX] jsone を 1.5.5 にアップデートする
  - skip_undefined のバグ修正

## 2020.5

- [UPDATE] CI のイメージを OTP-23.1.4 にアップデートする
- [UPDATE] rebar3 を 3.14.2 にアップデートする
- [UPDATE] jsone を 1.5.4 にアップデートする
- [CHANGE] jsone:encode/2 に skip_undefined を指定する

## 2020.4

- [UPDATE] CI のイメージを OTP-23.1.2 にアップデートする
- [UPDATE] rebar3 を 3.14.1 にアップデートする
- [UPDATE] rebar3 の minimum_otp_vsn を 23.1 にする
- [UPDATE] jsone を 1.5.3 にアップデートする
- [UPDATE] jesse を 1.5.6 にアップデートする

## 2020.3.1

- [FIX] cowboy_req:read_body の more に対応する
- [FIX] body が 0 バイトに対応する

## 2020.3

- [UPDATE] CI のイメージを OTP-23.0.2 にアップデートする
- [UPDATE] rebar3 の minimum_otp_vsn を 23.0 にする
- [UPDATE] cowboy を 2.8.0 にアップデートする
- [UPDATE] jesse を 1.5.5 にアップデートする
- [UPDATE] hackney を 1.16.0 にアップデートする
- [FIX] hackney を prod deps へ移動する

## 2020.2

- [CHANGE] CircleCI をやめる
- [UPDATE] CI のイメージを OTP-23.0-rc2 にアップデートする
- [UPDATE] jesse を 1.5.4 にアップデートする

## 2020.1

**YYYY.RELEASE[.FIX] にバージョン番号表記を変更**

- [UPDATE] rebar3 の minimum_otp_vsn を 22.2 にする
- [UPDATE] CI のイメージを OTP-22.2.8 にアップデートする
- [UPDATE] jesse を "efe0dca" にアップデートする
  - OTP 23 対応がリリースされるまでの暫定対応
- [UPDATE] jsone を 1.5.2 にアップデートする

## 9.0.1

- [FIX] loopback_address_only が逆になっていたのを修正する

## 9.0.0

- [UPDATE] jsone を 1.5.1 にアップデートする
- [UPDATE] CI のイメージを OTP-22.1.7 にアップデートする
- [ADD] loopback_address_only オプションを追加する

## 8.5.0

- [UPDATE] rebar3 の minimum_otp_vsn を 22.1 にする
- [UPDATE] CI のイメージを OTP-22.1.3 にアップデートする
- [UPDATE] cowboy を 2.7.0 にアップデートする
- [UPDATE] hackney を 1.15.2 にアップデートする
- [UPDATE] rebar3 を 3.12.0 にアップデートする

## 8.4.0

- [UPDATE] OTP 22.0 に対応する
- [UPDATE] rebar3 を 3.11.1 にアップデートする
- [UPDATE] jsone を 1.5.0 にアップデートする

## 8.3.0

- [UPDATE] cowboy を 2.6.3 にアップデートする
- [UPDATE] rebar3 を 3.10.0 にアップデートする
- [UPDATE] hackney を 1.15.1 にアップデートする

## 8.2.0

- [UPDATE] cowboy を 2.6.1 にアップデートする
- [UPDATE] rebar3 を 3.7.5 にアップデートする

## 8.1.0

- [UPDATE] OTP 21.1 に対応する
- [UPDATE] jesse を 1.5.2 にアップデートする
- [UPDATE] jsone を 1.4.7 にアップデートする
- [UPDATE] cowboy を 2.5.0 にアップデートする
- [UPDATE] rebar3 を 3.6.2 にアップデートする

## 8.0.1

- [UPDATE] jesse を 1.5.1 にアップデートする
- [UPDATE] rebar3 を 3.6.1 にアップデートする

## 8.0.0

- [UPDATE] OTP 21.0 に対応する
- [UPDATE] jsone を 1.4.6 にアップデートする
- [UPDATE] rebar を 3.5.3 にアップデートする
- [UPDATE] cowboy を 2.4.0 にアップデートする
- [UPDATE] jesse を shiguredo の otp-21-compatibility に変更するする
  - 公式が正式バージョンリリースするまではこのブランチ指定で進める

## 7.3.0

- [UPDATE] cowboy 2.3.0 にアップデートする
- [UPDATE] hackney を 1.12.1 にアップデートする

## 7.2.0

- [UPDATE] cowboy 2.2.0 にアップデートする
- [UPDATE] hackney を 1.11.0 にアップデートする
- [UPDATE] jesse を 1.5.0 にアップデートする
- [UPDATE] rebar3 を rebar 3.5.0 にアップデートする

## 7.1.0

- [UPDATE] cowboy 2.2.0 にアップデートする
- [UPDATE] hackney を 1.10.1 にアップデートする
- [UPDATE] rebar3 を rebar 3.4.7 にアップデートする

## 7.0.0

- [UPDATE] cowboy 2.0.0 にアップデートする

## 6.4.1

- [UPDATE] cowboy 2.0.0-rc.3 にアップデートする
- [UPDATE] rebar3 を rebar 3.4.4 にアップデートする
- [FIX] string:join/2 を lists:join/2 に切り替える

## 6.4.0

- [UPDATE] hackney を 1.9.0 にアップデートする
- [UPDATE] jsone を 1.4.5 にアップデートする
- [UPDATE] cowboy 2.0.0-rc.2 にアップデートする

## 6.3.1

- [UPDATE] cowboy 2.0.0-rc.1 にアップデートする
- [UPDATE] rebar3 を rebar 3.4.2 にアップデートする

## 6.3.0

- [UPDATE] cowboy 2.0.0-pre.10 にアップデートする
- [UPDATE] jsone を 1.4.4 にアップデートする
- [UPDATE] hackney を 1.8.6 にアップデートする
- [FIX] 1.8.0 系で hackney:start/stop がなくなったので対応

## 6.2.2

- [FIX] rebar3 を OTP 19.3 でビルドしたものにする

## 6.2.1

- [UPDATE] hackney を 1.8.3 にアップデートする
- [UPDATE] cowboy 2.0.0-pre.9 にアップデートする
- [UPDATE] rebar3 を rebar 3.4.0 にアップデートする

## 6.2.0

- [UPDATE] jsone を 1.4.3 にアップデートする
- [UPDATE] cowboy 2.0.0-pre.8 にアップデートする

## 6.1.0

- [CHANGE] middleware を復帰させる

## 6.0.0

- [UPDATE] hackney を 1.7.1 にアップデートする
- [CHANGE] Opts を削除する

## 5.0.5

- [UPDATE] cowboy 2.0.0-pre.7 にアップデートする

## 5.0.4

- [UPDATE] jsone 1.4.2 にアップデートする

## 5.0.3

- [UPDATE] hackney 1.6.5 にアップデートする
- [UPDATE] jsone 1.4.1 にアップデートする

## 5.0.2

- [UPDATE] cowboy 2.0.0-pre.6 にアップデートする

## 5.0.1

- [UPDATE] cowboy 2.0.0-pre.5 にアップデートする

## 5.0.0

- [UPDATE] hackney 1.6.3 にアップデートする
- [UPDATE] cowboy 2.0.0-pre.4 にアップデートする

## 4.4.3

- [UPDATE] hackney 1.6.2 にアップデートする

## 4.4.2

- [UPDATE] jsone を 1.4.0 にアップデートする

## 4.4.1

- [UPDATE] jsone を 1.3.1 にアップデートする

## 4.4.0

- [UPDATE] jsone を 1.3.0 にアップデートする

## 4.3.0

- [FIX] エラー理由が error_reasons なっていたのを error_reason にする
- [ADD] テストクライアントの戻りに 403 を許可する

## 4.2.1

- [UPDATE] MissingTargetFunction 時の戻りに error_reason を返すようにする

## 4.2.0

- [ADD] middleware 用の swidden_middleware:failure/2,3 を追加する
- [ADD] ヘッダーを追加できるテスト用クライアント swidden_client:request_with_ehaders/6,7 を追加する
  - ヘッダーは [{binary(), any()}] で渡す

## 4.1.0

- [CHANGE] #{error_reason: #{}} を追加する

## 4.0.0

- [CHANGE] JSON は全て map で処理する仕組みを追加する

## 3.1.0

- [ADD] map を指定できる failure/2 を追加する
- [UPDATE] rebar3 を rebar 3.2.0 on Erlang/OTP 19 Erts 8.0.2 にアップデートする
- [UPDATE] hackney 1.6.1 に上げる

## 3.0.0

- [ADD] 利用できるサービスを swidden:start 時に [{services, [<<"Spam">>]}] のように指定できるようにする
- [CHANGE] start/0 が配信され start/1 で Port 番号を指定する必要がある

## 2.3.0

- [CHANGE] swidden_client の戻り JSON を Map 形式にする

## 2.2.2

- [FIX] JSON の型を明示的に jsone モジュールを使用し、外へ出す thx melpon

## 2.2.1

- [UPDATE] OTP 19 対応
- [UPDATE] cowlib をアップデートする

## 2.2.0

- [UPDATE] jsone を 1.2.3 にアップデートする
- [UPDATE] jesse を 1.4.0 にアップデートする
- [UPDATE] hackney を 1.6.0 にアップデートする

## 2.1.2

- [FIX] 壊れた JSON が来たときのパースエラー時のエラーメッセージを追加する

## 2.1.1

- [FIX] 不要なデバッグログを削除する

## 2.1.0

- [UPDATE] JSON Schema エラーじのエラーメッセージを JSON で返すようにする
- [UPDATE] JSON Schema が見つからない時 JSON で Serfvice, Version, Operation を返すようにする

## 2.0.2

- [UPDATE] rebar3 コマンドを整理する
- [FIX] eunit_formatters は rebar3 に内蔵されているので削除する

## 2.0.1

- [FIX] Hackney を test profile から移動する

## 2.0.0

- [UPDATE] rebar3 にアップグレードする
  - rebar 3.0.0 on Erlang/OTP 18 Erts 7.2.1

## 1.4.0

- [UPDATE] hackney を 1.5.0 に上げる
- [UPDATE] jesse を 1.3.0 に上げる
- [UPDATE] cowboy を 2.0.0-pre.3 に固定する

## 1.3.1

- [FIX] terminate 部分のパターンマッチを増やす

  - Reason を出力するようにする

## 1.3.0

- [UPDATE] 18.2.1 に対応する
- [UPDATE] cowboy を最新 (dbb636034f20736e16eb9d6c809217c9525b6cbd) にアップデート
- [UPDATE] hackney を 1.4.9 にアップデート
- [UPDATE] jesse のリポジトリを for-GET/jesse へ変更し 1.2.0 に更新

  - 既存がすでにメンテナンスされなくなった
  - このライブラリで draft 4 へ対応したので今後は draft 4 へ移行していきたい

## 1.2.1

- [UPDATE] 18.2 に対応する
- [UPDATE] cowboy を最新 (b7d666cfc746f55b0a72ef8d37f703885099daf7) にアップデート
- [UPDATE] hackney を 1.4.7 にアップデート
- [UPDATE] eunit_formatters を v0.3.1 にアップデート
- [UPDATE] rebar.config から erts のバージョン固定を外す

## 1.2.0

- [ADD] Function の引数を拡張し、middleware から handler_opts で引き回したデータを Opts として扱えるようにする
- [ADD] swidden:start/2 で Opts として onresponse hook を受け取れるようにする

## 1.1.12

- [UPDATE] hackney を 1.4.4 にアップデート

## 1.1.11

- [UPDATE] rebar を 2.6.0 18 にアップデート

## 1.1.10

- [UPDATE] hackney を 1.3.2 にアップデート
- [UPDATE] jsone を 1.2.1 にアップデート
- [UPDATE] JSON Schema 読み込み時にエラーが起きた場合ファイルパスを表示する
- [UPDATE] Erlang 18.1 に対応する

## 1.1.9

- [UPDATE] jsone を 1.0.1 にアップデート

## 1.1.8

- [UPDATE] cowboy を最新版(https://github.com/ninenines/cowboy/commit/e25634cd9db82a4760087a2ba68d4c6a76353d66) にアップデート
- [UPDATE] hackney を 1.2.0 にアップデート

## 1.1.7

- [UPDATE] cowboy を最新版(https://github.com/ninenines/cowboy/commit/341f991d58fde702f68fa9d0076ad6dc2f942917) にアップデート
- [CHANGE] OTP のバージョンを 18.0 に固定する
- [CHANGE] rebar_swidden_plugin を一旦削除する

## 1.1.6

- [FIX] Body を空で返すとき <<>> ではなく [] で戻すようにする

## 1.1.5

- [ADD] 存在しないモジュールの場合は 400 で MissingTargetModule を戻すようにする
- [UPDATE] 存在しない関数の場合は 400 で MissingTargetFunction を戻すようにする
- [UPDATE] cowboy を 271869889587085494baaedc6b44e939252637f0 にアップデート
- [UPDATE] erlydtl を de00ccf522be8d3f9b0dcb7cd680f83b4fb7267a にアップデート

## 1.1.4

- [UPDATE] jsone を v0.3.3 にアップデート
- [ADD] 存在しない関数の場合は 400 で MissingTargetArgs を戻すようにする

## 1.1.3

- [UPDATE] jsone を v0.3.1 にアップデート

## 1.1.2

- [UPDATE] jsx から jsone に変更

## 1.1.1

- [UPDATE] jsone から jsx に変更

## 1.1.0

- [UPDATE] jsonx から jsone に変更
- [UPDATE] list\_\* 系などの JSON が送られてこない場合の JSON Schema は空にする仕組みに変更

## 1.0.1

- [BUG] lager の parse_transform が残っていたのを削除
