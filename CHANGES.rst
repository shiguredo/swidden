#################
CHANGES
#################

1.x
===

1.2.0
-----

- [ADD] Function の引数を拡張し、middleware から handler_opts で引き回したデータを Opts として扱えるようにする
- [ADD] swidden:start/2 で Opts として onresponse hook を受け取れるようにする


1.1.12
------

- [UPDATE] hackney を 1.4.4 にアップデート


1.1.11
------

- [UPDATE] rebar を 2.6.0 18 にアップデート

1.1.10
------

- [UPDATE] hackney を 1.3.2 にアップデート
- [UPDATE] jsone を 1.2.1 にアップデート
- [UPDATE] JSON Schema 読み込み時にエラーが起きた場合ファイルパスを表示する
- [UPDATE] Erlang 18.1 に対応する

1.1.9
-----

- [UPDATE] jsone を 1.0.1 にアップデート

1.1.8
-----

- [UPDATE] cowboy を最新版(https://github.com/ninenines/cowboy/commit/e25634cd9db82a4760087a2ba68d4c6a76353d66) にアップデート
- [UPDATE] hackney を 1.2.0 にアップデート

1.1.7
-----

- [UPDATE] cowboy を最新版(https://github.com/ninenines/cowboy/commit/341f991d58fde702f68fa9d0076ad6dc2f942917) にアップデート
- [CHANGE] OTP のバージョンを 18.0 に固定する
- [CHANGE] rebar_swidden_plugin を一旦削除する

1.1.6
-----

- [FIX] Body を空で返すとき <<>> ではなく [] で戻すようにする

1.1.5
-----

- [ADD] 存在しないモジュールの場合は 400 で MissingTargetModule を戻すようにする
- [UPDATE] 存在しない関数の場合は 400 で MissingTargetFunction を戻すようにする
- [UPDATE] cowboy を 271869889587085494baaedc6b44e939252637f0 にアップデート
- [UPDATE] erlydtl を de00ccf522be8d3f9b0dcb7cd680f83b4fb7267a にアップデート

1.1.4
-----

- [UPDATE] jsone を v0.3.3 にアップデート
- [ADD] 存在しない関数の場合は 400 で MissingTargetArgs を戻すようにする

1.1.3
-----

- [UPDATE] jsone を v0.3.1 にアップデート

1.1.2
-----

- [UPDATE] jsx から jsone に変更

1.1.1
-----

- [UPDATE] jsone から jsx に変更

1.1.0
-----

- [UPDATE] jsonx から jsone に変更
- [UPDATE] list_* 系などの JSON が送られてこない場合の JSON Schema は空にする仕組みに変更

1.0.1
-----

- [BUG] lager の parse_transform が残っていたのを削除
