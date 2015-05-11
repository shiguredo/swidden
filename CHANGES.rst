#################
CHANGES
#################

1.x
===

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
