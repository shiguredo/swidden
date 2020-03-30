# ヘッダーベース HTTP API フレームワーク

## 概要

AWS DynamoDB や Kinesis などの API の形式に影響を受けた HTTP API ライブラリです。

[DynamoDB や Route53 などの AWS API が独特な仕様なので紹介](https://gist.github.com/voluntas/811240c5b6a169ae1c6ac401e0197417)

特徴は以下の通りの部分です

- 指定したヘッダー名の値を使いディスパッチする
    - デフォルトのヘッダー名は X-Swd-Target
    - このヘッダー名は好きに変更できる
- URI は / のみ
- メソッドは POST のみ
- ヘッダー値は *サービス_バージョン.オペレーション* という形式
    - X-Swd-Target: Spam_20141101.CreateUser
    - X-Swd-Target: SpamAdmin_20141101.GetMetrics
- 入り口と出口が JSON
- 何も送らなければ Body は空になる

## サンプル

ユーザ追加 API 例

```
$ http POST 127.0.0.1:5000/ "x-spam-target:Spam_20141101.CreateUser" username=yakihata password=nogyo -vvv
POST / HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 45
Content-Type: application/json; charset=utf-8
Host: 127.0.0.1:5000
User-Agent: HTTPie/0.8.0
x-spam-target: Spam_20141101.CreateUser

{
    "password": "nogyo",
    "username": "yakihata"
}

HTTP/1.1 200 OK
connection: keep-alive
content-length: 2
content-type: application/json
date: Sun, 02 Nov 2014 18:53:09 GMT
server: Cowboy
```

ユーザ取得 API 例

```
$ http POST 127.0.0.1:5000/ "x-spam-target:Spam_20141101.GetUser" username=yakihata -vvv
POST / HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 24
Content-Type: application/json; charset=utf-8
Host: 127.0.0.1:5000
User-Agent: HTTPie/0.8.0
x-spam-target: Spam_20141101.GetUser

{
    "username": "yakihata"
}

HTTP/1.1 200 OK
connection: keep-alive
content-length: 20
content-type: application/json
date: Sun, 02 Nov 2014 18:53:33 GMT
server: Cowboy

{
    "password": "nogyo"
}
```

## 目的

ネットワークサーバの組込用 HTTP API を想定して作られています。そのため、ブラウザには優しい仕様になっていません。

ただ、ヘッダー + JSON なので、JS で操作するのは難しくないのかもしれません。

いくつもネットワークサーバを作っていると API の仕組みを統一したくなってきたため作りました。

- 組込が簡単なこと
- 使い方が同じなこと
- データベースとの連携が簡単に行えること
- バージョンにより API が気軽に増やせること

あたりを意識して作っています。といってもぱくりですが。

## 細かい話

- ヘッダー名は自分で指定可能
    - 判定ヘッダーのデフォルトは X-Swd-Target となる
    - name は好きにして良い amz が源流、自社名の省略が良いか X-Abc-Def とかでもよい
        - X- は付けた方がいい
    - 先頭大文字 PascalCase がいいかも
    - ただHTTP ヘッダーは大文字小文字無視するのでどうでもいい
- ヘッダー値は Service_Version.Operation という構成
    - Service 名は先頭大文字 DynamoDB や OpsWorks など DataPipeline ように PascalCase で
    - Version は 20120810 で YYYYMMDD に
    - Operation 名も PascalCase で CreateTrail や ActivatePipline など **動詞** を使う
        - Add / Create / Delete / Describe / List / Get / Merge / Put / Remove / Split など
        - リスト表示の時は ListUsers と複数形にする
- ユーザレイヤーのエラーは 400 のみで 4xx や 5xx はすべて swidden が生成する
- priv/swidden/dispatch.conf にルーティングを記述する
    - dispatch.conf は Erlang Term で記述
- JSON  Schema は priv/swidden/schmeas/:service/:version/:operation.json となる
    - URL の service, operation は PascalCase から snake_case に自動で変換される
- JSON Schema はライブラリの都合で Draft3 対応のみ
- dispatch.conf に書かれている API のスキーマがが読み込めない場合はエラーとなる

## 導入方法

まずは自分のアプリを作成します。

rebar を使っていれば create-app を使うのが簡単です

```
$ rebar create-app appid=spam
```

### rebar.config deps へ swidden を設定する

その後 rebar.config の deps に以下の設定をします

```
{deps,
  [
   {swidden,
    ".*", {git, "git@github.com:shiguredo/swidden.git", {tag, "4.0.0"}}}
  ]
}.
```

### priv ディレクトリ

priv ディレクトリの下に swidden ディレクトリを作ります

その下にスキーマファイルとディスパッチファイルを置きます。

#### dispatch.conf

ディスパッチファイルの名前は dispatch.conf 固定です。

```
spam/priv/swidden/dispatch.conf
```

- サービス名はヘッダー値の Service_Version.Operation の Service にあたります
    - binary 型の PascalCase で指定します
- バージョンはヘッダー値の Service_Version.Operation の Version にあたります
    - binary 型の YYYYMMDD で指定します
- オペレーション名はヘッダー値の Service_Version.Operation の Operation にあたります
    - binary 型の PascalCase で指定します

以下は dispatch.conf の構造です。

```
{サービス名1, [
    {バージョン1,
        [{オペレーション名1, モジュール名1},
         {オペレーション名2, モジュール名1}]},
    {バージョン2
        [{オペレーション名1, モジュール名2}]}]}.
{サービス名1, [
    {バージョンX,
        [{オペレーション名3, モジュール名3}]}]}.
```

モジュール名はそのオペレーションが実装されているモジュールを指定します。
モジュールが spam_user でオペレーションが CreateUser の場合は spam_user:create_user/1 が呼び出されます。

実際の値で埋めた設定が以下の通りになります。

```erlang
{<<"Spam">>, [
    {<<"20141101">>,
        [{<<"GetUser">>, spam_user},
         {<<"CreateUser">>, spam_user},
         {<<"UpdateUser">>, spam_user}
         {<<"DeleteUser">>, spam_user}]},
    {<<"20150701">>,
        [{<<"CreateUser">>, spam_user_with_group}]}]}.
{<<"SpamAdmin">>, [
    {<<"20141101">>,
        [{<<"GetMetrics">>, spam_admin}]}]}.
```

ハンドリングはすべて dispatch.conf に書かれている通りに動作します。

#### schemas

schemas の構造は以下の通りです

```
spam/priv/swidden/schemas/<service_name>/<version>/<schema>.json
```

以下は注意点です

- service_name は dispatch.conf のサービス名を snake_case にしたものが使われる
- schema は dispatch.conf のオペレーション名を snake_case にしたものが使われる
- dispatch.conf に設定されているオペレーションの JSON Schema が存在しない場合はエラーになる

以下は dispatch.conf に設定した分のスキーマ一覧です

- spam/priv/swidden/schemas/spam/20141101/get_user.json
- spam/priv/swidden/schemas/spam/20141101/create_user.json
- spam/priv/swidden/schemas/spam/20141101/update_user.json
- spam/priv/swidden/schemas/spam/20141101/delete_user.json
- spam/priv/swidden/schemas/spam/20150701/create_user.json
- spam/priv/swidden/schemas/spam_admin/20141101/get_metrics.json

これで priv 以下の設定は終わりです。

##### get_user.json の JSON Schema 例

```
{
    "properties": {
        "username": {"type": "string", "required": true}
    }
}
```

### 実際にアプリに組み込む

swidden:start/1 の引数は自分の作成しているアプリの名前です。

このアプリの名前を使って spam/priv/ のパスを探し出します。

アプリ起動時に swidden:start/1 を実行すればアプリの起動時に自動で dispatch.conf や JSON Schema を読み込みます。

```erlang
-module(spam_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, _Pid} = swidden:start(spam, [{port, 5000}, {header_name, <<"x-spam-target">>}]),

    ok = spam_user:start(),

    spam_sup:start_link().

stop(_State) ->
    ok = swidden:stop(),
    ok.
```

dispatch.conf で設定したモジュールの例です

戻り値に swidden:success/0,1 と swidden:failure/1 を使用します。

```erlang
-module(spam_user).

-export([start/0]).
-export([get_user/1, create_user/1, update_user/1, delete_user/1]).

-define(TABLE, spam_user_table).


start() ->
    _Tid = ets:new(?TABLE, [set, public, named_table]),
    ok.


get_user(#{<<"username">> := Username}) ->
    case ets:lookup(?TABLE, Username) of
        [] ->
            swidden:failure(<<"MissingUserException">>);
        [{Username, Password}] ->
            %% proplists を戻せば JSON で返ります
            swidden:success([{password, Password}]);
        [{Username, Password, _Group}] ->
            %% spam_user_with_group 対応
            swidden:success(#{password => Password})
    end.


create_user(#{<<"username">> := Username, <<"password">> := Password}) ->
    case ets:insert_new(?TABLE, {Username, Password}) of
        true ->
            swidden:success();
        false ->
            swidden:failure(<<"DuplicateUserException">>)
    end.


update_user(#{<<"username">> := Username, <<"password">> := Password}) ->
    case ets:lookup(?TABLE, Username) of
        [] ->
            swidden:failure(<<"MissingUserException">>);
        [{Username, _OldPassword}] ->
            true = ets:insert(?TABLE, {Username, Password}),
            swidden:success();
        [{Username, _OldPassword, Group}] ->
            %% spam_user_with_group 対応
            true = ets:insert(?TABLE, {Username, Password, Group}),
            swidden:success()
    end.


delete_user(JSON) ->
    Username = proplists:get_value(<<"username">>, JSON),
    case ets:lookup(?TABLE, Username) of
        [] ->
            swidden:failure(<<"MissingUserException">>);
        _ ->
            true = ets:delete(?TABLE, Username),
            swidden:success()
    end.
```

完全版はこのリポジトリの examples/spam にありますのでそちらを参照してください

#### swidden:success/0,1

swidden:success/0,1 は処理が成功したときに使用します。

- success/0 は特に返す値がない場合使用します
    - Body が空で戻ります
- success/1 は戻したい JSON (proplists) を引数に指定します

#### swidden:failure/1

swidden:failure/1 は処理が失敗したときに使用します。

引数には binary 型でエラーの文字列を入れてください。

たとえばユーザが存在しなかった時は <<"MissingUserException">> などです。

戻りは {"error_type": "MissingUserException"} となります。

### swidden:failure/2

swidden:failure/1 は処理が失敗したときに使用し、Type 意外に Reason が指定できます。

Reason は自由にユーザが決めて良い値です。 Reason はマップを使用してください。

たとえば Reason に #{code := 500} というのを入れた場合

戻りは {"error_type": "MissingUserException", "error_reason": {"code": 500}} となります。

#### 動作確認

examples/spam で make; make dev を実行します。

```
$ make; make dev
```

```
$ dev/spam/bin/spam
Erlang/OTP 17 [erts-6.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

Eshell V6.2  (abort with ^G)
(spam@127.0.0.1)1>
```

ユーザを追加してみます

```
$ http POST 127.0.0.1:5000/ "x-spam-target:Spam_20141101.CreateUser" username=yakihata password=nogyo -vvv
POST / HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 45
Content-Type: application/json; charset=utf-8
Host: 127.0.0.1:5000
User-Agent: HTTPie/0.8.0
x-spam-target: Spam_20141101.CreateUser

{
    "password": "nogyo",
    "username": "yakihata"
}

HTTP/1.1 200 OK
connection: keep-alive
content-length: 2
content-type: application/json
date: Sun, 02 Nov 2014 18:53:09 GMT
server: Cowboy
```

ユーザを確認してみます

```
$ http POST 127.0.0.1:5000/ "x-spam-target:Spam_20141101.GetUser" username=yakihata -vvv
POST / HTTP/1.1
Accept: application/json
Accept-Encoding: gzip, deflate
Connection: keep-alive
Content-Length: 24
Content-Type: application/json; charset=utf-8
Host: 127.0.0.1:5000
User-Agent: HTTPie/0.8.0
x-spam-target: Spam_20141101.GetUser

{
    "username": "yakihata"
}

HTTP/1.1 200 OK
connection: keep-alive
content-length: 20
content-type: application/json
date: Sun, 02 Nov 2014 18:53:33 GMT
server: Cowboy

{
    "password": "nogyo"
}
```

このライブラリを使えばコスト低く JSON Schema を使った HTTP API が作成できます。

## 利用したいサービスを指定したい場合

そのポートで利用するサービスを固定したい場合は swidden:start する際の引数に [{services, [<<"Spam">>]}] とサービスを指定することで、そのサービスだけが有効になります。

```
{ok, _} = swidden:start(spam, [{port, 3000}, {services, [<<"Spam">>]}]),
{ok, _} = swidden:start(spam, [{port, 5000}, {services, [<<"SpamAdmin">>]}]),
```

Spam は 3000 番ポートで、 SpamAdmin は 5000 番ポートで有効になります。

## 送信の時の Body が空の場合

たとえば ListUsers などの一覧取得の場合はもしかすると Body を空で送信する場合が出てくるかもしれません。

その場合は以下のようにしてください

- JSON Schema は用意するが {} と設定する
- 呼び出される関数は引数なしで実装する

```
list_users() ->
    Users = [ #{username => Username,
               {password => Password} || {Username, Password} <- ets:tab2list(?TABLE) ],
    swidden:success(Users).
```

## middleware の処理結果を handler に渡したい場合

たとえば認証を cowboy_middleware で処理して、成功したら取得した UserID を handler で使いたい場合などがあるかもしれません。

- middleware の引数 Env のキー handler_opts に対する値に、handler に渡したい値を追加する
- handler は Opts引数付き (Opts) か (JSON, Opts) で実装し、middleware から渡された値を Opts から取り出す
- swidden:start で middleware を追加する

例は以下です。

spam_auth.erl

```
-behaviour(cowboy_middleware).

execute(Req, Env) ->
    {bearer, Token} = cowboy_req:parse_header(<<"authorization">>, Req),
    UserID = get_user_id(Token),
    HandlerOpts = proplists:get_value(handler_opts, Env),
    Env2 = lists:keyreplace(handler_opts, 1, Env, {handler_opts, [{user_id, UserID} | HandlerOpts]}),
    {ok, Req, Env2}.
```

spam_handler.erl

```
get_user(Opts) ->
    UserID = proplists:get_value(user_id, Opts),
    spam_db:get_user(UserID),
    swidden:success(User).
```

## TODO

- 認証機能
- Response に対する JSON Schema によるバリデーション
- dispatch.conf の Map 化

## 既知の問題

今のところなし

## ライセンス

```
Copyright 2016 Shiguredo Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
