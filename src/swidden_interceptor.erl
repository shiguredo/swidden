-module(swidden_interceptor).

%% 引数なし API の事前処理
-callback preprocess(module(), function(), swidden:json_object()) ->
    {continue, swidden:json_object()} |
    {stop, ok} |
    {stop, {ok, swidden:json_object()}} |
    {sotp, {redirect, binary()}} |
    {stop, {error, binary()}} |
    {stop, {error, binary(), map()}}.

%% 引数あり API の事前処理
-callback preprocess(module(), function()) ->
    continue |
    {stop, ok} |
    {stop, {ok, swidden:json_object()}} |
    {sotp, {redirect, binary()}} |
    {stop, {error, binary()}} |
    {stop, {error, binary(), map()}}.

%% 事後処理
-callback postprocess(module(), function(),
                      ok | {ok, swidden:json_object()} | {ok, {redirect, binary()}} |
                      {error, binary()} | {error, binary(), map()})  ->
    ok |
    {ok, swidden:json_object()} |
    {ok, {redirect, binary()}} |
    {error, binary()} |
    {error, binary(), map()}.
