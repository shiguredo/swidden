-module(swidden_middleware).

-export([failure/2, failure/3]).

-define(HTTP_HEADER_CONTENT_TYPE,      <<"content-type">>).
-define(CONTENT_TYPE_APPLICATION_JSON, <<"application/json">>).


failure(Req, Type) when is_binary(Type) ->
    cowboy_req:reply(400,
                     #{?HTTP_HEADER_CONTENT_TYPE => ?CONTENT_TYPE_APPLICATION_JSON},
                     jsone:encode(#{error_type => Type}),
                     Req).


failure(Req, Type, Reason) when is_binary(Type) andalso is_map(Reason) ->
    cowboy_req:reply(400,
                     #{?HTTP_HEADER_CONTENT_TYPE => ?CONTENT_TYPE_APPLICATION_JSON},
                     jsone:encode(#{error_type => Type, error_reason => Reason}),
                     Req).
