-module(swidden_interceptor).

-callback execute(module(), function(), swidden:json_object()) ->
    {continue, swidden:json_object()} |
    {stop, ok} |
    {stop, {ok, swidden:json_object()}} |
    {sotp, {redirect, binary()}} |
    {stop, {error, binary()}}.

-callback execute(module(), function()) ->
    continue |
    {stop, ok} |
    {stop, {ok, swidden:json_object()}} |
    {sotp, {redirect, binary()}} |
    {stop, {error, binary()}}.
