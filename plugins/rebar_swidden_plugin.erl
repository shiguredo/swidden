-module(rebar_swidden_plugin).

-export([swidden_init/2]).
-export([swidden_gen_schemas/2]).
-export([swidden_doc/2]).

-define(DOCS_DIR, "api_docs").
-define(TEMPLATE, "template.dtl").
-define(TEMPLATE_MODULE, swidden_doc_template).

swidden_init(Config, _AppFile) ->
    BaseDir = rebar_config:get_xconf(Config, base_dir),
    %% TODO: use code:priv_dir
    SwiddenDir = filename:join([BaseDir, "priv", "swidden"]),
    SchemasDir = filename:join([SwiddenDir, "schemas"]),
    ok = filelib:ensure_dir(SchemasDir),
    ConfPath = filename:join(SwiddenDir, "dispatch.conf"),
    case filelib:is_file(ConfPath) of
        true ->
            exit(already_initialized);
        false ->
            Service = rebar_config:get_global(Config, service, "Spam"),
            Body = "{<<\"" ++ Service ++ "\">>,\n  [{<<\"19700101\">>,\n    [{<<\"GetUser\">>, spam_user}]}]}.\n",
            io:format("Writing ~s~n", [ConfPath]),
            file:write_file(ConfPath, Body)
    end,
    ok.


swidden_gen_schemas(Config, _AppFile) ->
    BaseDir = rebar_config:get_xconf(Config, base_dir),
    %% TODO: use code:priv_dir
    SwiddenDir = filename:join([BaseDir, "priv", "swidden"]),
    SchemasDir = filename:join([SwiddenDir, "schemas"]),
    ConfPath = filename:join(SwiddenDir, "dispatch.conf"),
    case filelib:is_file(ConfPath) of
        true ->
            gen_schemas(ConfPath, SchemasDir);
        false ->
            exit(missing_conf)
    end,
    ok.


gen_schemas(ConfPath, SchemasDir) ->
    {ok, Dispatch} = file:consult(ConfPath),
    lists:foreach(fun({Service, Versions}) ->
                  Versions2 = lists:foreach(fun({Version, Operations}) ->
                                            Operations2 = lists:foreach(fun({Operation, _}) ->
                                                                        FileName = list_to_binary([swidden_misc:pascal2snake(Operation), ".json"]), 
                                                                        SchemaPath = filename:join([SchemasDir,
                                                                                                    swidden_misc:pascal2snake(Service),
                                                                                                    Version,
                                                                                                    FileName]),
                                                                        filelib:ensure_dir(SchemaPath),
                                                                        case filelib:is_file(SchemaPath) of
                                                                            true ->
                                                                              ok;
                                                                            false ->
                                                                              Body = "{\n    \"description\": \"\",\n    \"properties\": {\n    }\n}",
                                                                              RelativePath = filename:join(["priv", "swidden", "schemas",
                                                                                                            swidden_misc:pascal2snake(Service),
                                                                                                            Version, FileName]),
                                                                              io:format("Writing ~s~n",[RelativePath]),
                                                                              file:write_file(SchemaPath, Body)
                                                                        end
                                                                    end, Operations),
                                            {Version, Operations2}
                                        end, Versions),
                  {Service, Versions2}
    end, Dispatch).


swidden_doc(Config, _AppFile) ->
    BaseDir = rebar_config:get_xconf(Config, base_dir),
    %% TODO: use code:priv_dir
    SwiddenDir = filename:join([BaseDir, "priv", "swidden"]),
    %% TODO: enable to specify `docs_dir`
    DocsDir = filename:join([BaseDir, ?DOCS_DIR]),
    gen_swidden_doc(SwiddenDir, DocsDir).


gen_swidden_doc(SwiddenDir, DocsDir) ->
    ConfPath = filename:join(SwiddenDir, "dispatch.conf"),
    case file:consult(ConfPath) of
        {ok, Dispatch} ->
            Dispatch2 = load_schemas(Dispatch, SwiddenDir),
            case prepare_docs_dir(DocsDir) of
                ok ->
                    ok = compile_template(),
                    F1 = fun({Service, Versions}) ->
                             FileName = list_to_binary([swidden_misc:pascal2snake(Service), ".md"]),
                             Path = filename:join(DocsDir, FileName),
                             Markdown = gen_md(Service, Versions),
                             {FileName, Path, Markdown}
                         end,
                    Docs = lists:map(F1, Dispatch2),

                    F2 = fun({FileName, Path, Markdown}) ->
                             io:format("Writing ~s~n", [filename:join(?DOCS_DIR, FileName)]),
                             file:write_file(Path, Markdown)
                         end,
                    ok = lists:foreach(F2, Docs),
                    io:format("Docs successfully generated.~n");
                {error, _Error} ->
                    exit(cannot_create_docs_dir)
            end;
        {error, _Error} ->
            exit(cannot_load_conf)
    end.


prepare_docs_dir(DocsDir) ->
    filelib:ensure_dir(filename:join(DocsDir, "dummy")).


compile_template() ->
    PrivPath = code:priv_dir(swidden),
    {ok, ?TEMPLATE_MODULE} = erlydtl:compile_file(filename:join(PrivPath, ?TEMPLATE), ?TEMPLATE_MODULE, [{auto_escape, false}]),
    ok.


load_schemas(Dispatch, SwiddenDir) ->
    lists:map(fun({Service, Versions}) ->
                  Versions2 = lists:map(fun({Version, Operations}) ->
                                            Operations2 = lists:map(fun({Operation, _}) ->
                                                                        FileName = list_to_binary([swidden_misc:pascal2snake(Operation), ".json"]), 
                                                                        SchemaPath = filename:join([SwiddenDir,
                                                                                                    "schemas",
                                                                                                    swidden_misc:pascal2snake(Service),
                                                                                                    Version,
                                                                                                    FileName]),
                                                                        %% TODO: error handling
                                                                        {ok, Binary} = file:read_file(SchemaPath),
                                                                        Schema = jsone:decode(Binary, [{format, proplist}]),
                                                                        {Description, Properties} = analyze_schema(Schema),
                                                                        {Operation, Description, Properties}
                                                                    end, Operations),
                                            {Version, Operations2}
                                        end, Versions),
                  {Service, Versions2}
    end, Dispatch).


analyze_schema(Schema) ->
    Description = proplists:get_value(<<"description">>, Schema, <<"">>),
    Properties = proplists:get_value(<<"properties">>, Schema),
    {Description, format_properties(Properties)}.


%% TODO: more cool format
format_properties(Properties) ->
    jsone:encode(Properties).


gen_md(Service, Versions) ->
    case ?TEMPLATE_MODULE:render([{service, Service}, {versions, Versions}]) of
        {ok, IOList} ->
            IOList;
        {error, _Error} ->
            exit(failed_to_render)
    end.
