%% @private
-module(rebar3_ex_doc).

-export([
    init/1,
    do/1,
    format_error/1
]).

-define(PROVIDER, ex_doc).
-define(DEPS, [{default, lock}]).
-define(PRV_ERROR(Reason),
    {error, {?MODULE, Reason}}
).

-define(RAISE(Reason), erlang:error(?PRV_ERROR(Reason))).

-define(DEFAULT_DOC_DIR, "doc").
-define(DEFAULT_DOC_LANG, "en").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
        State,
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 ex_doc"},
            {short_desc, "Generate documentation using ex_doc."},
            {desc, "Generate documentation using ex_doc."},
            {opts, [
                {app, $a, "app", string, help(app)},
                {canonical, $n, "canonical", string, help(canonical)},
                {output, $o, "output", {string, ?DEFAULT_DOC_DIR}, help(output)},
                {language, undefined, "language", {string, ?DEFAULT_DOC_LANG}, help(language)},
                {logo, $l, "logo", string, help(logo)},
                {formatter, $f, "formatter", string, help(formatter)}
            ]},
            {profiles, [docs]}
        ])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) ->
    {ok, rebar_state:t()} | {error, string()} | {error, {module(), any()}}.
do(State) ->
    case list_to_integer(erlang:system_info(otp_release)) of
        Min when Min >= 24 ->
            Apps = get_apps(State),
            run(State, Apps);
        Ver ->
            ?RAISE({unsupported_otp, Ver})
    end.

-spec format_error(any()) -> iolist().
format_error({app_not_found, AppName}) ->
    io_lib:format("The app '~ts' specified was not found.", [AppName]);
format_error({gen_chunks, Err}) ->
    rebar_api:debug("An unknown error occured generating chunks: ~p", [Err]),
    "An unknown error occured generating doc chunks with edoc. Run with DIAGNOSTICS=1 for more details.";
format_error({compile, Err}) ->
    rebar_api:debug("An unknown error occured compiling apps: ~p", [Err]),
    "An unknown error occured compiling apps. Run with DIAGNOSTICS=1 for more details.";
format_error({write_config, Err}) ->
    rebar_api:debug("Unknown error error occurred generating docs config: ~p", [Err]),
    "An unknown error occured generating docs config. Run with DIAGNOSTICS=1 for more details.";
format_error({unsupported_otp, Ver}) ->
    Str = "You are using erlang/OTP '~ts', but this plugin requires at least erlang/OTP 24.",
    io_lib:format(Str, [integer_to_list(Ver)]);
format_error({ex_doc, _}) ->
    "";
format_error(Err) ->
    rebar_api:debug("An unknown error occured: ~p", [Err]),
    "An unknown error has occured. Run with DIAGNOSTICS=1 for more details.".

%% ===================================================================
%% Private
%% ===================================================================

-spec get_apps(rebar_state:t()) -> [rebar_app_info:t()].
get_apps(State) ->
    case rebar_state:current_app(State) of
        undefined ->
            {Opts, _Args} = rebar_state:command_parsed_args(State),
            case proplists:get_value(app, Opts, undefined) of
                undefined ->
                    rebar_state:project_apps(State);
                AppName ->
                    App = find_app(rebar_state:project_apps(State), AppName),
                    [App]
            end;
        AppInfo ->
            [AppInfo]
    end.

-spec find_app(rebar_app_info:t(), string()) -> rebar_app_info:t().
find_app(Apps, Name) ->
    Pred = fun(App) ->
        rebar_app_info:name(App) =:= rebar_utils:to_binary(Name)
    end,
    case lists:filter(Pred, Apps) of
        [App] ->
            {ok, App};
        _ ->
            ?RAISE({app_not_found, Name})
    end.

-spec run(rebar_state:t(), [rebar_app_info:t()] | rebar_app_info:t()) -> {ok, rebar_state:t()}.
run(State, Apps) ->
    State1 = compile(State),
    lists:foreach(
        fun(App) ->
            {State2, App1, EdocOutDir} = gen_chunks(State1, App),
            ex_doc(State2, App1, EdocOutDir)
        end,
        Apps
    ),
    {ok, State}.

-spec compile(rebar_state:t()) -> rebar_state:t().
compile(State) ->
    Prv = providers:get_provider(compile, rebar_state:providers(State)),
    case providers:do(Prv, State) of
        {ok, State1} ->
            State1;
        {error, Err} ->
            ?RAISE({compile, Err})
    end.

-spec gen_chunks(rebar_state:t(), file:filename()) -> {rebar_state:t(), file:filename()}.
gen_chunks(State, App) ->
    OutDir = filename:join(rebar_app_info:out_dir(App), "doc"),
    Prv = providers:get_provider(edoc, rebar_state:providers(State)),
    EdocOpts = [
        {preprocess, true},
        {doclet, edoc_doclet_chunks},
        {layout, edoc_layout_chunks},
        {dir, OutDir},
        private,
        hidden
    ],
    State1 = rebar_state:set(State, edoc_opts, EdocOpts),
    AppOpts = rebar_app_info:opts(App),
    AppOpts1 = rebar_opts:set(AppOpts, edoc_opts, EdocOpts),
    App1 = rebar_app_info:opts(App, AppOpts1),
    State2 = rebar_state:project_apps(State1, [App1]),
    case providers:do(Prv, State2) of
        {ok, State3} ->
            {State3, App1, OutDir};
        {error, Err} ->
            ?RAISE({gen_chunks, Err})
    end.

-spec ex_doc(rebar_state:t(), rebar_app_info:t(), file:filename()) -> {ok, rebar_state:t()}.
ex_doc(State, App, EdocOutDir) ->
    AppName = rebar_utils:to_list(rebar_app_info:name(App)),
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    CommandStr = make_command_string(State, App, EdocOutDir, Opts),
    rebar_api:info("Running ex_doc for ~ts", [AppName]),
    case rebar_utils:sh(CommandStr, [{return_on_error, true}]) of
        {ok, []} ->
            {ok, State};
        {ok, Warnings} ->
            rebar_api:warn("~ts", [Warnings]),
            {ok, State};
        {error, {_Code, Errors}} ->
            rebar_api:error("~ts", [Errors]),
            ?RAISE({ex_doc, Errors})
    end.

-spec make_command_string(
    rebar_state:t(), rebar_app_info:t(), file:filename(), proplists:proplist()
) -> string().
make_command_string(State, App, EdocOutDir, Opts) ->
    AppName = rebar_utils:to_list(rebar_app_info:name(App)),
    Vsn = vcs_vsn(State, App),
    SourceRefVer = io_lib:format("v~ts", [Vsn]),
    Ebin = rebar_app_info:ebin_dir(App),
    ExDocScript = filename:join([code:priv_dir(rebar3_ex_doc), "ex_doc"]),
    BaseArgs = [
        ExDocScript,
        AppName,
        Vsn,
        Ebin,
        "--source-ref",
        SourceRefVer,
        "--config",
        ex_doc_config_file(App, EdocOutDir),
        "--output",
        output_dir(App, Opts),
        "--quiet"
    ],
    Optionals = [canonical, language, logo, formatter],
    CommandArgs = lists:foldl(
        fun(Opt, Args) -> Args ++ maybe_add_opt(Opt, Opts) end,
        BaseArgs,
        Optionals
    ),
    string:join(CommandArgs, " ").

-spec ex_doc_config_file(rebar_app_info:t(), file:filename()) -> file:filename().
ex_doc_config_file(App, EdocOutDir) ->
    ExDocOpts = ex_doc_opts_defaults(rebar_app_info:get(App, ex_doc, [])),
    ExDocConfigFile = filename:join([EdocOutDir, "docs.config"]),
    ok = write_config(ExDocConfigFile, ExDocOpts),
    ExDocConfigFile.

ex_doc_opts_defaults(Opts) ->
    case proplists:get_value(proglang, Opts, undefined) of
        undefined ->
            Opts ++ [{proglang, erlang}];
        _ ->
            Opts
    end.

-spec maybe_add_opt(atom(), proplists:proplist()) -> list().
maybe_add_opt(formatter, Opts) ->
    case proplists:get_all_values(formatter, Opts) of
        [] ->
            ["-f", "html", "-f", "epub"];
        [Formatter] ->
            ["-f", Formatter];
        Formatters ->
            ["-f"] ++ lists:join("-f", Formatters)
    end;
maybe_add_opt(canonical, Opts) ->
    case proplists:get_value(canonical, Opts, undefined) of
        undefined ->
            [];
        Canonical ->
            ["--canonical", Canonical]
    end;
maybe_add_opt(language, Opts) ->
    Lang = proplists:get_value(language, Opts, ?DEFAULT_DOC_LANG),
    ["--language", Lang];
maybe_add_opt(logo, Opts) ->
    case proplists:get_value(logo, Opts, undefined) of
        undefined ->
            [];
        Logo ->
            ["--logo", Logo]
    end.

-spec output_dir(rebar_app_info:t(), proplists:proplist()) -> file:filename().
output_dir(App, Opts) ->
    Dir = proplists:get_value(output, Opts, ?DEFAULT_DOC_DIR),
    filename:join(rebar_app_info:dir(App), Dir).

-spec write_config(file:filename(), proplists:proplist()) -> ok.
write_config(File, Config) ->
    NewConfig = [[io_lib:print(Prop), ".", io_lib:nl()] || Prop <- Config],
    BinConfig = iolist_to_binary(["%% coding: utf-8", io_lib:nl(), NewConfig]),
    case file:write_file(File, BinConfig, [{encoding, utf8}]) of
        ok ->
            ok;
        {error, Err} ->
            ?RAISE({write_config, Err})
    end.

-spec vcs_vsn(rebar_state:t(), rebar_app_info:t()) -> string().
vcs_vsn(State, App) ->
    Version = rebar_app_info:original_vsn(App),
    AppDir = rebar_app_info:dir(App),
    Resources = rebar_state:resources(State),
    vcs_vsn(Version, AppDir, Resources).

vcs_vsn(OriginalVsn, Dir, Resources) when is_list(Dir), is_list(Resources) ->
    FakeState = rebar_state:new(),
    {ok, AppInfo} = rebar_app_info:new(fake, OriginalVsn, Dir),
    vcs_vsn(
        AppInfo,
        OriginalVsn,
        rebar_state:set_resources(FakeState, Resources)
    );
vcs_vsn(AppInfo, Vcs, State) ->
    case vcs_vsn_cmd(AppInfo, Vcs, State) of
        {plain, VsnString} ->
            VsnString;
        {cmd, CmdString} ->
            cmd_vsn_invoke(CmdString, rebar_app_info:dir(AppInfo));
        unknown ->
            rebar_utils:abort("vcs_vsn: Unknown vsn format: ~p", [Vcs]);
        {error, Reason} ->
            rebar_utils:abort("vcs_vsn: ~ts", [Reason])
    end.

%% Temp work around for repos like relx that use "semver"
vcs_vsn_cmd(_, Vsn, _) when is_binary(Vsn) ->
    {plain, Vsn};
vcs_vsn_cmd(AppInfo, VCS, State) when VCS =:= semver; VCS =:= "semver" ->
    vcs_vsn_cmd(AppInfo, git, State);
vcs_vsn_cmd(_AppInfo, {cmd, _Cmd} = Custom, _) ->
    Custom;
vcs_vsn_cmd(AppInfo, {file, File}, _) ->
    Path = filename:join(rebar_app_info:dir(AppInfo), File),
    {ok, Vsn} = file:read_file(Path),
    {plain, rebar_utils:to_list(rebar_string:trim(Vsn))};
vcs_vsn_cmd(AppInfo, VCS, State) when is_atom(VCS) ->
    rebar_resource_v2:make_vsn(AppInfo, VCS, State);
vcs_vsn_cmd(AppInfo, {VCS, _} = V, State) when is_atom(VCS) ->
    rebar_resource_v2:make_vsn(AppInfo, V, State);
vcs_vsn_cmd(AppInfo, VCS, State) when is_list(VCS) ->
    try list_to_existing_atom(VCS) of
        AVCS ->
            case vcs_vsn_cmd(AppInfo, AVCS, State) of
                unknown -> {plain, VCS};
                Other -> Other
            end
    catch
        error:badarg ->
            {plain, VCS}
    end;
vcs_vsn_cmd(_, _, _) ->
    unknown.

cmd_vsn_invoke(Cmd, Dir) ->
    {ok, VsnString} = rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, false}]),
    rebar_string:trim(VsnString, trailing, "\n").

help(app) ->
    "Specify which app to generate docs for within an umbrella project";
help(canonical) ->
    "Indicate the preferred URL with rel=\"canonical\" link element,"
    "defaults to no canonical path.";
help(output) ->
    "Output directory for the generated docs.";
help(formatter) ->
    "Which formatters to use, \"html\" or \"epub\"."
    "This option can be given more than once."
    "By default, both html and epub are generated.";
help(language) ->
    "Identify the primary language of the documents,"
    "its value must be a valid BCP 47 language tag."
    "See https://tools.ietf.org/html/bcp47 BCO 47 for details.";
help(logo) ->
    "Path to the image logo of the project (only PNG or JPEG accepted)."
    "The image size will be 64x64 and copied to the assets directory.".
