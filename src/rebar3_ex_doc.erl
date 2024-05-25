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
-define(DEFAULT_MERMAID_VSN, "10.2.4").

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
                {ex_doc, $e, "ex_doc", string, help(ex_doc)},
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
    case otp_release() of
        Min when Min >= 25 ->
            Apps = get_apps(State),
            run(State, Apps);
        Ver ->
            ?RAISE({unsupported_otp, Ver})
    end.

-spec otp_release() -> OTPRelease
when OTPRelease :: pos_integer().
otp_release() ->
    list_to_integer(erlang:system_info(otp_release)).

-spec format_error(any()) -> iolist().
format_error({app_not_found, AppName}) ->
    io_lib:format("The app '~ts' specified was not found.", [AppName]);
format_error({invalid_ex_doc_path, Path}) ->
    Invalid = io_lib:format("Invalid ex_doc escript path : '~ts'~n", [Path]),
    Invalid ++ "Ensure that the path specified is a path to working ex_doc escript.";
format_error({gen_chunks, Err}) ->
    rebar_api:debug("An unknown error occurred generating chunks: ~p", [Err]),
    "An unknown error occurred generating doc chunks with edoc. Run with DIAGNOSTICS=1 for more details.";
format_error({compile, Err}) ->
    rebar_api:debug("An unknown error occurred compiling apps: ~p", [Err]),
    "An unknown error occurred compiling apps. Run with DIAGNOSTICS=1 for more details.";
format_error({write_config, Err}) ->
    rebar_api:debug("Unknown error error occurred generating docs config: ~p", [Err]),
    "An unknown error occurred generating docs config. Run with DIAGNOSTICS=1 for more details.";
format_error({unsupported_otp, Ver}) ->
    Str = "You are using Erlang/OTP '~ts', but this plugin requires at least Erlang/OTP 25.",
    io_lib:format(Str, [integer_to_list(Ver)]);
format_error({mermaid_vsn_not_string, Vsn}) ->
    Str = "ex_doc option 'with_mermaid' should be 'true', 'false', or string(). Got: ~p",
    io_lib:format(Str, [Vsn]);
format_error({no_compatible_ex_doc, OTPRelease}) ->
    Str = "ex_doc escript not available for (current) OTP release ~p~n",
    io_lib:format(Str, [OTPRelease]);
format_error({ex_doc, _}) ->
    "";
format_error(Err) ->
    rebar_api:debug("An unknown error occurred: ~p", [Err]),
    "An unknown error has occurred. Run with DIAGNOSTICS=1 for more details.".

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
                    {ok, App} = find_app(rebar_state:project_apps(State), AppName),
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

-spec gen_chunks(rebar_state:t(), file:filename()) -> {rebar_state:t(), rebar_app_info:t(), file:filename()}.
gen_chunks(State, App) ->
    OutDir = filename:join(rebar_app_info:out_dir(App), "doc"),
    EdocOptsFromRebarConfig = lists:keysort(1, proplists:unfold(rebar_state:get(State, edoc_opts, []))),
    Prv = providers:get_provider(edoc, rebar_state:providers(State)),
    EdocOptsDefault = lists:keysort(1, [
        {preprocess, true},
        {doclet, edoc_doclet_chunks},
        {layout, edoc_layout_chunks},
        {dir, OutDir},
        {includes, ["src", "include"]}
    ]),
    EdocOpts = lists:keymerge(1, EdocOptsFromRebarConfig, EdocOptsDefault),
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
    % We replace output default ( "doc" ), with undefined here to keep decision logic for which output
    % value to use later simple (i.e., output provided on CLI or in config).
    Opts1 = lists:map(fun ({output, "doc"}) -> {output, undefined}; (Prop) -> Prop end, Opts),
    CommandStr = make_command_string(State, App, EdocOutDir, Opts1),
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
    AppSrcFile = rebar_app_info:app_file_src(App),
    AppSrc = rebar_file_utils:try_consult(AppSrcFile),
    PkgName = proplists:get_value(pkg_name, AppSrc, AppName),
    ExDocOpts = rebar_app_info:get(App, ex_doc, []),
    Vsn = vcs_vsn(State, App),
    SourceRefVer = maybe_prefix_with_v(Vsn, ExDocOpts),
    Ebin = rebar_app_info:ebin_dir(App),
    BaseArgs = [
        ex_doc_escript(Opts),
        AppName,
        Vsn,
        Ebin,
        "--source-ref",
        SourceRefVer,
        "--config",
        ex_doc_config_file(App, EdocOutDir),
        "--quiet"
    ] ++ maybe_add_output_dir(App, Opts) ++ maybe_package_arg(PkgName, rebar_app_info:get(App, ex_doc, [])),
    DepPaths = rebar_state:code_paths(State, all_deps),
    PathArgs = lists:foldl(
        fun(Path, Args) -> ["--paths", Path | Args] end,
        [],
        DepPaths
    ),
    Optionals = [canonical, language, logo, formatter],
    CommandArgs = lists:foldl(
        fun(Opt, Args) -> Args ++ maybe_add_opt(Opt, Opts) end,
        BaseArgs ++ PathArgs,
        Optionals
    ),
    string:join(CommandArgs, " ").

maybe_add_output_dir(App, Opts) ->
    case {proplists:get_value(output, Opts, undefined), get_exdoc_opt(App, output)} of
        {undefined, undefined} ->
            ["--output", output_dir(App, ?DEFAULT_DOC_DIR)];
        {OutputDir, _} when OutputDir =/= undefined ->
            ["--output", output_dir(App, OutputDir)];
        {undefined, Specified}  ->
            ["--output", output_dir(App, Specified)]
    end.

maybe_package_arg(PkgName, Opts) when Opts =:= []
                                      orelse is_tuple(hd(Opts))
                                      orelse is_atom(hd(Opts)) ->
    case proplists:get_value(package, Opts, true) of
        true ->
            ["--package", rebar_utils:to_list(PkgName)];
        false ->
            []
    end;
maybe_package_arg(_, _) ->
    [].

maybe_prefix_with_v(Vsn, Opts) ->
    case proplists:get_value(prefix_ref_vsn_with_v, Opts, true) of
        true ->
            io_lib:format("v~ts", [Vsn]);
        false ->
            Vsn
    end.

ex_doc_escript(Opts) ->
    ExDoc = ex_doc_script_path(Opts),
    case os:type() of
         {win32, _} ->
            win32_ex_doc_script(ExDoc);
        _ ->
         ExDoc
    end.

ex_doc_script_path(Opts) ->
    ExDocOpts = proplists:get_value(ex_doc, Opts, undefined),
    case ExDocOpts of
        undefined ->
             ex_doc_for(otp_release(), 0);
        Path ->
            case filelib:is_regular(Path) of
                true ->
                    Path;
                false ->
                    ?RAISE({invalid_ex_doc_path, Path})
            end
    end.

ex_doc_for(OTPRelease, Sub) ->
    case ex_doc_exists_for(OTPRelease - Sub) of
        {true, File} ->
            File;
        % assuming compatibility lasts 3 versions
        false when Sub < 3 ->
            ex_doc_for(OTPRelease, Sub + 1);
        false ->
            ?RAISE({no_compatible_ex_doc, OTPRelease - Sub + 1})
    end.

ex_doc_exists_for(OTPRelease) ->
    Priv = code:priv_dir(rebar3_ex_doc),
    File = "ex_doc_otp_" ++ integer_to_list(OTPRelease),
    PrivFile = filename:join(Priv, File),
    Exists = filelib:is_regular(PrivFile),
    Exists andalso {true, PrivFile}.

win32_ex_doc_script(Path) ->
   "escript.exe " ++ filename:nativename(Path).

-spec ex_doc_config_file(rebar_app_info:t(), file:filename()) -> file:filename().
ex_doc_config_file(App, EdocOutDir) ->
    case  get_exdoc_opts(App) of
        {config_file, FilePath} ->
            FilePath;
        Opts ->
            ExDocOpts = to_ex_doc_format(Opts),
            ExDocConfigFile = filename:join([EdocOutDir, "docs.config"]),
            ok = write_config(ExDocConfigFile, ExDocOpts),
            ExDocConfigFile
    end.

get_exdoc_opts(App) ->
    case rebar_app_info:get(App, ex_doc, []) of
        ExDocOpts0 when ExDocOpts0 =:= [] orelse is_tuple(hd(ExDocOpts0)) orelse is_atom(hd(ExDocOpts0)) ->
            ex_doc_opts_defaults(ExDocOpts0);
        ExDocConfigFile ->
            {config_file, ExDocConfigFile}
    end.

get_exdoc_opt(App, OptName) ->
    case get_exdoc_opts(App) of
        {config_file, _} ->
            undefined;
        Opts ->
            proplists:get_value(OptName, Opts, undefined)
    end.

to_ex_doc_format(ExDocOpts0) ->
    ExDocOpts = lists:keysort(1, ExDocOpts0),
    lists:foldl(
        fun ({api_reference = K, APIReference}, Opts) ->
                [{K, APIReference} | Opts];
            ({assets = K, Assets}, Opts) ->
                [{K, to_binary(Assets)} | Opts];
            ({extras = K, Extras}, Opts) ->
                [{K, to_ex_doc_format_extras(Extras)} | Opts];
            ({homepage_url = K, HomepageURL}, Opts) ->
                [{K, to_binary(HomepageURL)} | Opts];
            ({main = K, Main}, Opts) ->
                FilenameNoExt = filename:rootname(Main),
                [{K, to_lower_binary(FilenameNoExt)} | Opts];
            ({source_url = K, SourceURL}, Opts) ->
                [{K, to_binary(SourceURL)} | Opts];
            ({proglang, _} = V, Opts) -> % internal exception
                [V | Opts];
            ({logo, _} = V, Opts) ->
                [V | Opts];
            ({before_closing_body_tag, _} = V, Opts) ->
                [V | Opts];
            ({before_closing_head_tag, _} = V, Opts) ->
                [V | Opts];
            ({skip_undefined_reference_warnings_on = K, Skips0}, Opts) ->
                Skips = [to_binary(Skip) || Skip <- Skips0],
                [{K, Skips} | Opts];
            ({output = K, OutputDir}, Opts) ->
                [{K, to_binary(OutputDir)} | Opts];
            ({prefix_ref_vsn_with_v, _}, Opts) ->
                Opts;
            ({with_mermaid, Vsn}, Opts) ->
                Key = before_closing_body_tag,
                Existing = proplists:get_value(Key, Opts),
                Mermaid = case mermaid_add(Vsn) of
                            "" -> undefined;
                            [{Key,MermaidMap}] -> MermaidMap
                          end,
                New = merge_before_closing_body_tag(Mermaid, Existing),
                lists:keyreplace(Key, 1, Opts, {Key, New});
            (OtherOpt, Opts) ->
                rebar_api:warn("unknown ex_doc option ~p", [OtherOpt]),
                [OtherOpt | Opts]
        end,
        [],
        ExDocOpts
    ).

to_ex_doc_format_extras(Extras0) ->
    lists:foldr(
        fun ({Extra, ExtraOpts}, Extras) ->
                [{to_atom(Extra), to_ex_doc_format_extras_opts(ExtraOpts)} | Extras];
            (Extra, Extras) when is_list(Extra) ->
                [to_binary(Extra) | Extras];
            (OtherExtra, Extras) -> % unknown: leaving as is
                [OtherExtra | Extras]
        end,
        [],
        Extras0
    ).

to_ex_doc_format_extras_opts(Extras) ->
    maps:map(
        fun (filename, Filename) ->
                to_binary(Filename);
            (title, Title) ->
                to_binary(Title);
            (Key, Value) ->
                rebar_api:warn("unknown ex_doc.extras option ~p := ~p", [Key, Value]),
                Value
        end,
        Extras
    ).

to_binary(Term) when is_list(Term) ->
    list_to_binary(Term);
to_binary(Term) when is_atom(Term) ->
    atom_to_binary(Term, latin1);
to_binary(Term) -> % unknown: leaving as is
    Term.

to_lower_binary(Term) when is_binary(Term) ->
    to_lower_binary(binary_to_list(Term));
to_lower_binary(Term) when is_list(Term) ->
    list_to_binary(string:to_lower(Term)).

to_atom(Term) when is_list(Term) ->
    list_to_atom(Term);
to_atom(Term) -> % unknown: leaving as is
    Term.

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

-spec output_dir(any(), file:filename()) -> file:filename().
output_dir(App, Dir) ->
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
    "Specify which app to generate docs for within an umbrella project.";
help(ex_doc) ->
    "Specify a path to an alternate version of ex_doc on your system (e.g., /path/to/ex_doc).";
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

% Mermaid support
mermaid_add(false) ->
  "";
mermaid_add(true) ->
  mermaid_add(?DEFAULT_MERMAID_VSN);
mermaid_add(Vsn) when is_list(Vsn) ->
    [{before_closing_body_tag, #{
        html => "<script src=\"https://cdn.jsdelivr.net/npm/mermaid@" ++ Vsn ++ "/dist/mermaid.min.js\"></script>                <script>
                   document.addEventListener(\"DOMContentLoaded\", function () {
                    mermaid.initialize({
                       startOnLoad: false,
                       theme: document.body.className.includes(\"dark\") ? \"dark\" : \"default\"
                     });
                     let id = 0;
                     for (const codeEl of document.querySelectorAll(\"pre code.mermaid\")) {
                       const preEl = codeEl.parentElement;
                       const graphDefinition = codeEl.textContent;
                       const graphEl = document.createElement(\"div\");
                       const graphId = \"mermaid-graph-\" + id++;
                       mermaid.render(graphId, graphDefinition).then(({svg, bindFunctions}) => {
                         graphEl.innerHTML = svg;
                         bindFunctions?.(graphEl);
                         preEl.insertAdjacentElement(\"afterend\", graphEl);
                         preEl.remove();
                       });
                     }
                   });
                 </script>"
    }}];
mermaid_add(Vsn) ->
    ?RAISE({mermaid_vsn_not_string, Vsn}).

merge_before_closing_body_tag(MermaidMap, Custom) ->
    try
        do_merge_before_closing_body_tag(MermaidMap, Custom)
    catch
        _:_:_ ->
            ?RAISE({invalid_before_closing_body_tag, Custom})
    end.

do_merge_before_closing_body_tag(undefined, Custom) ->
    Custom;
do_merge_before_closing_body_tag(MermaidMap, undefined) ->
    MermaidMap;
do_merge_before_closing_body_tag(MermaidMap, {M,F,Args}) 
        when is_atom(M) and is_atom(F) and is_list(Args) ->
    CustomMap = #{html => erlang:apply(M, F, [html|Args]),
                  epub => erlang:apply(M, F, [epub|Args])},
    do_merge_before_closing_body_tag(MermaidMap, CustomMap);
do_merge_before_closing_body_tag(MermaidMap, CustomFun)
        when is_function(CustomFun, 1) ->
    CustomMap = #{html => CustomFun(html),
                  epub => CustomFun(epub)},
    do_merge_before_closing_body_tag(MermaidMap, CustomMap);
do_merge_before_closing_body_tag(MermaidMap, CustomMap) ->
    maps:merge_with(fun (html,Mermaid,Custom) ->
                        Mermaid ++ Custom;
                        (_,_,V2) ->
                            V2
                    end,
                    MermaidMap,
                    CustomMap).