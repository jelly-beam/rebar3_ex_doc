-module(rebar3_ex_doc_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        mermaid_before_before_closing_body_tag,
        generate_docs,
        generate_docs_alternate_rebar3_config_format,
        generate_docs_with_current_app_set,
        generate_docs_with_bad_config,
        generate_docs_with_alternate_ex_doc,
        generate_docs_with_output_set_in_config,
        generate_docs_overriding_output_set_in_config,
        format_errors
    ] ++ all_post_27(list_to_integer(erlang:system_info(otp_release))).

all_post_27(OTPRelease) when OTPRelease >= 27 ->
    [
        mermaid_before_before_closing_body_tag_post_27,
        generate_docs_post_27,
        generate_docs_alternate_rebar3_config_format_post_27,
        generate_docs_without_extra_post_27,
        generate_docs_with_assets_post_27,
        generate_docs_with_current_app_set_post_27,
        generate_docs_with_bad_config_post_27,
        generate_docs_with_alternate_ex_doc_post_27,
        generate_docs_with_output_set_in_config_post_27,
        generate_docs_overriding_output_set_in_config_post_27,
        %% The following #123 regression tests are intentionally post-27-only
        %% (no _post_27 sibling): they exercise native -moduledoc/-doc, which
        %% only exists on OTP >= 27.
        generate_docs_native_docs_survive_edoc_backtick_crash,
        generate_docs_legacy_docs_still_abort_on_edoc_backtick_crash
    ];
all_post_27(_OTPRelease) ->
    [].

init_per_suite(Config) ->
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd("../../../.."),
    {ok, _} = rebar_utils:sh("mix do archive.install github hexpm/hex --force, deps.get, escript.build", [
        {return_on_error, true}
    ]),
    file:set_cwd(Cwd),
    Config.

end_per_suite(Config) ->
    Config.

mermaid_before_before_closing_body_tag_post_27(Config) ->
    mermaid_before_before_closing_body_tag([{post_27, true} | Config]).

generate_docs_post_27(Config) ->
    generate_docs([{post_27, true} | Config]).

generate_docs_alternate_rebar3_config_format_post_27(Config) ->
    generate_docs_alternate_rebar3_config_format([{post_27, true} | Config]).

generate_docs_with_current_app_set_post_27(Config) ->
    generate_docs_with_current_app_set([{post_27, true} | Config]).

generate_docs_with_bad_config_post_27(Config) ->
    generate_docs_with_bad_config([{post_27, true} | Config]).

generate_docs_with_alternate_ex_doc_post_27(Config) ->
    generate_docs_with_alternate_ex_doc([{post_27, true} | Config]).

generate_docs_with_output_set_in_config_post_27(Config) ->
    generate_docs_with_output_set_in_config([{post_27, true} | Config]).

generate_docs_without_extra_post_27(Config) ->
    generate_docs_without_extra([{post_27, true} | Config]).

generate_docs_overriding_output_set_in_config_post_27(Config) ->
    generate_docs_overriding_output_set_in_config([{post_27, true} | Config]).

generate_docs_with_assets_post_27(Config) ->
    generate_docs_with_assets([{post_27, true} | Config]).

generate_docs(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "default_docs",
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_alternate_rebar3_config_format(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "default_docs",
        config =>
            {ex_doc, [
                {main, "README.md"},
                {extras, [
                    "README.md",
                    {"LICENSE", #{
                        filename => "LICENSE.md",
                        title => "License"
                    }
                }]}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_without_extra(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "no_extra_docs",
        config => {ex_doc,[]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_with_assets(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    Assets = #{"src" => "erlang_source"},
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name =>  "assets_map_docs",
        config =>
            {ex_doc,[{assets, Assets}]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_with_alternate_ex_doc(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    Priv = code:priv_dir(rebar3_ex_doc),
    ExDoc = "ex_doc_otp_" ++ erlang:system_info(otp_release),
    Default = filename:join(Priv, ExDoc),
    Alt = filename:join(data_dir(Config), ExDoc),
    {ok, _} = file:copy(Default, Alt),
    file:change_mode(Alt, 8#00700),

    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "alternate_ex_doc",
        args => "-e " ++ Alt,
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig),

    StubConfig1 = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "alternate_ex_doc_bad_path",
        args => "-e path/to",
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>}
            ]}
    },
    {State1, App1} = make_stub(Post27, StubConfig1),

    ok = make_readme(App1),
    ok = make_license(App1),
    ?assertError({error,{rebar3_ex_doc,{invalid_ex_doc_path,"path/to"}}}, rebar3_ex_doc:do(State1)).

generate_docs_with_current_app_set(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "current_app",
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),
    State1 = rebar_state:current_app(State, App),
    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State1),
    check_docs(App, State, StubConfig).

generate_docs_with_bad_config(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "default_docs1",
        config =>
            {ex_doc, [
                {source_url, {"https://github.com/eh/eh", 2}},
                {extras, ["README.md", "LICENSE"]},
                {main, "readme"}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),
    ok = make_readme(App),
    ok = make_license(App),
    ?assertError({error, {rebar3_ex_doc, {ex_doc, _}}}, rebar3_ex_doc:do(State)).

generate_docs_with_output_set_in_config(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "output_set_in_config_docs",
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>},
                {output, "foo_docs"}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_overriding_output_set_in_config(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        args => "--output bar_docs",
        name => "override_output_set_in_config_docs",
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>},
                {output, "foo_docs"}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_native_docs_survive_edoc_backtick_crash(Config) ->
    %% Regression for #123: a native -doc project with a Markdown backtick in a
    %% %% comment attached to a declaration makes edoc:get_doc/2 throw. Because
    %% the app ships populated native Docs chunks in its beams, doc generation
    %% must NOT abort: the plugin logs the edoc error and lets ex_doc render
    %% straight from the beam. No special config required.
    %%
    %% Coverage note: this asserts the GRACEFUL path ({ok,_} + rendered docs).
    %% Its sibling generate_docs_legacy_docs_still_abort_on_edoc_backtick_crash
    %% asserts the SAME edoc crash still aborts a legacy project, which pins the
    %% precondition that the type-attached backtick really throws under edoc. If
    %% a future OTP stops throwing, that sibling fails loudly, so this test
    %% cannot silently pass via the {ok,State3} edoc-success arm unnoticed.
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "native_backtick_comment",
        src_variant => backtick_comment,
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>}
            ]}
    },
    %% post_27 forced true: native -moduledoc/-doc only exists on OTP >= 27.
    {State, App} = make_stub(true, StubConfig),
    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App, State, StubConfig).

generate_docs_legacy_docs_still_abort_on_edoc_backtick_crash(Config) ->
    %% Backward-compat guard for #123: a LEGACY (@doc, no -moduledoc/-doc)
    %% project that hits the same edoc backtick crash must STILL fail fast.
    %% has_native_docs/1 returns false (the beam carries no populated Docs
    %% chunk), so gen_chunks/2 re-raises ?RAISE({gen_chunks, _}) exactly as
    %% before this fix. This locks down the has_native_docs=false -> abort
    %% branch so a future regression that loosened native-docs detection (e.g.
    %% treating missing_chunk or opt-out 'hidden' docs as populated) would be
    %% caught here instead of silently downgrading a real edoc failure to a
    %% warning for legacy projects.
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => "legacy_backtick_comment",
        src_variant => legacy_backtick_comment,
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>}
            ]}
    },
    {State, App} = make_stub(true, StubConfig),
    ok = make_readme(App),
    ok = make_license(App),
    %% ?RAISE wraps the reason as {error, {?MODULE, Reason}} (the ?PRV_ERROR
    %% shape rebar3 expects from a provider), so erlang:error/1 carries that
    %% full term. Matching it confirms the legacy abort path (?RAISE) was hit.
    ?assertError(
        {error, {rebar3_ex_doc, {gen_chunks, _}}},
        rebar3_ex_doc:do(State)
    ).

format_errors(_) ->
    Err = "The app 'foo' specified was not found.",
    ?assertEqual(Err, rebar3_ex_doc:format_error({app_not_found, foo})),

    Err1 =
        "An unknown error occurred generating doc chunks with edoc. Run with DIAGNOSTICS=1 for more details.",
    ?assertEqual(Err1, rebar3_ex_doc:format_error({gen_chunks, some_error})),

    Err2 = "An unknown error occurred compiling apps. Run with DIAGNOSTICS=1 for more details.",
    ?assertEqual(Err2, rebar3_ex_doc:format_error({compile, some_error})),

    Err3 =
        "An unknown error occurred generating docs config. Run with DIAGNOSTICS=1 for more details.",
    ?assertEqual(Err3, rebar3_ex_doc:format_error({write_config, some_error})),

    Err4 = "",
    ?assertEqual(Err4, rebar3_ex_doc:format_error({ex_doc, abort})),

    Err5 = "An unknown error has occurred. Run with DIAGNOSTICS=1 for more details.",
    ?assertEqual(Err5, rebar3_ex_doc:format_error({eh, some_error})).

%% Check that:
%%      1. mermaid is included
%%      2. before_closing_body_tag is included
%%      3. mermaid comes before before_closing_body_tag
%% in main file and in module docs
mermaid_before_before_closing_body_tag(Config) ->
    Post27 = proplists:get_value(post_27, Config, false),
    AppNameStr = "default_docs",
    StubConfig = #{
        app_src => #{version => "0.1.0"},
        dir => data_dir(Config),
        name => AppNameStr,
        config =>
            {ex_doc, [
                {source_url, <<"https://github.com/eh/eh">>},
                {extras, [<<"README.md">>, <<"LICENSE">>]},
                {main, <<"readme">>},
                {with_mermaid, true},
                {before_closing_body_tag, #{html => "<div id=\"custom_html\"></div>"}}
            ]}
    },
    {State, App} = make_stub(Post27, StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),

    AppDir = rebar_app_info:dir(App),
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    #{config := {ex_doc, DocConfig}} = StubConfig,
    DocDir = filename:join(AppDir, get_doc_dir(Opts, DocConfig)),
    {ok, ModuleDoc} = file:read_file(filename:join(DocDir, AppNameStr ++ ".html")),
    {ok, ReadMeDoc} = file:read_file(filename:join(DocDir, "readme.html")),

    %% Mermaid CDN followed by init script
    MermaidRE = "<script src=\"https://cdn.jsdelivr.net/npm/mermaid@.*/dist/mermaid.min.js\">(?s).*</script>(?s).*<script>(?s).*</script>",
    #{html := BeforeCloseBodyTagRE} = proplists:get_value(before_closing_body_tag, DocConfig),

    lists:foreach(fun (Doc) ->
                        {match, [{StartMermaid,_}]} = re:run(Doc, MermaidRE, []),
                        {match, [{StartBeforeCloseBodyTag,_}]} = re:run(Doc, BeforeCloseBodyTagRE, []),
                        ?assert(StartMermaid < StartBeforeCloseBodyTag)
                    end,
                    [ReadMeDoc, ModuleDoc]).

check_docs(App, State, #{config := {ex_doc, DocConfig}} = _Stub) ->
    Extras = format_extras(proplists:get_value(extras, DocConfig, [])),
    Assets = proplists:get_value(assets, DocConfig),
    AppDir = rebar_app_info:dir(App),
    BuildDir = filename:join(AppDir, "_build"),
    {ok, ConfigFile} = file:consult(filename:join([BuildDir, "default/lib/", rebar_app_info:name(App), "doc/docs.config"])),
    ExpExtras = proplists:get_value(extras, ConfigFile, []),
    ?assertMatch(Extras, ExpExtras),
    AppName = rebar_app_info:name(App),
    AppNameStr = rebar_utils:to_list(AppName),
    {Opts, _Args} = rebar_state:command_parsed_args(State),
    DocDir = filename:join(AppDir, get_doc_dir(Opts, DocConfig)),
    {ok, IndexDoc} = file:read_file(filename:join(DocDir, "index.html")),
    ?assertMatch({match, [AppName]}, re:run(IndexDoc, AppName, [{capture, [0], binary}])),
    {ok, ModuleDoc} = file:read_file(filename:join(DocDir, AppNameStr ++ ".html")),
    ?assertMatch(
        {match, [<<"foo/0 does nothing">>]},
        re:run(ModuleDoc, "foo/0 does nothing", [{capture, [0], binary}])
    ),
    check_readme(Extras, DocDir),
    check_epub(Extras, DocDir, AppName, AppNameStr),
    check_assets(Assets, DocDir, filename:join([BuildDir, "default/lib/", rebar_app_info:name(App)])).

check_readme(Extras, DocDir) ->
    case lists:member(<<"README.md">>, Extras) of
        false -> ok;
        true ->
            {ok, ReadMe} = file:read_file(filename:join(DocDir, "readme.html")),
            ?assertMatch(
                {match, [<<"PLEASE READ ME">>]},
                re:run(ReadMe, "PLEASE READ ME", [{capture, [0], binary}])
            )
    end.

check_epub(Extras, DocDir, AppName, AppNameStr) ->
    {ok, [_ | _]} = zip:unzip(filename:join(DocDir, AppNameStr ++ ".epub"), [{cwd, DocDir}]),
    case lists:member(<<"README.md">>, Extras) of
        false -> ok;
        true ->
           {ok, EpubReadMe} = file:read_file(filename:join(DocDir, "OEBPS/readme.xhtml")),
            ?assertMatch({match, [AppName]}, re:run(EpubReadMe, AppName, [{capture, [0], binary}])),
            ?assertMatch(
                {match, [<<"PLEASE READ ME">>]},
                re:run(EpubReadMe, "PLEASE READ ME", [{capture, [0], binary}])
            )
    end.

check_assets(undefined, _DocDir, _Dir) ->
     ok;
check_assets(Assets, DocDir, Dir) when not is_map(Assets) ->
     {ok, ExpectedFiles} = file:list_dir(filename:join(Dir, Assets)),
     {ok, Files} = file:list_dir(filename:join(DocDir, "assets")),
     ?assertEqual(lists:sort(ExpectedFiles), lists:sort(Files));
check_assets(Assets, DocDir, Dir) when is_map(Assets) ->
     %% The semantics of assets is not well defined. It seems that Src and Target
     %% can only be directories and that one cannot copy a specific file this way
     ExpectedFiles =
        maps:fold(fun(Src, Target, Acc) ->
                      case filelib:is_dir(filename:join(Dir, Src)) of
                          true ->
                             {ok, Fs} = file:list_dir(filename:join(Dir, Src)),
                             [filename:join([Target, F]) || F <- Fs] ++ Acc;
                          false ->
                             [Target | Acc ]
                      end
                  end, [], Assets),
     lists:foreach(fun(ExpectedFile) ->
                       {ok, _} = file:read_file(filename:join(DocDir, ExpectedFile))
                   end, ExpectedFiles).

get_doc_dir(Opts, DocConfig) ->
    case proplists:get_value(output, Opts, proplists:get_value(output, DocConfig, "doc")) of
        undefined ->
            "doc";
        Doc ->
            Doc
    end.

format_extras(Extras0) ->
    lists:foldr(
        fun ({Extra, ExtraOpts}, Extras) ->
                [{list_to_atom(Extra), format_extras_opts(ExtraOpts)} | Extras];
            (Extra, Extras) when is_list(Extra) ->
                [list_to_binary(Extra) | Extras];
            (OtherExtra, Extras) ->
                [OtherExtra | Extras]
        end,
        [],
        Extras0
    ).

format_extras_opts(Extras) ->
    maps:to_list(
    maps:map(
        fun (filename, Filename) when is_list(Filename) ->
                list_to_binary(Filename);
            (title, Title) when is_list(Title) ->
                list_to_binary(Title);
            (_Key, Value) ->
                Value
        end,
        Extras
    )).

compile_src_file(App) ->
    Dir = rebar_app_info:dir(App),
    Name = rebar_app_info:name(App),
    Erl = filename:join([Dir, "src", rebar_utils:to_list(Name) ++ ".erl"]),
    Ebin = filename:join(Dir, rebar_app_info:ebin_dir(App)),
    {ok, _, _} = compile:file(Erl, [debug_info, {outdir, Ebin}, return]).

make_readme(App) ->
    file:write_file(filename:join(rebar_app_info:dir(App), "README.md"), <<"# PLEASE READ ME">>).

make_license(App) ->
    file:write_file(filename:join(rebar_app_info:dir(App), "LICENSE"), <<"LICENSE">>).

make_stub(Post27, #{name := Name, dir := Dir} = StubConfig) ->
    AppDir = filename:join(Dir, [Name]),
    mkdir_p(AppDir),

    _SrcFile = write_src_file(Post27, AppDir, StubConfig),
    _AppSrcFile = write_app_src_file(AppDir, StubConfig),
    _ConfigFile = write_config_file(AppDir, StubConfig),
    State = init_state(AppDir, StubConfig),
    [App] = rebar_state:project_apps(State),
    {ok, State0} = rebar_prv_app_discovery:init(State),
    {ok, State1} = rebar_prv_app_discovery:do(State0),
    {ok, State2} = rebar_prv_edoc:init(State1),
    {ok, State3} = rebar_prv_compile:init(State2),
    {ok, State4} = rebar_prv_compile:do(State3),
    {ok, State5} = init_ex_doc(State4, StubConfig),
    compile_src_file(App),
    {State5, App}.

init_ex_doc(State, #{args := Args}) ->
    State1 = rebar_state:command_args(State, Args),
    {ok, State2} = rebar3_ex_doc:init(State1),
    Provider = providers:get_provider_by_module(rebar3_ex_doc, rebar_state:providers(State2)),
    Opts = providers:opts(Provider) ++ rebar3:global_option_spec_list(),
    {ok, Args2} = getopt:parse(Opts, rebar_state:command_args(State2)),
    {ok, rebar_state:command_parsed_args(State2, Args2)};

init_ex_doc(State, _) ->
    rebar3_ex_doc:init(State).

init_state(Dir, Config) ->
    State = rebar_state(Dir, Config),
    LibDirs = rebar_dir:lib_dirs(State),
    rebar_app_discover:do(State, LibDirs).

write_src_file(Post27, Dir, #{name := Name} = StubConfig) ->
    Erl = filename:join([Dir, "src", Name ++ ".erl"]),
    ok = filelib:ensure_dir(Erl),
    Variant = maps:get(src_variant, StubConfig, Post27),
    ok = ec_file:write(Erl, erl_src_file(Variant, Name)).

write_app_src_file(Dir, #{name := Name, app_src := #{version := Vsn}}) ->
    Filename = filename:join([Dir, "src", Name ++ ".app.src"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(Name, Vsn)).

write_config_file(Dir, #{config := Config}) ->
    Filename = filename:join([Dir, "rebar.config"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, Config).

get_app_metadata(Name, Vsn) ->
    {application, list_to_atom(Name), [
        {description, "An OTP application"},
        {vsn, Vsn},
        {registered, []},
        {applications, [kernel, stdlib]},
        {env, []},
        {modules, []},
        {licenses, ["Apache 2.0"]},
        {links, []}
    ]}.

erl_src_file(backtick_comment = _Variant, Name) ->
    %% Native -moduledoc/-doc module with a Markdown backtick (ASCII 0x60) in a
    %% %% comment attached to a -type declaration. This is the exact #123 repro
    %% ("Crashes if type comment has backticks"): with {preprocess,true}
    %% edoc:get_doc/2 throws ("`-quote ended unexpectedly at line N"). The BEAM
    %% still carries a populated EEP-48 Docs chunk (moduledoc + foo/0 doc), so
    %% ex_doc renders from the beam and the graceful-degradation path skips
    %% edoc. Verified empirically on OTP 28: a backtick in a comment attached to
    %% a *function* does NOT throw, but one attached to a *type* does.
    io_lib:format(
        "-module('~s').\n"
        "-moduledoc \"\"\"\n"
        "A module\n"
        "\"\"\".\n"
        "-export([foo/0]).\n"
        "-export_type([t/0]).\n"
        "-type s() :: integer().\n"
        "%% a `backtick in this type comment\n"
        "-type t() :: s().\n"
        "-doc \"\"\"\n"
        "foo/0 does nothing\n"
        "\"\"\".\n"
        "-spec foo() -> t().\n"
        "foo() -> ok.\n",
        [Name]
    );
erl_src_file(legacy_backtick_comment = _Variant, Name) ->
    %% LEGACY (no -moduledoc/-doc) module hitting the SAME #123 edoc crash: a
    %% Markdown backtick in a %% comment attached to a -type declaration makes
    %% edoc:get_doc/2 throw. Unlike the native variant above, this module ships
    %% NO populated EEP-48 Docs chunk (legacy @doc lives in comments, not in the
    %% beam), so has_native_docs/1 returns false and gen_chunks/2 MUST still
    %% abort via ?RAISE({gen_chunks, _}) exactly as before. This pins the
    %% backward-compat contract: legacy projects fail fast on edoc errors, and
    %% it also fails loudly (the abort assertion no longer holds) if a future
    %% OTP changes edoc so the type-attached backtick stops throwing.
    io_lib:format(
        "%%% legacy module\n"
        "-module('~s').\n"
        "-export([foo/0]).\n"
        "-export_type([t/0]).\n"
        "-type s() :: integer().\n"
        "%% a `backtick in this type comment\n"
        "-type t() :: s().\n"
        "%% @doc foo/0 does nothing\n"
        "-spec foo() -> t().\n"
        "foo() -> ok.\n",
        [Name]
    );
erl_src_file(true = _Post27, Name) ->
    io_lib:format(
        "-module('~s').\n"
        "-moduledoc \"\"\"\n"
        "`~s` - a module\n"
        "\"\"\".\n"
        "-export([foo/0]).\n"
        "-doc \"\"\"\n"
        "foo/0 does nothing\n"
        "\"\"\".\n"
        "-spec foo() -> ok.\n"
        "foo() -> ok.\n",
        [Name, Name]
    );
erl_src_file(false = _Post27, Name) ->
    io_lib:format(
        "%%%-------------------------------------------------------------------\n"
        "%% @doc `~s' - a module\n"
        "%% @end\n"
        "%%%-------------------------------------------------------------------\n"
        "-module('~s').\n"
        "-export([foo/0]).\n"
        "%%% @doc\n"
        "%%% foo/0 does nothing\n"
        "%%% @end\n"
        "-spec foo() -> ok.\n"
        "foo() -> ok.\n",
        [Name, Name]
    ).

mkdir_p(Path) ->
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).

rebar_state(AppsDir, #{config := CustomConfig}) ->
    file:set_cwd(AppsDir),
    Config = [
        {dir, AppsDir},
        {current_profiles, [docs]},
        {command_parsed_args, []},
        {resources, []},
        {hex, [{doc, #{provider => ex_doc}}]}
    ],
    Config1 = lists:merge(Config, [CustomConfig]),
    State = rebar_state:new(Config1),
    State.

data_dir(Config) -> ?config(priv_dir, Config).
