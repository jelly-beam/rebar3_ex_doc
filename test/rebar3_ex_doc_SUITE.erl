-module(rebar3_ex_doc_SUITE).

-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        generate_docs,
        generate_docs_alternate_rebar3_config_format,
        generate_docs_with_current_app_set,
        generate_docs_with_bad_config,
        generate_docs_with_alternate_ex_doc,
        format_errors
    ].

init_per_suite(Config) ->
    {ok, Cwd} = file:get_cwd(),
    ok = file:set_cwd("../../../.."),
    {ok, _} = rebar_utils:sh("mix do deps.get, escript.build", [
        {return_on_error, true}
    ]),
    file:set_cwd(Cwd),
    Config.

end_per_suite(Config) ->
    Config.

generate_docs(Config) ->
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
    {State, App} = make_stub(StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App).

generate_docs_alternate_rebar3_config_format(Config) ->
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
    {State, App} = make_stub(StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App).

generate_docs_with_alternate_ex_doc(Config) ->
    Priv = code:priv_dir(rebar3_ex_doc),
    Default = filename:join(Priv, "ex_doc"),
    Alt = filename:join(data_dir(Config), "ex_doc"),
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
    {State, App} = make_stub(StubConfig),

    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State),
    check_docs(App),

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
    {State1, App1} = make_stub(StubConfig1),

    ok = make_readme(App1),
    ok = make_license(App1),
    ?assertError({error,{rebar3_ex_doc,{invalid_ex_doc_path,"path/to"}}}, rebar3_ex_doc:do(State1)).

generate_docs_with_current_app_set(Config) ->
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
    {State, App} = make_stub(StubConfig),
    State1 = rebar_state:current_app(State, App),
    ok = make_readme(App),
    ok = make_license(App),
    {ok, _} = rebar3_ex_doc:do(State1),
    check_docs(App).

generate_docs_with_bad_config(Config) ->
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
    {State, App} = make_stub(StubConfig),
    ok = make_readme(App),
    ok = make_license(App),
    ?assertError({error, {rebar3_ex_doc, {ex_doc, _}}}, rebar3_ex_doc:do(State)).

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

check_docs(App) ->
    AppDir = rebar_app_info:dir(App),
    AppName = rebar_app_info:name(App),
    AppNameStr = rebar_utils:to_list(AppName),
    DocDir = filename:join(AppDir, "doc"),
    {ok, IndexDoc} = file:read_file(filename:join(DocDir, "index.html")),
    {ok, ModuleDoc} = file:read_file(filename:join(DocDir, AppNameStr ++ ".html")),
    {ok, ReadMeDoc} = file:read_file(filename:join(DocDir, "readme.html")),
    {ok, [_ | _]} = zip:unzip(filename:join(DocDir, AppNameStr ++ ".epub"), [{cwd, DocDir}]),
    {ok, EpubReadMe} = file:read_file(filename:join(DocDir, "OEBPS/readme.xhtml")),
    ?assertMatch({match, [AppName]}, re:run(IndexDoc, AppName, [{capture, [0], binary}])),
    ?assertMatch(
        {match, [<<"PLEASE READ ME">>]},
        re:run(ReadMeDoc, "PLEASE READ ME", [{capture, [0], binary}])
    ),
    ?assertMatch(
        {match, [<<"foo/0 does nothing">>]},
        re:run(ModuleDoc, "foo/0 does nothing", [{capture, [0], binary}])
    ),
    ?assertMatch({match, [AppName]}, re:run(EpubReadMe, AppName, [{capture, [0], binary}])),
    ?assertMatch(
        {match, [<<"PLEASE READ ME">>]},
        re:run(EpubReadMe, "PLEASE READ ME", [{capture, [0], binary}])
    ).

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

make_stub(#{name := Name, dir := Dir} = StubConfig) ->
    AppDir = filename:join(Dir, [Name]),
    mkdir_p(AppDir),

    _SrcFile = write_src_file(AppDir, StubConfig),
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

write_src_file(Dir, #{name := Name}) ->
    Erl = filename:join([Dir, "src", Name ++ ".erl"]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, erl_src_file(Name)).

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

erl_src_file(Name) ->
    io_lib:format(
        "%%%-------------------------------------------------------------------\n"
        "%% @doc `~s' - a module\n"
        "%% end\n"
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
