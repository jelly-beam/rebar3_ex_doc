# rebar3_ex_doc

rebar3_ex_doc is a rebar3 plugin to generate documentation using [ex_doc](https://github.com/elixir-lang/ex_doc). This
plugin ships with ex_doc as an escript, thus you don't need to worry about having Elixir installed nor compiling
the ex_doc depdencies. 

This plugin can be used to generate documentation on its own and integrates with [rebar3_hex](https://github.com/erlef/rebar3_hex) for publishing documentation to go along with your packages on [hex.pm](https://hex.pm/). 

## Installation

### As a global plugin 

Simply add `rebar3_ex_doc` to the plugins section in your global `rebar3.config` 

```erlang 
{plugins, [rebar3_ex_doc]}.
```

### In your rebar3 project 

You can add this plugin to your rebar3 project as a project plugin, like so: 

```erlang 
{project_plugins, [rebar3_ex_doc]}.
```

## Setup 

This plugin should work out of the box, but the following configuration is recommended when publishing documentation to
hex.pm : 

```
{ex_doc, [
     {source_url, <<"https://github.com/namespace/your_app">>},
     {extras, [<<"README.md">>, <<"LICENSE">>]},
     {main, <<"readme">>},
     {proglang, erlang}
]}.
```

Please see the [ex_doc configuration documentation](https://hexdocs.pm/ex_doc/Mix.Tasks.Docs.html#module-configuration)
for a complete overview of available configuration directives. 

### Umbrella support

Umbrellas are supported but they must be configured on an app by app basis. This is to avoid publishing documentation to
hex.pm with wrong source urls, logos, etc. 

Specifically, for each app you wish to generate documentation for and more importantly publish to hex.pm one should place a rebar.config in each app directory with the desired configuration for that application.

## Usage 

After the plugin has been added to your project you can simplly run `rebar3 ex_doc` to generate your docs and view them
in your favorite browser.

### Integrating with rebar3_hex 

We highly recommend using `rebar3_ex_doc` as the documentation provider for all your hex projects to provide the users
of our ecosystem with a consistent documentation format and style.

To integrate with rebar3_hex merely specify `rebar3_ex_doc` as the doc provider in your projects 
hex configuration in `rebar.config` : 

```erlang
{hex, [
    {doc, #{provider => ex_doc}}
]}.
```

# Development

If you'd like to hack on this plugin follow the steps below : 

1. You must have at least Elixir 1.13.0 installed to build the ex_doc escript
2. Make a new rebar project or go into an existing project
3. Make a `_checkouts` dir 
4. cd `_checkouts` and clone this repository 
5. Run `mix escript.build` 
6. cd back to the root of your project 
7. Add `rebar3_ex_doc` as plugin to your rebar.config 
8. Run `rebar3 ex_doc` as an initial test
