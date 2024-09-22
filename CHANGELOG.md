# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [v0.2.24]

- Use a more recent Hex version [#101](https://github.com/jelly-beam/rebar3_ex_doc/pull/101)
- Fix broken link [#100](https://github.com/jelly-beam/rebar3_ex_doc/pull/100)
- Sync support windows changes by [#96](ttps://github.com/jelly-beam/rebar3_ex_doc/pull/96)

## [v0.2.33]

- Update to ex_doc 0.32.2

## [v0.2.22]

- Update to ex_doc 0.31.2
- Fix wrong repo. owner and update PR links in changelog by @kianmeng in #75
- Pull pre-build ex_doc from github for releases by @starbelly in #77
- Fix link to example in README.md by @starbelly in #78
- Fix with_mermaid by @MarkoMin in #64
- Execute OTP-specific ex_doc (from priv) by @paulo-ferraz-oliveira in #83

## [v0.2.21]

- Add Elixir script to bump ex_doc version from CLI ([#67](https://github.com/starbelly/rebar3_ex_doc/pull/67))
- Add new workflow to automatically (and periodically) update `ex_doc` version ([#65](https://github.com/starbelly/rebar3_ex_doc/pull/65))
- [automation] Update `ex_doc` to 0.30.6 ([#68](https://github.com/starbelly/rebar3_ex_doc/pull/68))
- [automation] Update `ex_doc` to 0.30.7 ([#69](https://github.com/starbelly/rebar3_ex_doc/pull/69))
- Use include_paths directive in app.src ([#70](https://github.com/starbelly/rebar3_ex_doc/pull/70))

## [v0.2.20]

- Bump ex_doc to 0.30.5
- Minor doc changes (#61)
- Fix rendering issues when mermaid is used ([#59](https://github.com/starbelly/rebar3_ex_doc/pull/59))


## [v0.2.19]

* Fix negation behaviour of output param in config ([#56](https://github.com/starbelly/rebar3_ex_doc/pull/56))
* Update ex_doc to 0.30.3
* Add out-of-the-box support for mermaid ([#51](https://github.com/starbelly/rebar3_ex_doc/pull/51))

## [v0.2.18]

* Update ex_doc to v0.29.4

## [v0.2.17]

* Allow generation of docs outside the context of a package ([#45](https://github.com/starbelly/rebar3_ex_doc/pull/45))
* Provide option to opt of out of prefixing source vsn with v ([#46](https://github.com/starbelly/rebar3_ex_doc/pull/46))

## [v0.2.16]

 -  Add support for external config files

## [v0.2.15]

 - bump ex_doc to 0.29.1
 - Translate 'skip_undefined_reference_warnings_on' ExDoc option
 - Document auto-link syntax for Erlang extras
 - add support for the "output" configuration option

## [v0.2.14]

 - Prevent warnings when passing the "logo" option (#32)([#32](https://github.com/starbelly/rebar3_ex_doc/pull/32))
 - Fix app commandline flag (#31)([#31](https://github.com/starbelly/rebar3_ex_doc/pull/31))
 - bump ex_doc to 0.29.0

## [v0.2.13]

  - bump ex_doc to 0.28.5

## [v0.2.12]

 - fix default edoc opts (#30)([#30](https://github.com/starbelly/rebar3_ex_doc/pull/30))

## v0.2.10 [2022-05-30]

  - Switch foldl to foldr to preserve extra pages order ([#28](https://github.com/starbelly/rebar3_ex_doc/pull/28))
  - gen_chunks: unfold edoc_opts from project rebar.config before merging (#25)([#25](https://github.com/starbelly/rebar3_ex_doc/pull/25))
  - Update development guide ([#24](https://github.com/starbelly/rebar3_ex_doc/pull/24))
  - Increase available options, as per rebar3_ex_doc's own usage ([#23](https://github.com/starbelly/rebar3_ex_doc/pull/23))

## v0.2.9 [2022-04-22]

 - bump ex_doc to 0.28.3
 - merge (from `rebar.config`) configuration `edoc_opts` with `ex_doc`
 - Switch to strings vs binaries for keys while still supporting binaries for backwards compat
 - don't require the user to lowercase the main document name

## v0.2.8 [2022-01-25]

 - v0.2.7 failed to include priv/ex_doc

## v0.2.7 [2022-01-24]

- Add the ebin directories of dependencies to the code path using ex_doc's --paths option.

## v0.2.6 [2022-01-22]

- Update to latest version of ex_doc (0.27.3)
- Added -e/--ex-doc switch to allow specifying an alternative ex_doc escript.
- Added changelog file as centralized release note so existing developer can
  have quick follow-up on recent changes.
- Added a screenshot to showcase the generated HTML output and how embedded
  HTML was used in a Markdown document.
- Added badges for quick overview of the project and provide quick links
  to hex.pm and hexdocs.pm from the README file in GitHub.
- Added a copyright and license section so people are aware of copyright
  owner and license term of this project.
- Replaced the Apache2 license with Markdown version instead of plain text
  version to improve readability.
- Fix misc. Markdown issues to improve readability.
- Fixed a bug where by we were not passing the package name to `ex_doc`

## v0.2.5 [2022-01-11]

- Update to the latest version of ex_doc (0.27.1)

## v0.2.4 [2022-01-10]

- Fixed a bug that prevented generating docs on windows thanks to @cw789 for
  raising the issue and testing the patch (this was actually released in 0.2.4,
  but noting here).
- Add link to github in hex metadata.

## v0.2.3 [2021-12-21]

- Fixed a bug where by `edoc` opts along with `ex_doc_opts` might cause beam chunks
  to not be emitted. Thanks to @Taure for raising the issue.

## v0.2.2 [2021-12-21]

- Be specific in README.md where `ex_doc` configuration should be placed.
- remove rogue "?" from source rev argument.

## v0.2.1 [2021-12-21]

- Make `{proglang, erlang}` the default for ex_doc config.
- Add ourselves (`rebar3_ex_doc`) to hex config for publishing.


## v0.2.0 [2021-12-21]

- Initial public release.
