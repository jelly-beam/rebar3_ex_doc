# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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

## v0.2.5 [2022-01-12]

- Fixed a bug that prevented generating docs on windows thanks to @cw789 for
  raising the issue and testing the patch (this was actually released in 0.2.4,
  but noting here).
- Fixed a bug where by we were not passing the package name to `ex_doc`
- Fixed a bug where by `edoc` opts along with `ex_doc_opts` might cause beam chunks
  to not be emitted (this was actually fixed in 0.2.3, but noting here). Thanks
  to @Taure for raising the issue.

## v0.2.0 [2021-12-21]

- Initial public release.
