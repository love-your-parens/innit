# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

## [0.3.0] - 2025-04-10
### Added
- `escape` and `unescape` utility functions
### Changed
- Breaking: INI strings are no longer unescaped automatically upon being decoded.

## [0.2.3] - 2025-04-09
### Added
- escaping comment signs
- quoting parameter names
- multi-line quoting
### Fixed
- continuations breaking after escaped comments
- escape signs sometimes left in the output

## [0.2.2] - 2025-04-02
### Fixed
- Commented-out parameters will now correctly be omitted

## [0.2.1] - 2025-03-26
### Added
- Docs
- Tests

## [0.2.0] - 2025-03-25
### Added
- .ini encoding/writing functionality
- Documentation

## [0.1.2] - 2025-03-24
### Changed
- License changed from EPL 1.0 to MIT
### Added
- Tests
    - Added test runner
    - Added rich comment tests
    - Integrated rich comment tests into the test runner
- Changelog

## [0.1.1] - 2025-03-22
### Changed
- Fleshed out the basic implementation of the parser
### Added
- Parser now supports sections
- Parser now supports multi-line parameters (keys and values alike)

## 0.1.0 - 2025-03-21
### Added
- The project itself
- First prototype of the .ini parser
