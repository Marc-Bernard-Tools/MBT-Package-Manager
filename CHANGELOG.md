# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).
The project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
and [ISO Date Format](https://www.iso.org/iso-8601-date-and-time-format.html).

## [Unreleased]

## Version [1.4.4] - 2024-02-15

### Changed

- Add link to MBT account on license page
- Update installer
- Added repository name and version constant

### Fixed

- Fall back "last update: never" to "install time"

## Version [1.4.3] - 2023-12-11

### Changed

- Update certificate for marcbernardtools.com
- Bump ABAP String Map 
- Open links to website in new browser window

### Fixed

- Update installer
- Fix sync between registry and installer persistence

## Version [1.4.2] - 2023-11-24

### Fixed

- Set version in metadata and coding

## Version [1.4.1] - 2023-11-19

### Changed

- Bump AJSON to 1.1.9
- Bump ABAP Logger 

### Fixed

- Fix dump when starting MBT Package Manager

## Version [1.4.0] - 2023-05-22

### Changed

- Switch from MBT Base to MBT Package Manager
- Updated kernel properties

## Version [1.3.2] - 2023-04-14

### Added

- Add DB properties
- Add WEEKDAY system property

### Changed

- Bump AJSON to 1.1.8
- Bump ABAP Logger 

### Fixed

- Fix license check and expiration
- Fix version check if no license is provided
- Fix Tool Manager sync

## Version [1.3.1] - 2022-10-18

### Added

- Contribution guidelines

### Changed

- Changed license from GPL3-or-later to GPL3-only
- Updated metadata to the latest abapGit format
- Renamed screen elements

## Version [1.3.0] - 2022-05-30

### Added

- Added support for Access Passes. You can now enter a single Access Pass license which will be valid for all tools.
- Added Contributor Covenant Code of Conduct
- Added option to set Authorization header for HTTP requests
- Added current codepage and endian settings to utility class (used by MBT Command Field)

### Changed

- Updated REUSE license info and compliance check
- Enhanced Readme 
- Updated linting rules
- Adjusted support tool for Access Passes
- Enhanced support tool to create online or offline repositories for installed tools in abapGit
- Improved check for HTTP return codes 201 and 426
- Move license key maintenance from Tools to Admin menu
- Refactor APHP deserializer
- Bump AJSON to 1.1.4
- Bump ABAP Logger 

### Fixed

- Fixed detection of the license expiration date
- Fixed deletion of marcbernardtools.com certificate

## Version [1.2.0] - 2021-12-08

Public Release

## Version [1.1.0] - 2021-04-13

Launch Release

## Version [1.0.0] - 2021-03-16

Initial Release. For details, see [Features](https://marcbernardtools.com/docs/marc-bernard-tools/features).


[Unreleased]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.4.4...main
[1.4.4]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.4.3...1.4.4
[1.4.3]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.4.2...1.4.3
[1.4.2]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.4.1...1.4.2
[1.4.1]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.4.0...1.4.1
[1.4.0]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.3.2...1.4.0
[1.3.2]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.3.1...1.3.2
[1.3.1]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.3.0...1.3.1
[1.3.0]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.2.0...1.3.0
[1.2.0]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.1.0...1.2.0
[1.1.0]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/compare/1.0.0...1.1.0
[1.0.0]: https://github.com/Marc-Bernard-Tools/MBT-Package-Manager/releases/tag/1.0.0
