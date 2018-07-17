# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
- Add tracking of github issues

## [0.5] - 2018-07-06
### Added
- Subcommand `github` to synchronize issues with GitHub
- Group tasks by due day
- Use pipe as the metadata delimiter to simplify mouse selection
- Unlimited note listing by default
- Use pager for output
- Delete merged version files

## [0.4] - 2018-06-06
### Added
- Option `-V`, `--version` showing version number, git revision and dirtiness
### Fixed
- Fix install instruction in README
- Fix fail when VCS-based dataDir exists and global doesn't

## [0.3] - 2018-04-22
### Added
- Subcommand `unarchive`
- "N task(s) omitted" message
- Shuffle/Sort notes
- Use code repository as a note storage

### Changed
- More compact format to store zeroes in RGA
- Make "To see more tasks" message conditional:
  it's only displayed when there some tasks were hidden due to limit

### Other
- Move ff-related tasks from cblp's personal ff to the project repo

## [0.2] - 2018-02-12
### Added
- Option `--all` to subcommand `agenda`
- Subcommand `config`
  - Subcommand `dir`
- Subcommand `delete`
- Subcommand `postpone`
- Subcommand `search`
- Yandex.Disk detection
- Show ending-soon tasks in Agenda
- Subcommand `edit`
  - Select editor automagically
- Multiline notes
- Start and end dates for notes
- Subcommand `add`/`new`
- Subcommand `delete`
- Clear note content on deletion

### Changed
- Move note id from text to fields
- Prettify multi-line notes
- Separate overdue and today tasks
- Compress ids with Base36
- Agenda:
  - Split with samples
  - Limit to 10 by default
  - Use Mainland-pretty for UI instead of YAML
  - Show notes list as a list with bullets
  - Show start/end dates
  - Sort notes by start
- Use RGA for the note text

## [0.1] - 2018-01-05
### Added
- Executable `ff`
- Subcommand `agenda`
- Subcommand `done`
- Subcommand `new`

[Unreleased]: https://github.com/ff-notes/ff/compare/v0.5...HEAD
[0.5]: https://github.com/ff-notes/ff/compare/v0.4...v0.5
[0.4]: https://github.com/ff-notes/ff/compare/v0.3...v0.4
[0.3]: https://github.com/ff-notes/ff/compare/v0.2...v0.3
[0.2]: https://github.com/ff-notes/ff/compare/0.1...v0.2
[0.1]: https://github.com/ff-notes/ff/tree/0.1
