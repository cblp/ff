# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0)
and this project adheres to
[Compatible Versioning](https://github.com/staltz/comver).

## [Unreleased]
### Added
- ff:
  - Search within archived contacts and tasks.

## [0.11] - 2019-03-02
### Added
- ff-qt:
  - Link to tracked task. Clickable. With context menu to copy link.
  - Load tasks asynchronously.
  - Update tasks asynchronously.
  - "Done and archive" button to task widget.

### Changed
- ff-qt:
  - UI is rewritten in real C++.
  - Sort tasks in agenda naturally.
    (See [NaturalTaskOrder](doc/NaturalTaskOrder.md))
- Target Haskell version is now GHC 8.6

### Fixed
- ff-qt:
  - Enable threading.

## [0.10] - 2019-02-05
### Changed
- ff-qt: Moved task actions menu button to the left.

### Fixed
- `ff edit` options parsing.

## [0.9] - 2019-01-09
### Added
- CLI: allow multiple notes in subcommands `delete`, `done`, `edit`, `postpone`,
  `show`, `unarchive`.
- Restored `ff-qt` -- experimental Qt GUI -- with minimal set of features.
  - Show agenda.
  - Postpone.
- Added сolorized output.

### Changed
- Show useful message when nothing to show.
- Use modern prettyprinter.

### Removed
- `ff-serve`

## [0.8] - 2018-12-15
### Added
- `ff show` command to show a specific note.
- Option to specify the work directory.
- External task tracking: support ssh URLs to GitHub.

### Changed
- UI: display UUIDs without quotes.

### Fixed
- Restore missed `ff add` command.

## [0.7] - 2018-11-03
### Added
- Track issue content
- Wiki notes
- Completion of ids
- Contacts
- Command to show wiki notes
- Search among tasks, wiki, and/or contacts

### Changed
- Now using RON as the primary format

## [0.6] - 2018-08-07
### Added
- Tracking of GitHub issues
  - Prohibit editing text and status of tracked notes, allow editing dates
- Option to view notes briefly and compactly (`--brief`)
- Web UI (command `ff serve`)
- `ff upgrade` -- tool to upgrade database format.
  Notes are always written in the latest format on save.
  This command upgrades formats of all documents in database to the latest.

### Fixed
- Treat document without versions as non-existent
- Limit retrying on reading

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

[Unreleased]: https://github.com/ff-notes/ff/compare/v0.11...HEAD
[0.11]: https://github.com/ff-notes/ff/compare/v0.10...v0.11
[0.10]: https://github.com/ff-notes/ff/compare/v0.9...v0.10
[0.9]: https://github.com/ff-notes/ff/compare/v0.8...v0.9
[0.8]: https://github.com/ff-notes/ff/compare/v0.7...v0.8
[0.7]: https://github.com/ff-notes/ff/compare/v0.6...v0.7
[0.6]: https://github.com/ff-notes/ff/compare/v0.5...v0.6
[0.5]: https://github.com/ff-notes/ff/compare/v0.4...v0.5
[0.4]: https://github.com/ff-notes/ff/compare/v0.3...v0.4
[0.3]: https://github.com/ff-notes/ff/compare/v0.2...v0.3
[0.2]: https://github.com/ff-notes/ff/compare/0.1...v0.2
[0.1]: https://github.com/ff-notes/ff/tree/0.1
