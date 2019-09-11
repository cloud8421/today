# CHANGELOG

## Unreleased

- Add `reset-ids` command, which resets all task ids, restarting from 1 and
  ordering from oldest to newest. (#14)

## 2.0.4 - 30 Aug 2019

- Fix wrong indentation inside in/out reporters. (#12)

## 2.0.3 - 30 Aug 2019

- Include blank lines inside in/out reporters to improve readability. (#11)

## 2.0.2 - 23 Aug 2019

- Add linebreak after in/out output. (#9)
- Add `--version` option to display version and exit. (#8)

## 2.0.1 - 22 Aug 2019

- Add empty line after task list output. (#6)
- Allow more relaxed formats for refs. (#7)

## 2.0 - 20 Aug 2019

- Uniform subcommands API, renaming delete -> remove, check -> finish. (#5)

## 1.0.1 - 20 Aug 2019

- Use TDFA regex engine, replacing PCRE-based implementation. (#3)

## 1.0 - 19 Aug 2019

- Initial release.
