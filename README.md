# Today - CLI Task Manager

[![CircleCI](https://circleci.com/gh/cloud8421/today.svg?style=svg&circle-token=b6247f7bf94460ff15e6b85ba4abbf19cfe62f70)](https://circleci.com/gh/cloud8421/today)

Efficiently manage tasks from the command line.

```
  @inbox [1/2]
    1. ✔  Install today 2d
    2. ◻  Learn how to use today 2d

  @work [0/1]
    3. ◻  Finally fix issue TODAY#2345 2d
          | TODAY#2345: https://github.com/cloud8421/today/issues/2345

  33% of all tasks complete.
  1 done · 0 in progress · 2 pending · 0 cancelled
```

## Demo

[![asciicast](https://asciinema.org/a/263518.svg)](https://asciinema.org/a/263518)

## Philosophy

Today heavily borrows from [taskbook](https://github.com/klaussinani/taskbook).

Features:

- Manage tasks and associate them to a specific context.
- Tasks can be marked as cancelled and subsequently deleted.
- Support shorthand references in the task description that get automatically expanded according to URL templates.
- Two commands, `in` and `out`, which output the list of tasks to a format that can be pasted to Slack.
- All data written to a single file, which can be put in Dropbox etc.

## Usage example

1. You can start with `today list` to see the default content.
2. Add a task with `today add "Email triage"`.
3. Add a task in a specific context with `today add --context "Find a solution for issue TODAY#2313"`. Note the `TODAY#2313` reference to service `TODAY` with identifier `2313`, which identifies an issue with that number in this very repo.
4. Add a reference rule for `TODAY#2313`, so that it gets expanded automatically (only needed once per Taskfile) `today set-ref TODAY "https://github.com/cloud8421/today/$id`. Always make sure that you add the `$id` fragment to signal where you want interpolation to happen.
5. Run `today list` and see the expanded reference below the task.
6. Run `today in` for a today message you can paste wherever you like (references are already expanded).
7. Start a task: `today start 2`
8. Finish a task: `today finish 2`
9. Rinse and repeat. At the end of the day, run `today out` for a end of the day message you can paste again wherever you like.
10. Run `today clear` to remove all done and cancelled tasks.

## Installation

Executables for Mac and Linux are available as [releases](https://github.com/cloud8421/today/releases).

Download the file for your architecture and decompress the archive.

Then `chmod +x today` and move the executable somewhere in your `$PATH`.

## Managing the taskfile

As Today saves data in a single file, you can keep that in Dropbox or under version control.

By default, Today searches for a `tasks.json` file in the current directory, creating one if necessary. You can override this behaviour by passing a `-f|--taskfile` flag with the path you want to use or by setting a `TASKFILE` environment variable.

## Command reference

```
TODAY - CLI Task Manager : Efficiently manage tasks from the command line

Usage: today [-f|--taskfile TASKFILE] COMMAND
  TODAY - CLI Task Manager

Available options:
  -h,--help                Show this help text
  -f,--taskfile TASKFILE   The path of the Taskfile to use. Can be replaced by a
                           TASKFILE environment variable, which takes precedence
                           over the option. (default: "./tasks.json")

Task management:
  list                     List all tasks. Can filter by context with
                           --include-context/-i or --exclude-context/-e).
  add                      Add a new task. Requires a text description and
                           optionally takes a context option (--context or -c)
                           to add the task under that context.
  remove                   Removes an existing task by its ID.
  start                    Start an existing task by its ID.
  pause                    Pause an existing task by its ID.
  finish                   Finishes a existing task by its ID, marking it as
                           done.
  cancel                   Cancel an existing task by its ID.
  update                   Update an existing task with a new text description.
  move                     Move an existing task under a new context.
  clear                    Clear all done and cancelled tasks, permanently
                           deleting them from the Taskfile.

Reporters:
  in                       Generate a today message summary, including all
                           pending and in progress tasks. Can filter by context
                           with --include-context/-i or --exclude-context/-e).
  out                      Generate a out for today message summary, including
                           all in progress, done and cancelled tasks. Can filter
                           by context with --include-context/-i or
                           --exclude-context/-e).

Refs management:
  refs                     List all configured ref replacement rules.
  set-ref                  Add a new ref replacement rule
  delete-ref               Delete an existing ref replacement rule by its repo.
```

## Autocompletion

Today can generate autocompletion functions for Bash, ZSH and Fish shells by passing the appropriate flag.

```
# Bash
source <(today --bash-completion-script `which today`)

# ZSH
source <(today --zsh-completion-script `which today`)

# Fish
today --fish-completion-script (which today) | source
```

## Development

Today is written in Haskell and requires [stack](https://www.haskellstack.org).

It supports all standard `stack` commands, e.g. `test`, `run`, `build` or `install`.

After cloning the repo, you can run `stack run -- list`, which will create an
example `tasks.json` file in the project directory and display all current
tasks.
