# T - CLI Task Manager

[![CircleCI](https://circleci.com/gh/cloud8421/t.svg?style=svg&circle-token=b6247f7bf94460ff15e6b85ba4abbf19cfe62f70)](https://circleci.com/gh/cloud8421/t)

Efficiently manage tasks from the command line.

```
  @inbox [1/2]
    1. ✔  Install t 2d
    2. ◻  Learn how to use t 2d

  @work [0/1]
    3. ◻  Finally fix issue T#2345 2d
          | T#2345: https://github.com/cloud8421/t/issues/2345

  33% of all tasks complete.
  1 done · 0 in progress · 2 pending · 0 cancelled
```

![tasklist](/screenshots/list.png)

## Usage

T is a command line task manager that heavily borrows from [taskbook](https://github.com/klaussinani/taskbook).

Features:

- Manage tasks and associate them to a specific context.
- Tasks can be marked as cancelled and subsequently deleted.
- Support shorthand references in the task description that get automatically expanded according to URL templates.
- A `today` command which outputs the list of pending/in-progress tasks to a format that can be pasted to Slack.
- All data written to a single file, which can be put in Dropbox etc.

## Example flow

1. You can start with `t list` to see the default content.
2. Add a task with `t add "Email triage"`.
3. Add a task in a specific context with `t add --context "Find a solution for issue T#2313"`. Note the `T#2313` reference to service `T` with identifier `2313`, which identifies an issue with that number in this very repo.
4. Add a reference rule for `T#2313`, so that it gets expanded automatically (only needed once per Taskfile) `t set-ref T "https://github.com/cloud8421/t/$id`. Always make sure that you add the `$id` fragment to signal where you want interpolation to happen.
5. Run `t list` and see the expanded reference below the task.
6. Run `t today` for a today message you can paste wherever you like (references are already expanded).
7. Start a task: `t start 2`
8. Finish a task: `t check 2`

## Command reference

```
T - CLI Task Manager : Efficiently manage tasks from the command line

Usage: t [-f|--taskfile TASKFILE] COMMAND
  T - CLI Task Manager

Available options:
  -h,--help                Show this help text
  -f,--taskfile TASKFILE   The path of the Taskfile to use. Can be replaced by a
                           TASKFILE environment variable, which takes precedence
                           over the option. (default: "./tasks.json")

Task management:
  list                     List all tasks. Optionally takes a context option
                           (--context or -c) to filter tasks by that context.
  add                      Add a new task. Requires a text description and
                           optionally takes a context option (--context or -c)
                           to add the task under that context.
  delete                   Deletes an existing task by its ID.
  check                    Check an existing task by its ID, marking it as done.
  cancel                   Cancel an existing task by its ID.
  start                    Start an existing task by its ID.
  pause                    Pause an existing task by its ID.
  update                   Update an existing task with a new text description.
  move                     Move an existing task under a new context.
  clear                    Clear all done and cancelled tasks, permanently
                           deleting them from the Taskfile.

Reporters:
  today                    Generate a today message summary, including all
                           pending and in progress tasks. Optionally takes a
                           context option (--context or -c) to filter tasks by
                           that context.

Refs management:
  refs                     List all configured ref replacement rules.
  set-ref                  Add a new ref replacement rule
  delete-ref               Delete an existing ref replacement rule by its repo.
```

## Development

T is written in Haskell and requires [stack](https://www.haskellstack.org).

It supports all standard `stack` commands, e.g. `test`, `run`, `build` or `install`.

After cloning the repo, you can run `stack run -- list`, which will create an
example `tasks.json` file in the project directory and display all current
tasks.
