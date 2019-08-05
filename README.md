# T - CLI Task Manager : Efficiently manage tasks from the command line

## Usage

```
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
