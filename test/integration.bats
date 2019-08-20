#!/usr/bin/env bats

TODAY=~/.local/bin/today

setup() {
  pushd "$BATS_TMPDIR" || exit
  rm tasks.json || true
}

teardown() {
  popd || exit
}

@test "use the default taskfile" {
  run "$TODAY" list
  [ "$status" -eq 0 ]

  taskfile="$(ls tasks.json)"
  [ "$taskfile" = "tasks.json" ]
}

@test "use a custom taskfile via a flag" {
  run "$TODAY" -f flag.json list
  [ "$status" -eq 0 ]

  taskfile="$(ls flag.json)"
  [ "$taskfile" = "flag.json" ]
}

@test "use a custom taskfile via an environment variable" {
  TASKFILE=env-variable.json run "$TODAY" list
  [ "$status" -eq 0 ]

  taskfile="$(ls env-variable.json)"
  [ "$taskfile" = "env-variable.json" ]
}

@test "add a task, browse tasks in contexts" {
  run "$TODAY" add --context work "Do something"
  [ "$status" -eq 0 ]

  run "$TODAY" list --include-context work
  [[ "$output" =~ "Do something" ]]

  run "$TODAY" list --exclude-context work
  [[ ! "$output" =~ "Do something" ]]
}

@test "add a task, remove a task" {
  run "$TODAY" add --context work "Do something"
  [ "$status" -eq 0 ]

  taskId="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" remove "$taskId"

  run "$TODAY" list
  [[ ! "$output" =~ "Do something" ]]
}

@test "add a task, generate a today in message" {
  run "$TODAY" add --context work "Do something"
  [ "$status" -eq 0 ]

  run "$TODAY" in --include-context work
  [[ "$output" =~ ":hourglass: Do something" ]]
}

@test "add a task, complete it and generate a today out message" {
  run "$TODAY" add --context work "Do something"
  [ "$status" -eq 0 ]

  taskId="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" finish "$taskId"
  [ "$status" -eq 0 ]

  run "$TODAY" out --include-context work
  [[ "$output" =~ ":white_check_mark: Do something" ]]
}
