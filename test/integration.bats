#!/usr/bin/env bats

TODAY=~/.local/bin/today

setup() {
  pushd "$BATS_TMPDIR" || exit
  rm tasks.json || true
}

teardown() {
  popd || exit
}

@test "it creates a default taskfile" {
  run "$TODAY" list
  [ "$status" -eq 0 ]

  taskfile="$(ls tasks.json)"
  [ "$taskfile" = "tasks.json" ]
}

@test "it supports setting a taskfile via a flag" {
  run "$TODAY" -f flag.json list
  [ "$status" -eq 0 ]

  taskfile="$(ls flag.json)"
  [ "$taskfile" = "flag.json" ]
}

@test "it supports setting a taskfile via an environment variable" {
  TASKFILE=env-variable.json run "$TODAY" list
  [ "$status" -eq 0 ]

  taskfile="$(ls env-variable.json)"
  [ "$taskfile" = "env-variable.json" ]
}

@test "it adds a task in a context" {
  run "$TODAY" add --context work "Do something"
  [ "$status" -eq 0 ]

  run "$TODAY" list --include-context work
  [[ "$output" =~ "Do something" ]]

  run "$TODAY" list --exclude-context work
  [[ ! "$output" =~ "Do something" ]]
}
