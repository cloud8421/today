#!/usr/bin/env bats

TODAY=~/.local/bin/today

load test_helper

setup() {
  pushd "$BATS_TMPDIR" || exit
  rm tasks.json || true
  rm flag.json || true
  rm env-variable.json || true
}

teardown() {
  popd || exit
}

@test "displays version number" {
  run "$TODAY" --version
  assert_success
  assert_output_contains "2.0"
}

@test "use the default taskfile" {
  run "$TODAY" list
  assert_success
  assert_file_exists "tasks.json"
}

@test "use a custom taskfile via a flag" {
  run "$TODAY" -f flag.json list
  assert_success
  assert_file_exists "flag.json"
}

@test "use a custom taskfile via an environment variable" {
  TASKFILE=env-variable.json run "$TODAY" list
  assert_success
  assert_file_exists "env-variable.json"
}

@test "add a task, browse tasks in contexts" {
  run "$TODAY" add --context work "Do something"
  assert_success

  run "$TODAY" list --include-context work
  assert_output_contains "Do something"

  run "$TODAY" list --exclude-context work
  assert_output_doesnt_contain "Do something"
}

@test "add a task, remove a task" {
  run "$TODAY" add --context work "Do something"
  assert_success

  task_id="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" remove "$task_id"
  assert_success

  run "$TODAY" list
  assert_output_doesnt_contain "Do something"
}

@test "add a task, generate a today in message" {
  run "$TODAY" add --context work "Do something"
  assert_success

  run "$TODAY" in --include-context work
  assert_output_contains ":hourglass: Do something"
}

@test "add a task, complete it and generate a today out message" {
  run "$TODAY" add --context work "Do something"
  assert_success

  task_id="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" finish "$task_id"
  assert_success

  run "$TODAY" out --include-context work
  assert_output_contains ":white_check_mark: Do something"
}

@test "setting and expanding refs" {
  run "$TODAY" add --context work "Fix issue SUPPORT#123"
  assert_success
  assert_output_doesnt_contain "https://example.zendesk.com/issues/123"

  run "$TODAY" set-ref SUPPORT 'https://example.zendesk.com/issues/$id'
  assert_success

  run "$TODAY" list
  assert_output_contains "https://example.zendesk.com/issues/123"
}
