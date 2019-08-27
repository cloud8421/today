#!/usr/bin/env bats

TODAY=~/.local/bin/today

load test_helper

setup() {
  pushd "$BATS_TMPDIR" || exit
  rm_if_exists "tasks.json"
  rm_if_exists "flag.json"
  rm_if_exists "env-variable.json"
}

teardown() {
  popd || exit
}

@test "general: displays version number" {
  run "$TODAY" --version
  assert_success
  assert_output_contains "2.0"
}

@test "taskfile: uses default" {
  run "$TODAY" list
  assert_success
  assert_file_exists "tasks.json"
}

@test "taskfile: supports flag" {
  run "$TODAY" -f flag.json list
  assert_success
  assert_file_exists "flag.json"
}

@test "taskfile: supports environment variable" {
  TASKFILE=env-variable.json run "$TODAY" list
  assert_success
  assert_file_exists "env-variable.json"
}

@test "tasks: add and browse context" {
  run "$TODAY" add --context work "Do something"
  assert_success

  run "$TODAY" list --include-context work
  assert_output_contains "Do something"

  run "$TODAY" list --exclude-context work
  assert_output_doesnt_contain "Do something"
}

@test "tasks: add and remove" {
  run "$TODAY" add --context work "Do something"
  assert_success

  task_id="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" remove "$task_id"
  assert_success

  run "$TODAY" list
  assert_output_doesnt_contain "Do something"
}

@test "tasks: add and move" {
  run "$TODAY" add --context work "Do something"
  assert_success

  task_id="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" list --include-context work
  assert_output_contains "Do something"

  run "$TODAY" move "$task_id" home
  assert_success
  run "$TODAY" list --include-context home
  assert_output_contains "Do something"

  run "$TODAY" list --include-context work
  assert_output_doesnt_contain "Do something"
}

@test "tasks: add and generate today in message" {
  run "$TODAY" add --context work "Do something"
  assert_success

  run "$TODAY" in --include-context work
  assert_output_contains ":hourglass: Do something"
}

@test "tasks: add, complete and generate today out message" {
  run "$TODAY" add --context work "Do something"
  assert_success

  task_id="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" finish "$task_id"
  assert_success

  run "$TODAY" out --include-context work
  assert_output_contains ":white_check_mark: Do something"
}

@test "tasks: add, complete and clear" {
  run "$TODAY" add --context work "Do something"
  assert_success

  task_id="$(echo "$output" | grep "Do something" | awk '{ print substr($2, 1, length($2)-1) }')"

  run "$TODAY" finish "$task_id"
  assert_success
  assert_output_contains "Do something"

  run "$TODAY" clear
  assert_output_doesnt_contain "Do something"
}

@test "refs: setting and expanding a ref" {
  run "$TODAY" add --context work "Fix issue SUPPORT#123"
  assert_success
  assert_output_doesnt_contain "https://example.zendesk.com/issues/123"

  run "$TODAY" set-ref SUPPORT 'https://example.zendesk.com/issues/$id'
  assert_success

  run "$TODAY" list
  assert_output_contains "https://example.zendesk.com/issues/123"
}

@test "refs: setting and unsetting a ref" {
  run "$TODAY" refs
  assert_success
  assert_output_doesnt_contain "SUPPORT"
  assert_output_doesnt_contain "https://example.zendesk.com/issues/123"

  run "$TODAY" set-ref SUPPORT 'https://example.zendesk.com/issues/$id'
  assert_success
  assert_output_contains "SUPPORT"
  assert_output_contains 'https://example.zendesk.com/issues/$id'

  run "$TODAY" delete-ref SUPPORT
  assert_success
  assert_output_doesnt_contain "SUPPORT"
  assert_output_doesnt_contain "https://example.zendesk.com/issues/123"
}
