#!/usr/bin/env bash

# Some functions lifted from https://github.com/dokku/dokku/blob/master/tests/unit/test_helper.bash

flunk() {
  {
    if [[ "$#" -eq 0 ]]; then
      cat -
    else
      echo "$*"
    fi
  }
  return 1
}

assert() {
  if ! "$*"; then
    flunk "failed: $*"
  fi
}

assert_equal() {
  if [[ "$1" != "$2" ]]; then
    {
      echo "expected: $1"
      echo "actual:   $2"
    } | flunk
  fi
}

# ShellCheck doesn't know about $status from Bats
# shellcheck disable=SC2154
# shellcheck disable=SC2120
assert_success() {
  if [[ "$status" -ne 0 ]]; then
    flunk "command failed with exit status $status"
  elif [[ "$#" -gt 0 ]]; then
    assert_output "$1"
  fi
}

# ShellCheck doesn't know about $output from Bats
# shellcheck disable=SC2154
assert_output() {
  local expected
  if [[ $# -eq 0 ]]; then
    expected="$(cat -)"
  else
    expected="$1"
  fi
  assert_equal "$expected" "$output"
}

# ShellCheck doesn't know about $output from Bats
# shellcheck disable=SC2154
assert_output_contains() {
  local input="$output"
  local expected="$1"
  local count="${2:-1}"
  local found=0
  until [ "${input/$expected/}" = "$input" ]; do
    input="${input/$expected/}"
    (( found+=1 ))
  done
  assert_equal "$count" "$found"
}

assert_output_doesnt_contain() {
  local input="$output"
  local expected="$1"
  local count="${2:-1}"
  local found=0
  until [ "${input/$expected/}" = "$input" ]; do
    input="${input/$expected/}"
    (( found+=1 ))
  done
  assert_equal 0 "$found"
}

assert_file_exists() {
  local filename="$1"
  if [ ! -f "$filename" ]; then
    flunk "File $filename does not exist"
  fi
}

rm_if_exists() {
  local filename="$1"
  if [ -f "$filename" ]; then
    rm "$filename"
  fi
}
