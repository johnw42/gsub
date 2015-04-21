#! /bin/bash

set -e
IFS=
cd $(dirname $0)
tmp_dir=dist/test_tmp
test_bin=dist/build/hs-gsub/hs-gsub

cleanup() {
  true
}

run() {
  set +e
  $test_bin "$@" &>$tmp_dir/stdout.txt
  echo $? >>$tmp_dir/stdout.txt
  set -e
}

check_stderr() {
  diff -u - $tmp_dir/stdout.txt
}

main() {
  trap cleanup EXIT

  cabal build
  rm -rf dist/test_tmp
  cp -a test_data $tmp_dir

  run 'a' 'b' $tmp_dir $tmp_dir/no_such_file
  check_stderr <<EOF
$tmp_dir: is a directory
$tmp_dir/no_such_file: no such file
1
EOF

  run 'a' '\1' $tmp_dir/a
  check_stderr <<EOF
hs-gsub: pattern has fewer than 1 groups
2
EOF
}

main "$@"
