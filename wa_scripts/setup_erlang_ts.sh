#!/usr/bin/env bash
set -euxo pipefail

# setup environment for system libraries.
script_loc="$(dirname "${BASH_SOURCE[0]}")"
source "$script_loc/build_erlang_env_setup.sh"

cat > lib/SKIP-APPLICATIONS <<EOF
jinterface
odbc
EOF

mkdir -p "$DEST_DIR/tests"
TESTROOT="$DEST_DIR/tests" make -j tests

cd "$DEST_DIR/tests/test_server"
"$DEST_DIR/bin/erl" -eval 'ts:install(), ts:compile_testcases(), init:stop().'  -noshell
