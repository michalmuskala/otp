#! /bin/bash
script_loc="$(dirname "${BASH_SOURCE[0]}")"
exec $script_loc/erl "$@"
