#!/bin/sh

set -o nounset
set -o errexit

readonly PROGNAME=$(basename $0)
readonly PROGDIR=$(readlink $(dirname $0))
readonly ARGS="${@}"
readonly NARGS="${#}"

emacs --no-init-file --load mesh-run-new.el
