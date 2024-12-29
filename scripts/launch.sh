#!/usr/bin/env bash
set -euo pipefail

if ! emacsclient -s launcher -e '(message "Launcher daemon running")' &>/dev/null; then
    ~/.config/emacs/scripts/launcher-daemon.sh
    sleep 1
fi

(emacsclient -s launcher -c -e "(+launch-default-launcher)" &)
