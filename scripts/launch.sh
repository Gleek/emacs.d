#!/usr/bin/env bash
set -euo pipefail

SERVER_NAME="server" # pass server / launcher for primary or dedicated daemon respectively
ELISP_CMD="(+launch-default-launcher)"
if ! emacsclient -s "$SERVER_NAME" -e '(message "Launcher daemon running")' &>/dev/null; then
    ~/.config/emacs/scripts/launcher-daemon.sh
    sleep 1
fi

if [ $(emacsclient -s "server" -e "(daemonp)") = "t" ]; then
    # if it's a daemon a new frame will be created
    emacsclient -s "$SERVER_NAME" -c -e "$ELISP_CMD" &
else
    emacsclient -s "$SERVER_NAME" -e "$ELISP_CMD" &
fi
