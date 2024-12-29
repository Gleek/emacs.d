#!/usr/bin/env bash
set -euo pipefail

# if it's already running
if emacsclient -s launcher -e '(message "Launcher")' &> /dev/null; then
    echo "Running already. Killing it first"
    emacsclient -s launcher -e '(kill-emacs)'
    sleep 1
fi

echo "Starting new emacs"
exec emacs --daemon=launcher \
     --eval '(setq server-name "launcher")' \
     --eval '(setq default-frame-alist '\''((visibility . nil) (vertical-scroll-bars)))'
