#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
CONFIG_DIR=$(cd "$SCRIPT_DIR/.." && pwd)
DEVICE=${ANDROID_SERIAL:-}
RESTART=false
PACKAGES=()

usage() {
  cat <<'EOF'
Usage: sync-android.sh [--restart] [--device SERIAL] [--package NAME]...

Sync the Emacs configuration to Android. Elpaca's package store, caches,
generated files, Git metadata, and core/core-secrets.el are not copied.

Options:
  -r, --restart       Restart Android Emacs after syncing
  -d, --device ID     ADB serial; defaults to ANDROID_SERIAL or the sole device
  -p, --package NAME  Also sync an Elpaca source package (repeatable)
  -h, --help          Show this help

When run from elpaca/sources/NAME, that package is included automatically.
EOF
}

while (($#)); do
  case $1 in
    -r|--restart)
      RESTART=true
      shift
      ;;
    -d|--device)
      [[ $# -ge 2 ]] || { echo "Missing value for $1" >&2; exit 2; }
      DEVICE=$2
      shift 2
      ;;
    -p|--package)
      [[ $# -ge 2 ]] || { echo "Missing value for $1" >&2; exit 2; }
      PACKAGES+=("$2")
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

command -v adb >/dev/null || { echo "adb is not installed or not in PATH" >&2; exit 1; }

if [[ -z $DEVICE ]]; then
  DEVICES=$(adb devices | awk 'NR > 1 && $2 == "device" { print $1 }')
  if [[ $(printf '%s\n' "$DEVICES" | sed '/^$/d' | wc -l | tr -d ' ') != 1 ]]; then
    echo "Expected one connected ADB device; pass --device SERIAL." >&2
    adb devices >&2
    exit 1
  fi
  DEVICE=$DEVICES
fi

adb -s "$DEVICE" get-state >/dev/null
adb -s "$DEVICE" shell 'run-as org.gnu.emacs test -d files/.emacs.d'

case $PWD/ in
  "$CONFIG_DIR"/elpaca/sources/*/)
    CURRENT_PACKAGE=${PWD#"$CONFIG_DIR"/elpaca/sources/}
    CURRENT_PACKAGE=${CURRENT_PACKAGE%%/*}
    PACKAGES+=("$CURRENT_PACKAGE")
    ;;
esac

ARCHIVE=$(mktemp /tmp/emacs-android-sync.XXXXXX.tar.gz)
trap 'rm -f "$ARCHIVE"' EXIT

CONFIG_ITEMS=(
  init.el
  early-init.el
  bootstrap.el
  .mc-lists.el
  eshell-aliases
  core
  packages
  resources
  snippets
  auto-insert
)

echo "Syncing Emacs config to $DEVICE..."
COPYFILE_DISABLE=1 tar -C "$CONFIG_DIR" -czf "$ARCHIVE" \
  --exclude='.DS_Store' \
  --exclude='*.elc' \
  --exclude='*.eln' \
  --exclude='core/core-secrets.el' \
  "${CONFIG_ITEMS[@]}"
adb -s "$DEVICE" push "$ARCHIVE" /data/local/tmp/emacs-config-sync.tar.gz >/dev/null
adb -s "$DEVICE" shell \
  'run-as org.gnu.emacs tar -xzf /data/local/tmp/emacs-config-sync.tar.gz -C files/.emacs.d'
# Config source must win over bytecode left by an earlier Android build.
adb -s "$DEVICE" shell \
  'run-as org.gnu.emacs find files/.emacs.d/core files/.emacs.d/packages -type f \( -name "*.elc" -o -name "*.eln" \) -delete' \
  >/dev/null

for PACKAGE in ${PACKAGES[@]+"${PACKAGES[@]}"}; do
  [[ $PACKAGE =~ ^[A-Za-z0-9._+-]+$ ]] || {
    echo "Invalid package name: $PACKAGE" >&2
    exit 2
  }
  PACKAGE_DIR="$CONFIG_DIR/elpaca/sources/$PACKAGE"
  [[ -d $PACKAGE_DIR ]] || {
    echo "Package source does not exist: $PACKAGE_DIR" >&2
    exit 1
  }
  adb -s "$DEVICE" shell \
    "run-as org.gnu.emacs test -d files/.emacs.d/elpaca/sources/$PACKAGE" || {
      echo "Android package source is missing: $PACKAGE" >&2
      exit 1
    }

  echo "Syncing package $PACKAGE..."
  COPYFILE_DISABLE=1 tar -C "$PACKAGE_DIR" -czf "$ARCHIVE" \
    --exclude='.git' \
    --exclude='.agent-shell' \
    --exclude='.DS_Store' \
    --exclude='*.elc' \
    --exclude='*.eln' \
    .
  adb -s "$DEVICE" push "$ARCHIVE" "/data/local/tmp/emacs-$PACKAGE-sync.tar.gz" >/dev/null
  adb -s "$DEVICE" shell \
    "run-as org.gnu.emacs tar -xzf /data/local/tmp/emacs-$PACKAGE-sync.tar.gz -C files/.emacs.d/elpaca/sources/$PACKAGE"
  # Android build directories link to their source trees. Remove stale bytecode
  # so the synchronized Lisp is used immediately on the next start.
  adb -s "$DEVICE" shell \
    "run-as org.gnu.emacs sh -c 'if test -d files/.emacs.d/elpaca/builds/$PACKAGE; then find files/.emacs.d/elpaca/builds/$PACKAGE -name \"*.elc\" -delete; fi'"
done

if $RESTART; then
  echo "Restarting Android Emacs..."
  adb -s "$DEVICE" shell am force-stop org.gnu.emacs
  adb -s "$DEVICE" shell monkey -p org.gnu.emacs -c android.intent.category.LAUNCHER 1 >/dev/null
fi

echo "Sync complete."
