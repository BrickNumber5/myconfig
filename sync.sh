#!/bin/sh

# 1. Initialize the critical variables
root="${BRI_SYNC_ROOT:-/}"
root="$(realpath "$root")"
user="${BRI_SYNC_USER:-"$USER"}"
home="${BRI_SYNC_HOME:-"$(getent passwd "$user" | cut -d: -f6)"}"
home="$(realpath "$home")"
repo="${BRI_SYNC_REPO:-.}"
repo="$(realpath "$repo")"
sys_staging="$(mktemp --directory)"
repo_staging="$(mktemp --directory)"

# 2. Stage files
for script in $(find "$repo/sync-scripts" -mindepth 1 -print); do
    ( source "$script" && prep )
done

# 3. Command based behavior
case "$1" in
    (diff)
        git diff --no-index -- "$repo_staging" "$sys_staging"
    ;;
    (pull)
        for script in $(find "$repo/sync-scripts" -mindepth 1 -print); do
            ( source "$script" && pull )
        done
    ;;
esac

# 4. Clean up staged
rm -r "$sys_staging"
rm -r "$repo_staging"
