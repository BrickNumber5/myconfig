#!/bin/sh

# 1. Initialize the critical variables
root="${BRI_SYNC_ROOT:-/}"
root="$(realpath "$root")"
user="${BRI_SYNC_USER:-"$SUDO_USER"}"
user="${user:-"$USER"}"
home="${BRI_SYNC_HOME:-"$(getent passwd "$user" | cut -d: -f6)"}"
home="$(realpath "$home")"
repo="${BRI_SYNC_REPO:-.}"
repo="$(realpath "$repo")"
sys_staging="$(mktemp --directory)"
repo_staging="$(mktemp --directory)"

printf 'Running sync ...\n'

# 2. Stage files
for script in $(find "$repo/sync-scripts" -mindepth 1 -print | LC_ALL=C sort); do
    printf 'Preparing to sync using %s...\n' "$script"
    ( source "$script" && prep )
done

# 3. Command based behavior
case "$1" in
    (diff)
        printf 'Diffing sync perparations...\n'
        git diff --no-index -- "$repo_staging" "$sys_staging"
    ;;
    (pull)
        printf 'Pulling changes from the system...\n'
        for script in $(find "$repo/sync-scripts" -mindepth 1 -print | LC_ALL=C sort); do
            printf 'Pulling changes using %s...\n' "$script"
            ( source "$script" && pull )
        done
    ;;
    (push)
        printf 'Pushing changes to the system...\n'
        for script in $(find "$repo/sync-scripts" -mindepth 1 -print | LC_ALL=C sort); do
            printf 'Pushing changes using %s...\n' "$script"
            ( source "$script" && push )
        done
    ;;
esac

# 4. Clean up staged
rm -r "$sys_staging"
rm -r "$repo_staging"
