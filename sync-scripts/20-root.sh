# Root sync script

prep() {
    find "$repo/root" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$repo_staging/root/$file")"
        cp "$repo/root/$file" "$repo_staging/root/$file"
        mkdir -p "$(dirname "$sys_staging/root/$file")"
        cp "$root/$file" "$sys_staging/root/$file"
    done
}

pull() {
    find "$sys_staging/root" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$repo/root/$file")"
        if cmp --quiet -- "$sys_staging/root/$file" "$repo/root/$file"; then continue; fi
        cp -v "$sys_staging/root/$file" "$repo/root/$file"
        chown "$user:" "$home/$file"
    done
}

push() {
    find "$repo_staging/root" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$root/$file")"
        if cmp --quiet -- "$repo_staging/root/$file" "$root/$file"; then continue; fi
        cp -v "$repo_staging/root/$file" "$root/$file"
        chown "root:" "$root/$file"
    done
}
