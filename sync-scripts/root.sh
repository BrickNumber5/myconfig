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
