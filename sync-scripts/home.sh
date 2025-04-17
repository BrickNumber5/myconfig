# Home sync script

prep() {
    find "$repo/home" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$repo_staging/home/$file")"
        cp "$repo/home/$file" "$repo_staging/home/$file"
        mkdir -p "$(dirname "$sys_staging/home/$file")"
        cp "$home/$file" "$sys_staging/home/$file"
    done
}
