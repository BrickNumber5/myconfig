# Home sync script

prep() {
    find "$repo/home" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$repo_staging/home/$file")"
        cp "$repo/home/$file" "$repo_staging/home/$file"
        mkdir -p "$(dirname "$sys_staging/home/$file")"
        [ -e "$home/$file" ] && cp "$home/$file" "$sys_staging/home/$file"
    done
}

pull() {
    find "$sys_staging/home" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$repo/home/$file")"
        if cmp --quiet -- "$sys_staging/home/$file" "$repo/home/$file"; then continue; fi
        if [ -e "$sys_staging/home/$file" ]; then
            cp -v "$sys_staging/home/$file" "$repo/home/$file"
            chown "$user:" "$repo/home/$file"
        fi
    done
}

push() {
    find "$repo_staging/home" ! -type d -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$home/$file")"
        if cmp --quiet -- "$repo_staging/home/$file" "$home/$file"; then continue; fi
        cp -v "$repo_staging/home/$file" "$home/$file"
        chown "$user:" "$home/$file"
    done
}
