# Package sync script

prep() {
    cp "$repo/pkglist" "$repo_staging/pkglist"
    
    printf '' > "$sys_staging/pkglist"
    
    # Helper file that lists the resolved package names of all packages in pkglist
    # (in the form returned by pacman -Qe)
    printf '' > "$sys_staging/.mini.pkglist"
    
    # Helper file that lists the package names of all packages in pkglist NOT
    # explicitly installed on the system
    printf '' > "$sys_staging/.repo-only.pkglist"
    
    cat "$repo/pkglist" |
    while IFS= read -r -d $'\n' line; do
        local package_name="$(printf '%s\n' "$line" | sed 's/#.*$//g;s/\s*//g')"
        
        # Keep lines that don't contain a package
        if [ -z "$package_name" ]; then
            printf '%s\n' "$line" >> "$sys_staging/pkglist"
            continue
        fi
        
        # Keep lines that contain a package that is explictly installed
        local resolved_package_name
        resolved_package_name="$(pacman -Qen "$package_name" 2>/dev/null)"
        if [ "$?" == 0 ]; then
            printf '%s\n' "$line" >> "$sys_staging/pkglist"
            printf '%s\n' "$resolved_package_name" >> "$sys_staging/.mini.pkglist"
            continue
        else
            printf '%s\n' "$package_name" >> "$sys_staging/.repo-only.pkglist"
        fi
    done
    
    # Also mark base and all of install.sh's PKG_LISTs as included in the pkglist
    # so they don't trigger a 'New Packages' section.
    sed -n "s/^.*PKG_LIST='\(.*\)'/\1/p" "$repo/install.sh" |
    tr ' ' $'\n' | pacman -Qe base - >> "$sys_staging/.mini.pkglist"
    
    # Find explictly installed packages not in pkglist
    pacman -Qen | grep -xvFf "$sys_staging/.mini.pkglist" - > "$sys_staging/.sys-only.pkglist"
    
    if [ -s "$sys_staging/.sys-only.pkglist" ]; then
        printf '\n# New Packages\n' >> "$sys_staging/pkglist"
        sed 's/^\(.*\)\s.*$/\1/g' "$sys_staging/.sys-only.pkglist" >> "$sys_staging/pkglist"
    fi
    
    rm "$sys_staging/.mini.pkglist"
    rm "$sys_staging/.sys-only.pkglist"
    cp "$sys_staging/.repo-only.pkglist" "$repo_staging/.repo-only.pkglist"
}

pull() {
    if cmp --quiet -- "$sys_staging/pkglist" "$repo/pkglist"; then return; fi
    cp -v "$sys_staging/pkglist" "$repo/pkglist"
    chown "$user:" "$repo/pkglist"
}

push() {
    if [ -s "$sys_staging/.repo-only.pkglist" ]; then
        cat "$sys_staging/.repo-only.pkglist" | pacman -Syu -
    fi
}
