# Firefox customization sync script

firefox_path="$home/.mozilla/firefox"
skeleton_path=".config/firefox-custom/skeleton-profile"
search_json_path="$skeleton_path/search.json"

prep() {
    # Stage and extract $home/.../search.json.mozlz4 → $sys_staging/home/.../search.json{,.sidecar}
    if [ -e "$home/$search_json_path.mozlz4" ]; then
        $repo/util-scripts/json-mozlz4-tool.py < $home/$search_json_path.mozlz4 > $sys_staging/home/$search_json_path
        $repo/util-scripts/search-json-sidecar-handler.py extract $sys_staging/home/$search_json_path $sys_staging/home/$search_json_path.sidecar
    fi
    
    # For each synced file $file in the skeleton profile
    # (the synced files consist of the contents of $repo/home/$skeleton_path
    # except that search.json.{,.sidecar} are combined into search.json.mozlz4)
    find "$repo/home/$skeleton_path" -path '*/search.json.sidecar' -prune -o ! -type d -a -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        [ "$file" = "search.json" ] && file="$file.mozlz4"
        local canonical_path="$(realpath -m $home/$skeleton_path/$file)"
        
        # For each firefox profile $profile
        sed '/Path=/!d;s/Path=//' < $firefox_path/profiles.ini |
        while IFS= read -r -d $'\n' profile; do
            # Store the path pointed to by the file in $sys_staging/firefox-custom/$profile/$file
            mkdir -p $(dirname "$sys_staging/firefox-custom/$profile/$file")
            realpath -m "$firefox_path/$profile/$file" > "$sys_staging/firefox-custom/$profile/$file"
            
            # Store the canonical path in the corresponding $repo_staging location
            mkdir -p $(dirname "$repo_staging/firefox-custom/$profile/$file")
            printf '%s\n' "$canonical_path" > "$repo_staging/firefox-custom/$profile/$file"
        done
    done
}

pull() {
    # For each synced file $file in the skeleton profile
    find "$repo/home/$skeleton_path" -path '*/search.json.sidecar' -prune -o ! -type d -a -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        [ "$file" = "search.json" ] && file="$file.mozlz4"
        
        # If there are n profiles p1, p2, ..., pn then thare are n + 2 copies
        # of each skeleton profile file.
        #   - The repo copy, located at $repo/home/$skeleton_path/$file
        #   - The canonical system copy, at $home/$skeleton_path/$file
        #   - One copy for each profile, at $firefox_path/$profile/$file
        # 
        # We will eventually want to pull the canonical system copy, but we
        # want to first resolve any differences between it and the profile
        # copies.
        #   - If all profile copies (if there are any) are the same as the
        #     canonical system copy, then no action is needed
        #   - If there are exactly two different contents (each possibly
        #     multiple times) and all copies of one version are newer than any
        #     copy of the other the newer copy is the one we want.
        #     - This handles both the "canonical copy has been edited" case
        #       which happens when, e.g. updating chrome/userChrome.css and the
        #     - "symlink which has been replaced with an updated file" case
        #       which happens when, e.g. you add a search engine and firefox
        #       overwrites the search.json.mozlz4 file.
        #   - If there are more than two different versions, we should report
        #     the conflict and leave it to the user to resolve.
        
        # 1. Initialize version A to the canonical system copy
        local version_a="$sys_staging/firefox-custom/sync-version-a/$file"
        mkdir -p $(dirname "$version_a")
        cp "$home/$skeleton_path/$file" "$version_a"
        
        # We have to store these in files if we want the following loop
        # (which is a subshell) to be able to modify it
        local ages="$sys_staging/firefox-custom/sync-version-ages"
        mkdir -p "$ages"
        stat --printf='%X' "$home/$skeleton_path/$file" > "$ages/oldest_a"
        stat --printf='%X' "$home/$skeleton_path/$file" > "$ages/newest_a"
        
        # 2. Create the directory where version B will exist if needed
        local version_b="$sys_staging/firefox-custom/sync-version-b/$file"
        mkdir -p $(dirname "$version_b")
        printf "%s"  "1000000000000000" > "$ages/oldest_b" # For all practical purposes, +∞
        printf "%s" "-1000000000000000" > "$ages/newest_b" # For all practical purposes, -∞
        
        # 3. For each firefox profile $profile
        sed '/Path=/!d;s/Path=//' < $firefox_path/profiles.ini |
        while IFS= read -r -d $'\n' profile; do
            local profile_version="$firefox_path/$profile/$file"
            local age="$(stat --printf='%X' "$profile_version")"
            if ! cmp --quiet -- "$profile_version" "$version_a"; then
                # This isn't version A, check version B
                if ! [ -e "$version_b" ]; then
                    # No existing version B, create one
                    cp "$profile_version" "$version_b"
                elif ! cmp --quiet -- "$profile_version" "$version_b"; then
                    # This isn't version A or B, report the conflict
                    printf 'sync-scripts/40-firefox.sh: 1: Conflicted file %s needs manual resolution\n' "$file" 1>&2
                    rm "$version_a"
                    # For fun technical reasons (this is technically a subshell
                    # because we are piping to while) exiting ends the inner
                    # loop, we then detect that and skip the post-processing
                    exit 1
                fi
                # This is the same as version B, update the timestamp as needed
                [ "$age" -lt "$(< "$ages/oldest_b")" ] && printf "%s\n" "$age" > "$ages/oldest_b"
                [ "$age" -gt "$(< "$ages/newest_b")" ] && printf "%s\n" "$age" > "$ages/newest_b"
            else
                # This is the same as version A, update the timestamp as needed
                [ "$age" -lt "$(< "$ages/oldest_a")" ] && printf "%s\n" "$age" > "$ages/oldest_a"
                [ "$age" -gt "$(< "$ages/newest_a")" ] && printf "%s\n" "$age" > "$ages/newest_a"
            fi
            : # Potentially load-bearing colon (we need the while loop to exit 0 normally)
        done
        
        if [ "$?" != "0" ]; then continue; fi
        # 4. Do something based on the relative timestamps of the versions
        if [ "$(< "$ages/newest_b")" -gt "$(< "$ages/newest_a")" ] && ! [ "$(< "$ages/oldest_b")" -lt "$(< "$ages/newest_a")" ] ; then
            # Version B is newer, overwrite version a with it
            cp "$version_b" "$version_a"
        elif [ "$(< "$ages/newest_a")" -gt "$(< "$ages/newest_b")" ] && ! [ "$(< "$ages/oldest_a")" -lt "$(< "$ages/newest_b")" ] ; then
            # Version A is newer, no action is needed
            :
        else
            # Neither version A nor B is cleanly newer, 
            printf 'sync-scripts/40-firefox.sh: 1: Conflicted file %s needs manual resolution\n' "$file" 1>&2
            rm "$version_a"
            continue
        fi
        
        # 5. If this is search.json.mozlz4 we need to split it into
        #    search.json{,.sidecar}
        if [ "$file" = "search.json.mozlz4" ]; then
            local search_version_a="$sys_staging/firefox-custom/sync-version-a/search.json" 
            $repo/util-scripts/json-mozlz4-tool.py < "$search_version_a.mozlz4" > "$search_version_a" 
            $repo/util-scripts/search-json-sidecar-handler.py extract "$search_version_a" "$search_version_a.sidecar" 
            tree "$search_version_a.sidecar" # TODOX
            rm "$search_version_a.mozlz4"
        fi
    done
    
    # For each synced file $file in version A
    find "$sys_staging/firefox-custom/sync-version-a" ! -type d -a -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        mkdir -p "$(dirname "$repo/home/$skeleton_path/$file")"
        if cmp --quiet -- "$sys_staging/firefox-custom/sync-version-a/$file" "$repo/home/$skeleton_path/$file"; then continue; fi
        cp -v "$sys_staging/firefox-custom/sync-version-a/$file" "$repo/home/$skeleton_path/$file"
        chown "$user:" "$repo/home/$skeleton_path/$file"
    done
}

push() {
    # At this point while pushing 30-home.sh has already run so the contents of
    # $repo/home/$skeleton_path have been pushed to $home/$skeleton_path.
    # All that remains is to merge the search.json files and create the
    # symlinks from the profile files to the skeleton.
    
    # Merge and compress $home/.../search.json{,.sidecar} → $home/.../search.json.mozlz4
    $repo/util-scripts/search-json-sidecar-handler.py combine $home/$search_json_path $home/$search_json_path.sidecar
    $repo/util-scripts/json-mozlz4-tool.py < $home/$search_json_path > $home/$search_json_path.mozlz4
    chown "$user:" "$home/$search_json_path.mozlz4"
    rm "$home/$search_json_path"
    rm -r "$home/$search_json_path.sidecar"
    
    # For each synced file $file in the skeleton profile
    find "$repo/home/$skeleton_path" -path '*/search.json.sidecar' -prune -o ! -type d -a -printf '%P\0' |
    while IFS= read -r -d $'\0' file; do
        [ "$file" = "search.json" ] && file="$file.mozlz4"
        local canonical_path="$home/$skeleton_path/$file"
        
        # For each firefox profile $profile
        sed '/Path=/!d;s/Path=//' < $firefox_path/profiles.ini |
        while IFS= read -r -d $'\n' profile; do
            # Create the directory the file belongs in if it doesn't exist
            mkdir -p $(dirname "$firefox_path/$profile/$file")
            
            # Symlink the file to the skeleton's copy
            ln -svf "$canonical_path" "$firefox_path/$profile/$file"
        done
    done
}
