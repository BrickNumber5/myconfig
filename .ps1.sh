# TODO: Add git support
# TODO: Optimization -- if nothing has changed reuse past work

# Dependencies: realpath, tput, grep, find

# We're using the powerline symbols here
# I almostly exclusively use fira code which has built-in support for powerline
# symbols so I don't have to think to hard about it
__bri_ps1__CHAR_TRI_SOLID=''
__bri_ps1__CHAR_TRI_LINE=''
__bri_ps1__CHAR_SPACE=' '
__bri_ps1__CHAR_ELIPSIS='…'
__bri_ps1__CHAR_NEWLINE_INDICATOR='↩'
__bri_ps1__CHAR_GIT_INDICATOR=''

# Dynamic sizing
__bri_ps1__DYN_SIZE_N=35 # Where possible, the minimum number of characters to leave for the prompt
__bri_ps1__DYN_SIZE_P=2  # Where possible, the maximum extent of the available space as a fraction
__bri_ps1__DYN_SIZE_Q=3  # P/Q for the prompt to take up

# Colors
__bri_ps1__CLR_FG=231
__bri_ps1__CLR_BG=232
__bri_ps1__CLR_NEGATIVE=196

__bri_ps1__CLR_HOST=163
__bri_ps1__CLR_USER=170
__bri_ps1__CLR_PATH=213
__bri_ps1__CLR_GIT=111
__bri_ps1__CLR_EXIT_CODE=231

# Render the leading triangle
#     Takes: <bg color #>
__bri_ps1__render_leading_tri() {
    printf '\1'
    tput setab "$1"
    tput setaf "$__bri_ps1__CLR_BG"
    printf '\2'
    printf "$__bri_ps1__CHAR_TRI_SOLID"
}

# Render a single segment of text with the trailing (but not leading) triangle
# assumes that the background color was already set correctly by e.g.
# a previous call to render segment or __bri_ps1__render_leading_tri()
#     Takes: <text>, <fg color #>, <bg color #>, <next bg color #>
__bri_ps1__render_segment() {
    printf '\1'
    tput setaf "$2"
    printf '\2'
    printf "$__bri_ps1__CHAR_SPACE%s$__bri_ps1__CHAR_SPACE" "$1"
    if [ "$3" -ne "$4" ]; then
        printf '\1'
        tput setab "$4"
        tput setaf "$3"
        printf '\2'
        printf "$__bri_ps1__CHAR_TRI_SOLID"
    else
        printf "$__bri_ps1__CHAR_TRI_LINE"
    fi
}

# Latch to render a segment, rendering the previous segment if one is latched
# or rendering the intial tri otherwise
#     Takes: <text>, <fg color #>, <bg color #>
__bri_ps1__render_segment_latch() {
    __bri_ps1__render_latched "$3"
    __bri_ps1__latch_text="$1"
    __bri_ps1__latch_fg="$2"
    __bri_ps1__latch_bg="$3"
}

# Render the latched segment
#     Takes: <next bg color #>
__bri_ps1__render_latched() {
    if [ "$__bri_ps1__latch_text" == '' ]; then
        __bri_ps1__render_leading_tri "$1"
    else
        __bri_ps1__render_segment "$__bri_ps1__latch_text" "$__bri_ps1__latch_fg" "$__bri_ps1__latch_bg" "$1"
    fi
    unset __bri_ps1__latch_text
}

# Extract the path into an array of segments
__bri_ps1__extract_path() {
    local path="$(realpath --relative-base ~ .)/"
    
    # If a path doesn't start with /
    if printf '%s' "$path" | grep -cv '^/' >/dev/null; then
        # Prepend ~/
        path='~/'"$path"
    fi
    
    unset __bri_ps1__path_segments
    declare -g -a __bri_ps1__path_segments
    unset __bri_ps1__path_segment_min_prefix_lengths
    
    local i=0
    local partial_path=""
    while [ "$path" != "" ]; do
        path_segment="${path%%\/*}"
        path="${path#*\/}"
        if [ "$path_segment" == "" -o "$path_segment" == "." ]; then continue; fi
        
        __bri_ps1__path_segments[i]="$(printf '%s' "$path_segment" | sed ":a;N;\$!ba;s/\n/$__bri_ps1__CHAR_NEWLINE_INDICATOR/g")"
        
        if [ "$i" != 0 ]; then
            __bri_ps1__path_segment_min_prefix_lengths[i]="${#path_segment}"
            for (( j = 1 ; j <= ${#path_segment} - 2 ; ++j )); do
                local segment_prefix="${path_segment:0:j}"
                local num_files_sharing_prefix=$(
                    find "$partial_path" -mindepth 1 -maxdepth 1 \
                                         -name "$(printf '%q' "$segment_prefix")*" \
                                         -print0 | grep -cz '^'
                )
                if [ "$num_files_sharing_prefix" == 1 ]; then
                    __bri_ps1__path_segment_min_prefix_lengths[i]="$(( j + 1 ))"
                    break
                fi
            done
        fi
        
        partial_path="$partial_path/$path_segment"
        if [ "$partial_path" == "/~" ]; then partial_path="$HOME"; fi
        
        (( ++i ))
    done
}

# Determine the maximum and minimum length of the path segments
# The path must already have been extracted, and must be unreduced
__bri_ps1__sizehints_for_path() {
    __bri_ps1__path_sizehint_max=0
    for path_segment in "${__bri_ps1__path_segments[@]}"; do
        (( __bri_ps1__path_sizehint_max += ${#path_segment} + 3 ))
    done
    
    if (( "${#__bri_ps1__path_segments[@]}" <= 2 )); then
        __bri_ps1__path_sizehint_min="$__bri_ps1__path_sizehint_max"
    else
        __bri_ps1__path_sizehint_min=$(( ${#__bri_ps1__path_segments[0]} + ${#__bri_ps1__path_segments[-1]} + 10 ))
    fi
}

# Shrink the path
# Assumes the path has already been extracted and hinted
# and that the path can be shrunk
__bri_ps1__shrink_path() {
    # 1. Find the longest shrinkable item, excluding the first and last
    #    Tiebreak in favor of the item closest to the center, favoring the beginning
    local longest_shrink_length=0
    local mid_point="$(( (${#__bri_ps1__path_segments[*]} - 1 ) / 2 ))"
    local target_index=0
    for (( i = 1 ; i <= ${#__bri_ps1__path_segments[*]} - 2 ; ++i )); do
        local segment_length="${#__bri_ps1__path_segments[$i]}"
        local minimum_segment_length="${__bri_ps1__path_segment_min_prefix_lengths[i]}"
        if (( segment_length <= minimum_segment_length )); then continue; fi
        if [ "${__bri_ps1__path_segments[$i]}" == "$__bri_ps1__CHAR_ELIPSIS" ]; then continue; fi
        if (( segment_length == longest_shrink_length )); then
            if (( i <= mid_point || ( i - mid_point < mid_point - target_index ) )); then
                target_index="$i"
            fi
        fi
        if (( segment_length > longest_shrink_length )); then
            longest_shrink_length="$segment_length"
            target_index="$i"
        fi
    done
    
    # 2. If no segment is shrinkable, then mark all segments as infinitely shrinkable and run again.
    #    That is, remove the segments need to maintain the unique prefixes requirement
    if (( longest_shrink_length == 0 )); then
        for (( i = 1 ; i <= ${#__bri_ps1__path_segments[*]} - 2 ; ++i )); do
            __bri_ps1__path_segment_min_prefix_lengths[i]=0
        done
        __bri_ps1__shrink_path
        return
    fi
    
    # 4. Shrink the targeted segment
    if (( ${__bri_ps1__path_segment_min_prefix_lengths[target_index]} > 0 )); then
        if (( ${#__bri_ps1__path_segments[target_index]} > 1 )); then
            (( --__bri_ps1__path_sizehint_max ))
        fi
        __bri_ps1__path_segments[target_index]="${__bri_ps1__path_segments[target_index]:0:-2}$__bri_ps1__CHAR_ELIPSIS"
    else
        (( __bri_ps1__path_sizehint_max -= ${#__bri_ps1__path_segments[target_index]} - 1 ))
        __bri_ps1__path_segments[target_index]="$__bri_ps1__CHAR_ELIPSIS"
        
        # 5. If we've shrunk a segment to a single elipsis, and an adjacent segment has also been shrunk,
        #    remove the adjacent segment.
        if [ "${__bri_ps1__path_segments[target_index + 1]}" == "$__bri_ps1__CHAR_ELIPSIS" ]; then
            unset __bri_ps1__path_segments["$(( target_index + 1 ))"]
            (( __bri_ps1__path_sizehint_max -= 4 ))
        fi
        if [ "${__bri_ps1__path_segments[target_index - 1]}" == "$__bri_ps1__CHAR_ELIPSIS" ]; then
            unset __bri_ps1__path_segments["$(( target_index - 1 ))"]
            (( __bri_ps1__path_sizehint_max -= 4 ))
        fi
        
        __bri_ps1__path_segments=( "${__bri_ps1__path_segments[@]}" )
    fi
}

# Render the path
# The path should already have been extracted and reduced
#     Takes: <fg color #>, <bg color #>
__bri_ps1__render_path_latch() {
    for path_segment in "${__bri_ps1__path_segments[@]}"; do
        __bri_ps1__render_segment_latch "$path_segment" "$1" "$2"
    done
}

# Extract git VCS information
__bri_ps1__extract_git() {
    unset __bri_ps1__git_segments
    declare -g -a __bri_ps1__git_segments
    __bri_ps1__git_segments=()
    
    # 1. Find the branch name (and if we're in a git repository at all in the process
    local head_short_sha="$(git rev-parse --short HEAD 2>/dev/null)"
    [ -n "$head_short_sha" ] || return 1 # Not in a git repo
    
    local branch_name=""
    branch_name="$(git symbolic-ref --short HEAD 2>/dev/null)" ||
    branch_name="($(git describe --contains --all HEAD 2>/dev/null))" ||
    branch_name="($head_short_sha)"
    
    __bri_ps1__git_segments[0]="$__bri_ps1__CHAR_GIT_INDICATOR $branch_name"
    
    # 2. Upstream divergence
    local upstream_divergence="$(git rev-list --count --left-right @{upstream}...HEAD 2>/dev/null)"
    local ud_minus="${upstream_divergence%	*}"
    local ud_plus="${upstream_divergence#*	}"
    if [ -n upstream_divergence ] && (( ud_minus != 0 || ud_plus != 0 )); then
        if (( ud_plus != 0 )); then
            __bri_ps1__git_segments[1]="${__bri_ps1__git_segments[1]}/+$ud_plus"
        fi
        if (( ud_minus != 0 )); then
            __bri_ps1__git_segments[1]="${__bri_ps1__git_segments[1]}/-$ud_minus"
        fi
        __bri_ps1__git_segments[1]="${__bri_ps1__git_segments[1]#/}"
    fi
    
    # 3. TODO: Staged, unstaged, and untracked files
    
    # 4. TODO: Status keyword (e.g. MERGING)
}

# Render git VCS information
#     Takes: <fg color #>, <bg color #>
__bri_ps1__render_git_latch() {
    for git_segment in "${__bri_ps1__git_segments[@]}"; do
        __bri_ps1__render_segment_latch "$git_segment" "$1" "$2"
    done
}

# Render the ps2
__bri_ps1__render_ps2() {
    __bri_ps1__render_segment_latch '+' "$__bri_ps1__CLR_FG" "$__bri_ps1__CLR_HOST"
    __bri_ps1__render_latched "$__bri_ps1__CLR_BG"
    printf '\1'
    tput sgr0
    printf '\2'
    printf "$__bri_ps1__CHAR_SPACE"
}

# Render the ps1
__bri_ps1__render() {
    # Save the exit code
    local exit_code="$?"
    
    # Setup
    printf '\n'
    
    __bri_ps1__COLUMNS_SOFTMAX=$((   COLUMNS * __bri_ps1__DYN_SIZE_P / __bri_ps1__DYN_SIZE_Q < COLUMNS - __bri_ps1__DYN_SIZE_N
                                   ? COLUMNS * __bri_ps1__DYN_SIZE_P / __bri_ps1__DYN_SIZE_Q : COLUMNS - __bri_ps1__DYN_SIZE_N ))
    
    # Prepare segments based on space
    (( __bri_ps1__COLUMNS_SOFTMAX -= 2 ))                     # The leading tri and trailing space
    (( __bri_ps1__COLUMNS_SOFTMAX -= ${#HOSTNAME} + 3 ))      # The fixed-size hostname segment
    (( __bri_ps1__COLUMNS_SOFTMAX -= ${#USER} + 3 ))          # The fixed-size username segment
    if (( "$exit_code" != 0 )); then
        (( __bri_ps1__COLUMNS_SOFTMAX -= ${#exit_code} + 3 )) # The fixed-size exit code segment
    fi
    
    # Prepare the path segment
    __bri_ps1__extract_path
    __bri_ps1__sizehints_for_path
    
    # Prepare the git segment
    __bri_ps1__extract_git
    
    # Dynamically shrink shrinkable segments (path [TODO: and git])
    while ((    __bri_ps1__path_sizehint_max > __bri_ps1__path_sizehint_min
              && __bri_ps1__path_sizehint_max > __bri_ps1__COLUMNS_SOFTMAX   )); do
        __bri_ps1__shrink_path
    done
    
    # Render the segments
    
    # Host
    __bri_ps1__render_segment_latch "$HOSTNAME" "$__bri_ps1__CLR_FG" "$__bri_ps1__CLR_HOST"
    
    # User
    __bri_ps1__render_segment_latch "$USER" "$__bri_ps1__CLR_FG" "$__bri_ps1__CLR_USER"
    
    # Path
    __bri_ps1__render_path_latch "$__bri_ps1__CLR_FG" "$__bri_ps1__CLR_PATH"
    
    # Git
    __bri_ps1__render_git_latch "$__bri_ps1__CLR_FG" "$__bri_ps1__CLR_GIT"
    
    # Exit Code
    if (( "$exit_code" != 0 )); then
        __bri_ps1__render_segment_latch "$exit_code" "$__bri_ps1__CLR_NEGATIVE" "$__bri_ps1__CLR_EXIT_CODE"
    fi
    
    # Finish
    __bri_ps1__render_latched "$__bri_ps1__CLR_BG"
    printf '\1'
    tput sgr0
    printf '\2'
    printf "$__bri_ps1__CHAR_SPACE"
    
    return "$exit_code"
}
