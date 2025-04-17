# TODO: Optimization -- if nothing has changed reuse past work

# Dependencies: realpath, tput, grep, find, wc, head

# We're using the powerline symbols here
# I almostly exclusively use fira code which has built-in support for powerline
# symbols so I don't have to think to hard about it
__bri_ps1__CHAR_TRI_SOLID=''
__bri_ps1__CHAR_TRI_LINE=''
__bri_ps1__CHAR_SPACE=' '
__bri_ps1__CHAR_ELIPSIS='…'
__bri_ps1__CHAR_NEWLINE_INDICATOR='↩'
__bri_ps1__CHAR_GIT_INDICATOR=''
__bri_ps1__CHAR_GIT_STAGED_MRK='●'
__bri_ps1__CHAR_GIT_UNSTAGED_MRK='◉'
__bri_ps1__CHAR_GIT_UNTRACKED_MRK='○'
__bri_ps1__CHAR_GIT_CONFLICTED_MRK='∅'
__bri_ps1__CHAR_ANGLE_BRACKET_L='⟨'
__bri_ps1__CHAR_ANGLE_BRACKET_R='⟩'

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
            unset __bri_ps1__path_segment_min_prefix_lengths["$(( target_index + 1 ))"]
            (( __bri_ps1__path_sizehint_max -= 4 ))
        fi
        if [ "${__bri_ps1__path_segments[target_index - 1]}" == "$__bri_ps1__CHAR_ELIPSIS" ]; then
            unset __bri_ps1__path_segments["$(( target_index - 1 ))"]
            unset __bri_ps1__path_segment_min_prefix_lengths["$(( target_index - 1 ))"]
            (( __bri_ps1__path_sizehint_max -= 4 ))
        fi
        
        __bri_ps1__path_segments=( "${__bri_ps1__path_segments[@]}" )
        __bri_ps1__path_segment_min_prefix_lengths=( "${__bri_ps1__path_segment_min_prefix_lengths[@]}" )
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
    unset __bri_ps1__git_segment_remove_order
    declare -g -a __bri_ps1__git_segment_remove_order
    __bri_ps1__git_segment_remove_order=()
    
    # 1. Find the branch name (and if we're in a git repository at all in the process)
    #    as well as other basic info we happen to be able to pull at the same time we
    #    may need later.
    local git_info git_info_exit_code
    git_info="$(git rev-parse --git-dir --is-inside-git-dir --short HEAD 2>/dev/null)"
    git_info_exit_code="$?"
    (( git_info_exit_code != 0 )) && return 1 # Not in a git repo
    
    local head_short_sha="${git_info##*$'\n'}"
    git_info="${git_info%$'\n'*}"
    local is_inside_git_dir="${git_info##*$'\n'}"
    local git_dir="${git_info%$'\n'*}"
    
    # We try the branch name if there is one or, failing that for detached HEADs
    # a future branch name with an offset (e.g. (main~2)) wrapped in parentheses
    # to indicate the detached head state, or if that too fails the short sha
    # (e.g. (453cd06…)) which should always work no matter how unusual the git
    # state is.
    local branch_name=""
    branch_name="$(git symbolic-ref --short HEAD 2>/dev/null)" ||
    branch_name="($(git describe --contains --all HEAD 2>/dev/null))" ||
    branch_name="($head_short_sha$__bri_ps1__CHAR_ELIPSIS)"
    
    __bri_ps1__git_segments[0]="$__bri_ps1__CHAR_GIT_INDICATOR $branch_name"
    __bri_ps1__git_segment_remove_order[0]='X'
    
    # 1a. If we are inside a git dir, we can't run any of the operations we want to for
    #     further information. We could heuristically guess that the parent is the
    #     worktree, but this may be incorrect for e.g. bare repositories and may produce
    #     bad results. Instead, simply report this fact and bail.
    if [ "$is_inside_git_dir" = true ]; then
        __bri_ps1__git_segments[1]="$__bri_ps1__CHAR_ANGLE_BRACKET_L.git$__bri_ps1__CHAR_ANGLE_BRACKET_R"
        __bri_ps1__git_segment_remove_order[1]='X'
        return 2
    fi
    
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
        __bri_ps1__git_segment_remove_order[1]=2
    fi
    
    # 3. Staged, unstaged, and untracked files
    local num_staged_files="$(git diff --name-only --staged --diff-filter=u | wc -l)"
    local num_conflicted_files="$(git diff --name-only --diff-filter=U | wc -l)"
    # FIXME: Duplicate work between this and the previous, factor out
    # Using grep -c(ount) x[whole lines] v[not in] f(ile) (the conflicting files) F[basic strings, not regexes]
    local num_unstaged_files="$(grep -cxvFf <(git diff --name-only --diff-filter=U) <(git diff --name-only))"
    local num_untracked_files="$(git ls-files --others --exclude-standard | wc -l)"
    if (( num_staged_files != 0 || num_conflicted_files != 0 || num_unstaged_files != 0 || num_untracked_files != 0 )); then
        local files_segment=''
        # This strategy uses eval (which is evil) but
        # a) we quote the potentially dangerous characters and
        # b) this is still faster than a lot of other solutions for small counts
        #    (which are realistically most of what we will be running against)
        if (( num_staged_files != 0 )); then
            files_segment="$files_segment$(
                eval "printf \"%.s$(
                    printf '%q' "$__bri_ps1__CHAR_GIT_STAGED_MRK"
                )\" {1..$num_staged_files}"
            )"
        fi
        if (( num_conflicted_files != 0 )); then
            files_segment="$files_segment$(
                eval "printf \"%.s$(
                    printf '%q' "$__bri_ps1__CHAR_GIT_CONFLICTED_MRK"
                )\" {1..$num_conflicted_files}"
            )"
        fi
        if (( num_unstaged_files != 0 )); then
            files_segment="$files_segment$(
                eval "printf \"%.s$(
                    printf '%q' "$__bri_ps1__CHAR_GIT_UNSTAGED_MRK"
                )\" {1..$num_unstaged_files}"
            )"
        fi
        if (( num_untracked_files != 0 )); then
            files_segment="$files_segment$(
                eval "printf \"%.s$(
                    printf '%q' "$__bri_ps1__CHAR_GIT_UNTRACKED_MRK"
                )\" {1..$num_untracked_files}"
            )"
        fi
        __bri_ps1__git_segments+=( "$files_segment" )
        __bri_ps1__git_segment_remove_order+=( 1 )
    fi
    
    # 4. Status word (REBASING, MERGING, BISECTING, CHERRY PICKING, and REVERTING)
    local status_word=''
    local current_step total_steps
    if [ -d "$git_dir/rebase-merge" ]; then
        status_word='REBASING'
        current_step="$(head -1 "$git_dir/rebase-merge/msgnum")"
        total_steps="$(head -1 "$git_dir/rebase-merge/end")"
    elif [ -d "$git_dir/rebase-apply" ]; then
        status_word='REBASING'
        current_step="$(head -1 "$git_dir/rebase-apply/next")"
        total_steps="$(head -1 "$git_dir/rebase-apply/last")"
    elif [ -f "$git_dir/MERGE_HEAD" ]; then
        status_word='MERGING'
    elif [ -f "$git_dir/BISECT_LOG" ]; then
        status_word='BISECTING'
    elif [ -f "$git_dir/CHERRY_PICK_HEAD" ]; then
        status_word='CHERRY PICKING'
    elif [ -f "$git_dir/REVERT_HEAD" ]; then
        status_word='REVERTING'
    elif [ -f "$git_dir/sequencer/todo" ]; then
        # Under some (I'm not totally clear on) set of conditions, the cherry
        # pick head or revert head can disappear even though you are still
        # performing the operation in question. We can find this information
        # by looking at the sequencer.
        local sequencer_todo="$(head -1 "$git_dir/sequencer/todo")"
        case "$sequencer_todo" in
            (p*)
                status_word='CHERRY PICKING'
            ;;
            (r*)
                status_word='REVERTING'
            ;;
        esac
    fi
    
    if [ -n "$status_word" ]; then
        __bri_ps1__git_segments+=( "$status_word" )
        __bri_ps1__git_segment_remove_order+=( 'X' )
        
        if [ -n "$current_step" ] && [ -n "$total_steps" ]; then
            __bri_ps1__git_segments+=( "$current_step/$total_steps" )
            __bri_ps1__git_segment_remove_order+=( '3' )
        fi
    fi
    
    # Things we explictly don't handle:
    #   AM               -- I've never used it, never intend to, and so cannot
    #                       be bothered to figure out how it works in enough
    #                       detail to add support.
    #   Sparse Checkouts -- This is a Microsofty extension for handling really
    #                       big monorepos. I'm not much of a monorepo person
    #                       and the implementation is really subject to change
    #                       anyway making any support likely to quickly become
    #                       out of date and stop working.
    #   Bare Repos       -- I've never used them so I don't no how they work
    #                       if at some point I need a bare repo I will probably
    #                       add whatever support that needs.
    #                       I think (but can't confirm) that the current
    #                       tactics will work okay in a bare repo
}

# Determine the maximum and minimum length of the git segments
# The info must already have been extracted, and must be unreduced
__bri_ps1__sizehints_for_git() {
    __bri_ps1__git_sizehint_max=0
    for git_segment in "${__bri_ps1__git_segments[@]}"; do
        (( __bri_ps1__git_sizehint_max += ${#git_segment} + 3 ))
    done
    
    __bri_ps1__git_sizehint_min=0
    for (( i = 0 ; i < ${#__bri_ps1__git_segments[*]} ; ++i )); do
        if [ "${__bri_ps1__git_segment_remove_order[i]}" == 'X' ]; then
            (( __bri_ps1__git_sizehint_min += ${#__bri_ps1__git_segments[i]} + 3 ))
        fi
    done
}

# Shrink the git VCS info
# Assumes the info has already been extracted and hinted
# and that the info can be shrunk
__bri_ps1__shrink_git() {
    # 1. Find the first segment to be removed
    local target_index=0
    local min_rm_order=1000000
    for (( i = 1 ; i < ${#__bri_ps1__git_segments[*]} ; ++i )); do
        local rm_order="${__bri_ps1__git_segment_remove_order[i]}" 
        if [ "$rm_order" == 'X' ]; then continue; fi
        if (( rm_order > min_rm_order )); then continue; fi
        target_index="$i"
        min_rm_order="$rm_order"
    done
    
    # 2. Remove the segment
    (( __bri_ps1__git_sizehint_max -= ${#__bri_ps1__git_segments[target_index]} + 3 ))
    unset __bri_ps1__git_segments[target_index]
    unset __bri_ps1__git_segment_remove_order[target_index]
    __bri_ps1__git_segments=( "${__bri_ps1__git_segments[@]}" )
    __bri_ps1__git_segment_remove_order=( "${__bri_ps1__git_segment_remove_order[@]}" )
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
    __bri_ps1__sizehints_for_git
    
    # Dynamically shrink shrinkable segments (path [TODO: and git])
    while (( __bri_ps1__path_sizehint_max + __bri_ps1__git_sizehint_max > __bri_ps1__COLUMNS_SOFTMAX )); do
        if ((    __bri_ps1__path_sizehint_max <= __bri_ps1__path_sizehint_min
              && __bri_ps1__git_sizehint_max  <= __bri_ps1__git_sizehint_min  )); then break; fi
        
        # If the git segment is at least half the size of the path segment
        # and can be shrunk, shrink it
        if ((    __bri_ps1__git_sizehint_max * 2 >= __bri_ps1__path_sizehint_max
              && __bri_ps1__git_sizehint_max > __bri_ps1__git_sizehint_min )); then
            __bri_ps1__shrink_git
            continue
        fi
        
        # Otherwise if the path segment can be shrunk, shrink it
        if (( __bri_ps1__path_sizehint_max > __bri_ps1__path_sizehint_min )); then
            __bri_ps1__shrink_path
            continue
        fi
        
        # Otherwise if the git segment can be shrunk, shrink it
        # (We hit this case if the path is very large but cannot be shrunk, e.g.
        # when we are in a directory with a very long name)
        if (( __bri_ps1__git_sizehint_max > __bri_ps1__git_sizehint_min )); then
            __bri_ps1__shrink_git
            continue
        fi
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
