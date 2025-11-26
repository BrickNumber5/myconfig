#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
# probably, unnecessary, none of these seem to be used in practice
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# set default cursor
xsetroot -cursor_name left_ptr

# source the xinitrc.d system scripts
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# disable automatic screen blanking
xset s off
xset s noblank
xset -dpms

# create /tmp/status_bar
_status_bar_pipe=/tmp/status_bar
if ! [ -p $_status_bar_pipe ]; then
    mkfifo $_status_bar_pipe
fi

# boot xmonad, recompile first because pacman breaks haskell packages with
# reckless abandon due to something, something, dynamic linking so a recompile
# is needed with every upgrade, recompiling is fast enough that i'm reasonably
# comfortable with just always recompiling on every login, just to be safe
xmonad --recompile
exec xmonad
