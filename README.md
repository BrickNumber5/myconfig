# myconfig

My (Arch\*) Linux configuration files. These are arranged in the relative positions they have from my home folder, except as otherwise noted. This repository is maintained exclusively for myself, although if I ever make it public, others are welcome to see how I accomplished something. (Just don't expect any support)

\* Realistically, I doubt it matters.

## Configured Software

This repo contains configuration files for:

 - Bash   (Mostly to launch X on login, `~/.bashrc`, `~/.bash_profile`)
 - XMonad (Used directly without a WM, launched automatically by `~/.xinitrc`)
 - XMobar (`~/.config/xmobar/`)
 - XTerm  (`~/.Xresources`)
 - Vim    (`~/.vimrc`) 

## Required Software

The following software is expected by some configuration file. This is not an exclusive list, although most things not here are either requirements of things which are or near-universal (e.g. part of arch `base` or `base-devel`)

 - `xmonad`  - For xmonad
 - `xmobar`  - For status bars
 - `xterm`   - Current terminal emulator, planning to replace. Used in `~/.config/xmonad/xmonad.hs` at least.
 - `firefox` - Used for `M-f` (`~/.config/xmonad/xmonad.hs`)
 - `python3` - Used for the calculator button (`~/.config/xmonad/xmonad.hs`)
 - `lux`     - Used for brightness management in (`~/.config/xmonad/xmonad.hs`) (could be quite easily replaced with an alternative background manager.
 - `feh`     - For wallpaper (`~/.config/xmonad/xmonad.hs`)
 - `slock`   - Lock the screen with `M-S-s` (`~/.config/xmonad/xmonad.hs`)


## Required Other

In addition to the required software, some configuration files expect something to be located in a particular place.

 - `~/.wallpaper/current` - This should be an image to load as the current wallpaper (using feh). In practice, I store all my wallpapers in `~/.wallpaper` and symlink this to whichever I want to use.
 - Fira Code - Font. Used by xterm (`~/.Xresources`) and xmobar (`~/.config/xmobar/`) at least. Installed anywhere such that X can access it.
