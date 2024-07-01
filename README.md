# myconfig

My (Arch\*) Linux configuration files. These are arranged in the relative positions they have from my home folder, except as otherwise noted. This repository is maintained exclusively for myself, although if I ever make it public, others are welcome to see how I accomplished something. (Just don't expect any support)

\* Realistically, I doubt it matters.

## Configured Software

This repo contains configuration files for:

 - Bash        (Mostly to launch X on login, `~/.bashrc`, `~/.bash_profile`)
 - XMonad      (Used directly without a WM, launched automatically by `~/.xinitrc`)
 - XMobar      (`~/.config/xmobar/`)
 - XTerm       (`~/.Xresources`)
 - Vim         (`~/.vimrc`)
 - Alacritty   (`~/.config/alacritty/alacritty.toml`)
 - Font Config (`~/.config/fontconfig`)

## Required Software

The following software is expected by some configuration file. This is not an exclusive list, although most things not here are either requirements of things which are or near-universal (e.g. part of arch `base` or `base-devel`)

 - `xmonad`    - For xmonad
 - `xmobar`    - For status bars
 - `xterm`     - Maybe? I was using this as my terminal, but I've switched to alacritty, there might still be dependencies, though.
 - `alacritty` - For Terminal Emulation (`~/.config/xmonad/xmonad.hs`)
 - `firefox`   - Used for `M-f` (`~/.config/xmonad/xmonad.hs`)
 - `python3`   - Used for the calculator button (`~/.config/xmonad/xmonad.hs`)
 - `lux`       - Used for brightness management in (`~/.config/xmonad/xmonad.hs`) (could be quite easily replaced with an alternative backlight manager.
 - `feh`       - For wallpaper (`~/.config/xmonad/xmonad.hs`)
 - `slock`     - Lock the screen with `M-S-s` (`~/.config/xmonad/xmonad.hs`)
 - `scrot`     - Take screenshots with `M-<Print>` or `M-S-<Print>` (`~/.config/xmonad/xmonad.hs`)
 - `dmenu`     - For menus (`~/.config/xmonad/xmonad.hs`)
 - `pulseaudio`, `pamixer` - For Audio Controls and Display (`~/.config/xmonad/xmonad.hs`, `~/.config/xmobar/xmobartoprc`)

## Required Other

In addition to the required software, some configuration files expect something to be located in a particular place.

 - `~/.wallpaper/current` - This should be an image to load as the current wallpaper (using feh). In practice, I store all my wallpapers in `~/.wallpaper` and symlink this to whichever I want to use.
 - `~/Screenshots` - The screenshot shortcuts in `~/.config/xmonoad/xmonad.hs`) put screenshots here
 - Fira Code - Font. Used by xterm (`~/.Xresources`), xmobar (`~/.config/xmobar/`) and set as the default monospace font. 
 - Noto Color Emoji - Font. Set as fallback for all default font styles and specifically referred to in other places

## Other Software

(For the sake of having a record)
 - `github-cli` - For github

## Other Assorted Notes

For reasons in order to make the custom default font settings work, add a symlink to `/user/share/fontconfig/config.avail` to `~/.config/fontconfig/conf.d/` under the same name
