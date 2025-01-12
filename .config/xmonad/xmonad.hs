import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.Dmenu as Dmenu
import XMonad.Util.Run

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

main :: IO ()
main = xmonad
     . ewmh
     . docks
     . withSB ( statusBarProp "xmobar ~/.config/xmobar/xmobartoprc" (pure customXmobarPP)
             <> statusBarProp "xmobar ~/.config/xmobar/xmobarbotrc" (pure customXmobarPP))
     $ customConfig

customXmobarPP :: PP
customXmobarPP = def
    { ppSep             = magenta " • "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = xmobarBorder "Bottom" tmmagenta 2
    , ppHidden          = white
    , ppHiddenNoWindows = lowWhite
    , ppOrder           = \[ws, l, win] -> [ws, win]
    , ppTitle           = ppWindow
    }
  where
    ppWindow :: String -> String
    ppWindow = (\w -> if   null w
                      then ""
                      else wrap (white "[") (white "]") . magenta . xmobarRaw $ w)
             . shorten 25

    lowWhite, magenta, white :: String -> String
    magenta  = xmobarColor tmmagenta ""
    white    = xmobarColor "#FFFFFF" ""
    lowWhite = xmobarColor "#AAAAAA" ""

customConfig = def
    { modMask     = mod4Mask   -- Rebind Mod to the Super Key (So I can actually use [Alt] for normal things)
    , layoutHook  = avoidStruts $ spacingWithEdge 3 $ customLayoutHook
    , manageHook  = customManageHook
    , startupHook = customStartupHook
    , borderWidth = 2
    , focusedBorderColor = tmmagenta
    , terminal = "alacritty"
    }
  `removeKeysP`
    [ "M-p"
    , "M-S-p" 
    ]
  `additionalKeysP`
    [ ("M-/",   runProcessWithInput "dmenu_path" [] ""
            >>= menu "Launch" . split (== '\n')
            >>= safeSpawnProg
      )
    , ("M-C-/", runProcessWithInput "dmenu_path" [] ""
            >>= menu "Launch (In Terminal)" . split (== '\n')
            >>= runInTerm ""
      )
      
    , ("M-f",   safeSpawnProg "firefox")
      -- This relies, perhaps dangerously, on a) the current working directory always being home
      -- (since runProcessWithInput doesn't do shell expansion) and b) that the profiles will always
      -- be in approximately the same format in ~/.mozilla/firefox/profiles.ini.
      -- Likely, the worst possible actual breakage will be that this shortcut simply stops working at
      -- some point.
    , ("M-S-f", runProcessWithInput "sed" ["/Name=/!d;s/Name=//", ".mozilla/firefox/profiles.ini"] ""
            >>= menu "Firefox Profile" . split (== '\n')
            >>= safeSpawn "firefox" . ("-P" :) . (: [])
      )
    
    , ("<XF86Calculator>", runInTerm "" "python3")
    
    , ("M-<Print>",   unGrab *> spawn "cd ~/Screenshots ; scrot")
    , ("M-S-<Print>", unGrab *> spawn "cd ~/Screenshots ; scrot -s")
    
    , ("M-S-s", safeSpawnProg "slock")
    
    , ("<XF86MonBrightnessUp>",   safeSpawn "lux" ["-a", "5%"])
    , ("<XF86MonBrightnessDown>", safeSpawn "lux" ["-s", "5%"])
    
    , ("<XF86AudioMute>",          safeSpawn "pamixer" ["-t"])
    , ("<XF86AudioLowerVolume>",   safeSpawn "pamixer" ["-d", "1"])
    , ("<XF86AudioRaiseVolume>",   safeSpawn "pamixer" ["-i", "1"])
    , ("S-<XF86AudioRaiseVolume>", safeSpawn "pamixer" ["-i", "1", "--allow-boost"])
    ]

customLayoutHook = tiledLayout ||| threeColLayout ||| Mirror tiledLayout ||| Full
  where tiledLayout    = Tall        nmain delta mratioTiled
        threeColLayout = ThreeColMid nmain delta mratioThree
        nmain          = 1       -- Number of windows in main pane (1)
        delta          = 3/100   -- Resize increment (%)
        mratioTiled    = 2/3     -- Ratio of main pane to others
        mratioThree    = 1/2

customManageHook :: ManageHook
customManageHook = composeAll
    [ className =? "Gimp" --> doFloat
    , isDialog            --> doFloat
    ]

customStartupHook :: X ()
customStartupHook = do
  spawn "feh --bg-fill --no-fehbg ~/.wallpaper/current"

 -- Utility to split strings
split     :: (t -> Bool) -> [t] -> [[t]]
split p l = case dropWhile p l of
                 [] -> []
                 l' -> x : split p l''
                     where (x, l'') = break p l'

 -- Utility for spawning menus
menu :: MonadIO m => String -> [String] -> m String
menu prompt options = Dmenu.menuArgs "dmenu"
    [ "-p", prompt
    , "-i"
    , "-b"
    , "-fn", "Fira Code:style=Bold"
    , "-nb", tmblack
    , "-nf", tmwhite
    , "-sb", tmblack
    , "-sf", tmmagenta
    ] $ map (filter (/= '\n')) options

 -- Theme Colors
tmblack, tmgray, tmwhite, tmmagenta :: String
tmblack   = "#000000"
tmgray    = "#AAAAAA"
tmwhite   = "#FFFFFF"
tmmagenta = "#FF44AA"
