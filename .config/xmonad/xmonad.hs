import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP

main :: IO ()
main = xmonad
     . ewmh
     . withEasySB ( statusBarProp "xmobar ~/.config/xmobar/xmobartoprc" (pure customXmobarPP)
                 <> statusBarProp "xmobar ~/.config/xmobar/xmobarbotrc" (pure customXmobarPP)) defToggleStrutsKey
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
    --, ppExtras          = [logTitle >>= return . fmap formatFocused]
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
    , layoutHook  = spacingWithEdge 3 $ customLayoutHook
    , manageHook  = customManageHook
    , startupHook = customStartupHook
    , borderWidth = 2
    , focusedBorderColor = tmmagenta
    }
  `additionalKeysP`
    [ ("M-f", spawn "firefox")
    , ("<XF86Calculator>", spawn "xterm -e python3")    
    
    , ("M-<Print>", unGrab *> spawn "cd ~/Screenshots ; scrot")
    , ("M-S-<Print>", unGrab *> spawn "cd ~/Screenshots ; scrot -s")
    
    , ("<XF86MonBrightnessUp>",   spawn "lux -a 5%")
    , ("<XF86MonBrightnessDown>", spawn "lux -s 5%")
    ]

customLayoutHook = threeColLayout ||| tiledLayout ||| Mirror tiledLayout ||| Full
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
  spawnOnce "feh --bg-fill --nofehbg ~/.wallpaper/current"

tmmagenta :: String
tmmagenta = "#FF44AA"
