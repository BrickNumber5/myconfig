import XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.SpawnOnce
import qualified XMonad.Util.Dmenu as Dmenu
import XMonad.Util.Run
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.InsertPosition

import XMonad.Util.Hacks

import System.Directory (listDirectory)
import System.IO (readFile')
import System.Posix.Types (ProcessID)
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Signals (sigTERM, signalProcessGroup)

import Codec.Binary.UTF8.String (encodeString)

import Control.Exception (SomeException, try)

main :: IO ()
main = xmonad
     . javaHack -- javax.swing assumes all wms reparent unless told otherwise and misbehaves wildly, tell it we don't
     . ewmh
     . docks
     . unclutter
     . addTray
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
    , manageHook  = insertPosition Below Newer <+> customManageHook
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
    
    , ("M-<Print>",   unGrab *> safeSpawn "scrot" ["--file", scrotFormat])
    , ("M-S-<Print>", unGrab *> safeSpawn "scrot" ["--file", scrotFormat, "--select"])
    
    , ("M-S-s", safeSpawnProg "slock")
    
    , ("<XF86MonBrightnessUp>",   liftIO $ getBacklightDir >>= \b -> adjustBacklight b $ perceptual ( 0.05 +))
    , ("<XF86MonBrightnessDown>", liftIO $ getBacklightDir >>= \b -> adjustBacklight b $ perceptual (-0.05 +))
    
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
  runProcessWithInput "feh" ["--bg-fill", "--no-fehbg", ".wallpaper/current"] "" *> return ()

  -- -- Trayer -- --
newtype SavedTrayerPID = SavedTrayerPID { getPID :: Maybe ProcessID }
  deriving (Show, Read)

instance ExtensionClass SavedTrayerPID where
  initialValue  = SavedTrayerPID Nothing
  extensionType = PersistentExtension

  -- Add trayer removing any previously existing trayer
addTray :: XConfig a -> XConfig a
addTray cfg = cfg
    { startupHook = do
        startupHook cfg
        XS.gets getPID >>= flip whenJust (io . killPID)
        pid <- safeSpawnPID "trayer"
            [ "--edge", "bottom"
            , "--align", "right"
            , "--SetDockType", "true"
            , "--SetPartialStrut", "true"
            , "--expand", "true"
            , "--width", "15"
            , "--transparent", "true"
            , "--tint", "0x000000"
            , "--height", "30"
            , "--alpha", "128"
            ]
        XS.put $ SavedTrayerPID $ Just pid
    }

  -- -- Unclutter -- --
newtype SavedUnclutterPID = SavedUnclutterPID { getUnclutterPID :: Maybe ProcessID }
  deriving (Show, Read)

instance ExtensionClass SavedUnclutterPID where
  initialValue  = SavedUnclutterPID Nothing
  extensionType = PersistentExtension

  -- Activate unclutter removing any previously existing unclutter
unclutter :: XConfig a -> XConfig a
unclutter cfg = cfg
    { startupHook = do
        startupHook cfg
        XS.gets getUnclutterPID >>= flip whenJust (io . killPID)
        pid <- safeSpawnPID "unclutter"
            [ "--jitter", "15"
            , "--ignore-buttons", "4,5,6,7"
            , "--start-hidden"
            , "--timeout", "1"
            ]
        XS.put $ SavedUnclutterPID $ Just pid
    }

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

 -- Alteration of XMonad.Util.Run.safeSpawn to return the process id
safeSpawnPID :: MonadIO m => FilePath -> [String] -> m ProcessID
safeSpawnPID prog args = io $ forkProcess $ do
  uninstallSignalHandlers
  _ <- createSession
  executeFile (encodeString prog) True (map encodeString args) Nothing

 -- Utlity to kill a process by PID
 -- Mostly the same as XMonad.Hooks.StatusBar which uses this internally
 -- (for more or less exactly the same reason we want it) but does not
 -- export it.
killPID :: ProcessID -> IO ()
killPID pid = try @SomeException (signalProcessGroup sigTERM pid) *> return ()

 -- Utilities for backlight management
backlightSearchDir :: String
backlightSearchDir =  "/sys/class/backlight"

getBacklightDir :: IO String
getBacklightDir =  do dirs <- listDirectory backlightSearchDir
                      case dirs of
                          []    -> fail "Unable to find the backlight acpi directory"
                          dir:_ -> return $ backlightSearchDir ++ "/" ++ dir

getBacklightProp :: String -> String -> IO Integer
getBacklightProp backlight prop = readFile' (backlight ++ "/" ++ prop) >>= return . read

setBacklightProp :: String -> String -> Integer -> IO ()
setBacklightProp backlight prop val = writeFile (backlight ++ "/" ++ prop) (show val)

adjustBacklight :: String -> (Double -> Double) -> IO ()
adjustBacklight backlight f = do brightness    <- getBacklightProp backlight "brightness"
                                 maxBrightness <- getBacklightProp backlight "max_brightness"
                                 let brightnessFrac    = fromIntegral brightness / fromIntegral maxBrightness
                                 let newBrightnessFrac = f brightnessFrac
                                 let newBrightness     = round $ newBrightnessFrac * fromIntegral maxBrightness
                                 setBacklightProp backlight "brightness" newBrightness

perceptual     :: (Double -> Double) -> (Double -> Double)
perceptual f x =  let maxValue      = 100 -- Tune for "feel"
                      stretchedIn   = x * maxValue
                      perceptualIn  = case stretchedIn of 0 -> 0
                                                          x -> log x / log maxValue
                      perceptualOut = clamp 0 1 $ f perceptualIn
                      stretchedOut  = case perceptualOut of 0 -> 0
                                                            y -> exp $ y * log maxValue
                   in stretchedOut / maxValue

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo $ min hi x

 -- Format string for scrot
scrotFormat :: String
scrotFormat =  "screenshots/%Y-%m-%d_%H.%M.%S_$wx$h_$a_screenshot.png"

 -- Theme Colors
tmblack, tmgray, tmwhite, tmmagenta :: String
tmblack   = "#000000"
tmgray    = "#AAAAAA"
tmwhite   = "#FFFFFF"
tmmagenta = "#FF44AA"
