import XMonad

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
import XMonad.Hooks.RefocusLast

import XMonad.Util.Hacks

import XMonad.Prompt
import qualified XMonad.Prompt.Shell as Shell

import qualified XMonad.StackSet as W

import System.Directory (listDirectory)
import System.IO (readFile')
import System.Posix.Types (ProcessID)
import System.Posix.Process (createSession, executeFile, forkProcess)
import System.Posix.Signals (sigTERM, signalProcessGroup)

import qualified Data.Map as M

import Codec.Binary.UTF8.String (encodeString)

import Control.Exception (SomeException, try)
import Control.Monad (liftM)

main :: IO ()
main = xmonad
     . javaHack -- javax.swing assumes all wms reparent unless told otherwise and misbehaves wildly, tell it we don't
     . ewmh
     . docks
     . unclutter
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
    { modMask            = mod4Mask -- Use <Super> as modifier key
    , layoutHook         = avoidStruts $ spacingWithEdge 3 $ customLayoutHook
    , manageHook         = customManageHook
    , startupHook        = customStartupHook
    , borderWidth        = 2
    , focusedBorderColor = tmmagenta
    , terminal           = "alacritty"
    , keys               = customKeys
    , mouseBindings      = customMouseBindings
    }

customKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
customKeys XConfig
    { modMask    = modMask
    , terminal   = terminal
    , workspaces = workspaces
    , layoutHook = layoutHook
    } = M.fromList $ map (\(key, description, action) -> (key, action)) keys
                  ++ [ ((modMask .|. mask, key), windows $ fn i)
                        | (i, key)   <- zip workspaces [xK_1 .. xK_9]
                        , (mask, fn) <- [ (0,        W.greedyView)
                                        , (mod1Mask, \i -> W.greedyView i
                                                         . W.shift i)
                                        ]]
  where
    keys :: [((KeyMask, KeySym), String, X ())]
    keys =
        [ ( (modMask .|. shiftMask,   xK_Return)
          , "launch terminal emulator"
          , safeSpawn terminal []
          )
        , ( (modMask .|. shiftMask,   xK_c)
          , "close focused window"
          , kill
          )

        , ( (modMask,                 xK_space)
          , "cycle through available layouts"
          , sendMessage NextLayout)
        , ( (modMask .|. shiftMask,   xK_space)
          , "reset available layouts"
          , setLayout layoutHook)

        , ( (modMask,                 xK_m)
          , "focus the main window"
          , windows W.focusMaster
          )
        , ( (modMask,                 xK_j)
          , "focus the next window"
          , windows W.focusDown)
        , ( (modMask,                 xK_k)
          , "focus the previous window"
          , windows W.focusUp)

        , ( (modMask,                 xK_Return)
          , "swap the focused window with the main window"
          , windows W.swapMaster
          )
        , ( (modMask .|. shiftMask,   xK_j)
          , "swap the focused window with the next window"
          , windows W.swapDown
          )
        , ( (modMask .|. shiftMask,   xK_k)
          , "swap the focused window with the previous window"
          , windows W.swapUp
          )

        , ( (modMask,                 xK_h)
          , "shrink the main area"
          , sendMessage Shrink
          )
        , ( (modMask,                 xK_l)
          , "expand the main area"
          , sendMessage Expand
          )

        , ( (modMask,                 xK_t)
          , "tile/float the focused window"
          , withFocused $ \window ->
                          floatLocation window
                      >>= \(_, location) ->
                          windows $ \stackset ->
                                    if M.member window (W.floating stackset) then
                                        W.sink window stackset
                                    else
                                        W.float window location stackset)

        , ( (modMask,                 xK_q)
          , "recompile and restart xmonad"
          , spawn "type xmonad && xmonad --recompile && xmonad --restart"
          )

        , ( (modMask,                 xK_slash)
          , "launch an application"
          , customShellPrompt "launch: " customPromptConfig
                >>= flip whenJust
                    (\x -> safeSpawn "/bin/bash"
                        [ "-c", "exec " ++ x
                        ])
          )
        , ( (modMask .|. controlMask, xK_slash)
          , "launch an application in terminal emulator"
          , customShellPrompt "launch (in term): " customPromptConfig
                >>= flip whenJust
                    (\x -> safeSpawn terminal
                        [ "-e", "/bin/bash"
                        , "-c", "exec " ++ x
                        ])
          )

        , ( (modMask,                 xK_f)
          , "launch firefox"
          , safeSpawnProg "firefox"
          )

        , ( (modMask .|. shiftMask,   xK_f)
          , ""
          , customFirefoxPrompt customPromptConfig
          )

        , ( (noModMask,               stringToKeysym "XF86Calculator")
          , "launch python3 in terminal emulator"
          , runInTerm "" "python3"
          )

        , ( (modMask,                 xK_Print)
          , "take a screenshot"
          , unGrab *> safeSpawn "scrot" ["--file", scrotFormat]
          )
        , ( (modMask .|. shiftMask,   xK_Print)
          , "take a screenshot of a selected screen region"
          , unGrab *> safeSpawn "scrot" ["--file", scrotFormat, "--select"]
          )

        , ( (modMask .|. shiftMask,   xK_s)
          , "lock the screen"
          , safeSpawnProg "slock"
          )

        , ( (noModMask,               stringToKeysym "XF86MonBrightnessUp")
          , "increase brightness"
          , liftIO $ getBacklightDir
                >>= \backlight ->
                    adjustBacklight backlight $ perceptual ( 0.05 +)
          )
        , ( (noModMask,               stringToKeysym "XF86MonBrightnessDown")
          , "decrease brightness"
          , liftIO $ getBacklightDir
                >>= \backlight ->
                    adjustBacklight backlight $ perceptual (-0.05 +)
          )

        , ( (noModMask,               stringToKeysym "XF86AudioMute")
          , "(un)mute audio"
          , safeSpawn "pamixer" ["-t"]
          )
        , ( (noModMask,               stringToKeysym "XF86AudioLowerVolume")
          , "decrease volume"
          , safeSpawn "pamixer" ["-d", "1"]
          )
        , ( (noModMask,               stringToKeysym "XF86AudioRaiseVolume")
          , "increase volume"
          , safeSpawn "pamixer" ["-i", "1"]
          )
        , ( (shiftMask,               stringToKeysym "XF86AudioRaiseVolume")
          , "increase volume, disregarding cap"
          , safeSpawn "pamixer" ["-i", "1", "--allow-boost"]
          )
        ]

customMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
customMouseBindings XConfig
    { modMask = modMask
    } = M.fromList $
    [ ( (modMask, button1)
      , \window -> focus window
                >> float window
                >> windows W.shiftMaster
                >> mouseMoveWindow window
      )
    , ( (modMask, button3)
      , \window -> focus window
                >> float window
                >> windows W.shiftMaster
                >> mouseResizeWindow window
      )
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
  -- NOTE: The composition order is super finicky and will misbehave if altered
    [ isFloat <||> willFloat --> doF W.swapUp <+> doCenterFloat
    , isDialog               --> doCenterFloat
    ,                            insertPosition Below Newer
    ]

customStartupHook :: X ()
customStartupHook = do
  runProcessWithInput "feh" ["--bg-fill", "--no-fehbg", ".wallpaper/current"] "" *> return ()

  -- -- Prompts -- --
customPromptConfig :: XPConfig
customPromptConfig = def
    { font              = "xft:Fira Code SemiBold-12"
    , bgColor           = tmblack
    , fgColor           = tmwhite
    , bgHLight          = tmmagenta
    , fgHLight          = tmblack
    , borderColor       = tmmagenta
    , promptBorderWidth = 2
    , position          = Bottom
    , height            = 30
    , historySize       = 0
    , maxComplRows      = Just 15
    , changeModeKey     = 0
    }

data CustomShellPrompt = CustomShellPrompt String

instance XPrompt CustomShellPrompt where
    showXPrompt (CustomShellPrompt label) = label
    completionToCommand _ = foldr (\c cs -> if c `elem` " \"#$&'()*;?@[\\]{}" then
                                                '\\' : c : cs
                                            else
                                                c : cs
                                  ) ""

customShellPrompt :: String -> XPConfig -> X (Maybe String)
customShellPrompt label config = do
    cmds <- io Shell.getCommands
    mkXPromptWithReturn (CustomShellPrompt label) config (Shell.getShellCompl cmds $ searchPredicate config) return

data CustomFirefoxPrompt = CustomFirefoxPrompt

instance XPrompt CustomFirefoxPrompt where
  showXPrompt    CustomFirefoxPrompt = "Firefox: "
  nextCompletion CustomFirefoxPrompt = getNextCompletion

customFirefoxPrompt :: XPConfig -> X ()
customFirefoxPrompt config =
    runProcessWithInput "sed" ["/Name=/!d;s/Name=//", ".mozilla/firefox/profiles.ini"] ""
    >>= \profiles ->
        mkXPrompt CustomFirefoxPrompt
                  config
                  (mkComplFunFromList' config $ split (== '\n') profiles)
                  (safeSpawn "firefox" . ("-P" :) . (: []))

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
