
import Codec.Binary.UTF8.String

import qualified Data.Map as M
import Data.Ratio ((%))

import System.Exit
import System.IO

-- XMonad part ----------------
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.Submap

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks

import qualified XMonad.Layout.IM as IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect

import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig

import XMonad.Prompt

----------------------------------------------------------------

-- | Data for XPrompt 
data XPDict = XPDict
instance XPrompt XPDict where
    showXPrompt = const "Посмотреть слово: "
-- | Look wod in dictionary 
lookupDictionary :: XPConfig -> X ()
lookupDictionary config = mkXPrompt XPDict config (return . const []) 
    ((\x -> spawnU $ "(echo "++x++"; dict "++x++") | dzen_less") . shellEscape)


-- | Escapes all shell metacharacters.
shellEscape :: String -> String 
shellEscape = concatMap (\x -> if x `elem` " ;$!@#%&|<>" then '\\':[x] else [x])
 
-- | Unicode safe spawn 
spawnU :: MonadIO m => String -> m ()
spawnU = spawn . encodeString

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf = 
  let upperKeys = ["1","2","3","4","5","6","7","8","9","0","-","="]
      -- Make pair of move/shift to workspace keybindings
      makeShiftPair :: String -> (String, String) -> [(String, X())]
      makeShiftPair p (k, name) = [ (p++k,       windows $ W.greedyView name)
                                  , (p++"S-"++k, windows $ W.shift name) ]
  in mkKeymap conf $
    -- Move/switch to workspace
    ((zip upperKeys $ take 10 $ XMonad.workspaces conf) >>= makeShiftPair "M-")
    ++
    -- More move/switch to workspace
    [ ("M-w", windows $ W.greedyView "WWW")
    , ("M-a", submap $ mkKeymap conf $ 
            [ ("w", "WWW")
            , ("r", "RSS")
            , ("m", "Mail")
            , ("i", "IM")
            , ("t", "Torrent")
            , ("a", "Audio")
            , ("x", "Media")
            , ("g", "Gimp")
            ] >>= makeShiftPair ""
      )
    -- Quit XMonad
    , ("M-S-q"       , io (exitWith ExitSuccess))
    -- Restart XMonad
    , ("M-q"         , broadcastMessage ReleaseResources >> restart "xmonad" True)
    -- Run termnal emulator 
    , ("M-S-<Return>", spawn $ XMonad.terminal conf)
    -- Run menu
    , ("M-p"         , spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- Close focused window 
    , ("M-S-c"       , kill)
    -- Rotate through the available layout algorithms
    , ("M-<Space>"   , sendMessage NextLayout)
    -- Reset the layouts on the current workspace to default
    , ("M-S-<Space>" , setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size 
    , ("M-n"         , refresh)
    -- Toggle struts on/off
    , ("M-b"         , sendMessage ToggleStruts)
    -- Move focus to the next/prev window
    , ("M-<Tab>"     , windows W.focusDown)
    , ("M-<Left>"    , windows W.focusDown)
    , ("M-j"         , windows W.focusDown)
    , ("M-<Right>"   , windows W.focusUp)
    , ("M-k"         , windows W.focusUp)
    -- Move focus to the master window
    , ("M-m"         , windows W.focusMaster)
    -- Swap the focused window and the master window
    , ("M-<Return>"  , windows W.swapMaster)
    -- Swap the focused window with the next/prev window
    , ("M-S-j"       , windows W.swapDown)
    , ("M-S-k"       , windows W.swapUp)
    -- Shrink/expand the master area
    , ("M-h"         , sendMessage Shrink)
    , ("M-l"         , sendMessage Expand)
    -- Push window back into tiling
    , ("M-t"         , withFocused $ windows . W.sink)
    -- Inc/dec the number of windows in the master area
    , ("M-,"          , sendMessage (IncMasterN 1))
    , ("M-."          , sendMessage (IncMasterN (-1)))

    -- MPD keybindings 
    , ("M-<Page_Down>"   , spawn "mpc next   > /dev/null")
    , ("M-<Page_Up>"     , spawn "mpc prev   > /dev/null")
    , ("M-<End>"         , spawn "mpc toggle > /dev/null")
    , ("M-<Home>"        , spawn "mpc stop   > /dev/null")
    , ("M-<Insert>"      , spawn "mpc play   > /dev/null")
    , ("M-<Delete>"      , submap $ mkKeymap conf $
            [ ("<Delete>"    , spawn "mpc del 0  > /dev/null")
            , ("S-<Delete>"  , spawn "mpc clear  > /dev/null") ]
       )
    -- Applications shortcuts
    , ("M-M1-e"  , spawn "emacs")
    , ("M-M1-i"  , spawn "iceweasel")
    , ("M-i"     , spawn "iceweasel \"$(xsel)\"")
    , ("M-M1-k"  , spawn "konqueror")
    , ("M-M1-w"  , spawn "kdesu wireshark")
    , ("M-s"     , scratchpadSpawnActionCustom "xterm -name scratchpad -e sh -c 'screen -d -R scratch'")
    -- Useful action 
    , ("M-M1-a" , spawn "fmt ~/.local/share/apod/description | dzen_less")
    , ("M-d"    , spawn "look_dictionary | dzen_less")
    , ("M-S-d"  , lookupDictionary myXPconfig )
    , ("M-z"    , spawn "dzen_less -e < ~/.xsession-errors")

    ]
  
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Events:
myHandleEventHook = ewmhDesktopsEventHook
 
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
myLayout = smartBorders $
           avoidStruts  $ 
           onWorkspace "IM" (IM.withIM (1%5) (IM.Resource "main") defaultLayout) $
           onWorkspace "Gimp" gimp $ 
           defaultLayout
    where
      -- Default layout
      defaultLayout = tiled ||| Mirror tiled ||| Full
      -- Simple tiled layout 
      tiled   = Tall 1 (1/2) (3/100)
      -- Layout for GIMP
      gimp = IM.withIM (1%5) (IM.Role "gimp-toolbox") $ reflectHoriz $ 
             IM.withIM (1%5) (IM.Role "gimp-dock") (Full ||| tiled)

 
------------------------------------------------------------------------
-- Window rules:
-- 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll $ concat [  
    -- Float dialogs
    [isDialog --> doFloat],
    -- Floating windows 
    map (className ?-> doCenterFloat) ["XDosEmu", "feh"],
    -- Media windows
    map (className ?-> doMedia)       ["MPlayer", "wesnoth"],
    -- Ignored windows 
    map (className ?-> doIgnore)      ["stalonetray", "trayer", "fbpanel", "xfce4-panel", "Xfce4-panel"],
    map (resource  ?-> doIgnore)      ["desktop_window", "kdesktop"],
    -- Windows placement hooks
    map (className ?->> "WWW")        ["Iceweasel", "Firefox-bin", "Firefox"],
    map (className ?->> "RSS")        ["Akregator"],
    map (className ?->> "IM")         ["psi"],
    map (className ?->> "Mail")       ["Kmail"],
    map (className ?->> "Torrent")    ["Ktorrent","Deluge"],
    map (className ?->> "Audio")      ["Sonata"],
    map (className ?->> "Gimp")       ["Gimp"],
    -- Scratchpad hook
    [ scratchpadManageHook $ W.RationalRect (1%8) (1%6) (6%8) (2%3) ]
    ]
    where 
      -- Apply manage hook
      (?->) :: Query String -> ManageHook -> String -> ManageHook
      (?->) q hook name = q =? name --> hook
      -- Shift window to workspace
      (?->>) :: Query String -> String -> String -> ManageHook
      q ?->> tgt = q ?-> doF (W.shift tgt)
      -- Hook for multimedia related windows
      doMedia :: ManageHook
      doMedia = doF (W.greedyView "Media" . W.shift "Media") <+> doFullFloat

------------------------------------------------------------------------
-- XPromt settings 
myXPconfig = defaultXPConfig { 
               font = "-xos4-terminus-medium-r-normal-*-16-160-*-*-*-*-iso10646-*" 
             }


----------------------------------------------------------------
-- XMonad config
myConfig = defaultConfig {
      -- simple stuff
      terminal           = "my-terminal",  -- Supposed to be symlink to actual terminal emulator
      modMask            = mod4Mask,
      focusFollowsMouse  = True,
      borderWidth        = 1,
      workspaces         = (map show [1..10]) ++ 
                           ["WWW","RSS","Mail","IM","Torrent","Audio","Media","Gimp"],
      normalBorderColor  = "#dddddd",
      focusedBorderColor = "#ff0000",
      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,
      -- hooks, layouts
      handleEventHook    = myHandleEventHook,
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = ewmhDesktopsLogHookCustom (take 10)
      }


------------------------------------------------------------------------
-- Run xmonad
main = xmonad myConfig
