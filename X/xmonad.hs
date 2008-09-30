--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import System.Exit
import System.IO
import qualified System.IO.UTF8
import qualified Data.Map as M
import Data.Ratio ((%))

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers

import qualified XMonad.Layout.IM as IM
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Gaps
import XMonad.Layout.NoBorders

import XMonad.Util.Run
import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig



------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = let
    upperKeys = ["1","2","3","4","5","6","7","8","9","0","-","="]
 in M.union 
    ( M.fromList $ 
    -- MPD additional keybindings 
    -- XF86AudioLowerVolume
    [ ((0,                   0x1008ff11   ), spawn "mpc seek -10 > /dev/null")
    -- XF86AudioRaiseVolume
    , ((0,                   0x1008ff13   ), spawn "mpc seek +10 > /dev/null")
    -- XF86AudioPrev
    , ((0,                   0x1008ff16   ), spawn "mpc repeat > /dev/null")
    -- XF86Music
    , ((0,                   0x1008ff92   ), spawn "mpc random  > /dev/null")
    , ((controlMask,         0x1008ff92   ), spawn "mpc shuffle > /dev/null")
    ] ) 

    ( mkKeymap conf $
    -- Switch to workspace 
    [ ("M-"   ++ key, windows $ W.greedyView ws) |
      (key,ws) <- zip upperKeys (XMonad.workspaces conf) ]
    ++ 
    -- Move window to workspace
    [ ("M-S-" ++ key, windows $ W.shift ws) |
      (key,ws) <- zip upperKeys (XMonad.workspaces conf) ]
    ++
    [ -- Quit XMonad
      ("M-S-q"       , io (exitWith ExitSuccess))
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
    , ("M-S-<Delete>"    , spawn "mpc del 0  > /dev/null")
    , ("M-M1-S-<Delete>" , spawn "mpc clear  > /dev/null")

    -- Applications shortcuts
    , ("M-M1-e"  , spawn "emacs22")
    , ("M-M1-i"  , spawn "iceweasel")
    , ("M-M1-k"  , spawn "konqueror")
    , ("M-M1-w"  , spawn "kdesu wireshark")
    , ("M-s"     , spawn "urxvt -name scratchpad -e sh -c 'screen -d -R scratch'")

    -- Useful action 
    , ("M-M1-a" , spawn "fmt ~/.local/share/apod/description | dzen_less")
    , ("M-d"    , spawn "look_dictionary | dzen_less")
    , ("M-z"    , spawn "dzen_less -e < ~/.xsession-errors")
    ]
  )
  
 
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
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
myLayout = smartBorders $
           gaps [(U,18*2)] $
           onWorkspace "IM" (IM.IM (1%5) (IM.Resource "main")) $
           tiled ||| Mirror tiled ||| Full
    where
      -- default tiling algorithm partitions the screen into two panes
      tiled   = Tall nmaster delta ratio
      -- The default number of windows in the master pane
      nmaster = 1
      -- Default proportion of screen occupied by master pane
      ratio   = 1/2
      -- Percent of screen to increment by when resizing panes
      delta   = 3/100
 
------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll $ concat [
    -- Floating windows 
    [ className =? c --> doFloat       | c <- ["Gimp"]],
    [ className =? c --> doCenterFloat | c <- ["XDosEmu", "feh"]],
    [ className =? c --> doFullFloat   | c <- ["wesnoth", "MPlayer"]],
    -- Ignored windows 
    [ resource =? c --> doIgnore | c <- ["desktop_window", "kdesktop", "stalonetray"]],
    -- Other hooks 
    [ className =? "Akregator"      --> doF (W.shift "RSS")
    , className =? "psi"            --> doF (W.shift "IM")
    , className =? "Sonata"         --> doF (W.shift "Муз")
    , className =? "Ktorrent"       --> doF (W.shift "Торр")
    , className =? "Iceweasel"      --> doF (W.shift "WWW")
    -- Scratchpad hook
    , scratchpadManageHook $ W.RationalRect (1%8) (1%6) (6%8) (2%3)
    ] ]
 
------------------------------------------------------------------------
-- Status bars and logging
myLogHook h = defaultPP { 
                ppCurrent         = wrap " ^bg(blue)^fg(#eee)" "^fg()^bg()"
              , ppVisible         = wrap "<" ">"
              , ppHidden          = wrap " " ""
              , ppHiddenNoWindows = wrap " " ""
              , ppUrgent          = wrap "@" "@"
              , ppSep             = " | "
              , ppWsSep           = ""
              , ppTitle           = dzenEscape
              , ppOutput          = System.IO.UTF8.hPutStrLn h
              }
    where
      escape = concatMap (\x -> if x == '^' then "^^" else [x])


------------------------------------------------------------------------
-- Run xmonad
main = do 
  dzenh <- spawnPipe "dzen2 -ta l -y 18 -e ''"
  xmonad $ myConfig dzenh

myConfig h = defaultConfig {
      -- simple stuff
      terminal           = "urxvt",
      modMask            = mod4Mask,
      focusFollowsMouse  = True,
      borderWidth        = 1,
      workspaces         = ["1","2","WWW","Муз","Mail","Торр","7","RSS","IM","--"],
      normalBorderColor  = "#dddddd",
      focusedBorderColor = "#ff0000",
      
      -- key bindings
      keys               = myKeys,
      mouseBindings      = myMouseBindings,
      
      -- hooks, layouts
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = dynamicLogWithPP $ myLogHook h
      }
