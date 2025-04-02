{-# LANGUAGE ApplicativeDo #-}
import Codec.Binary.UTF8.String
import Data.Monoid (All(..))
import Data.List   (isInfixOf)
import qualified Data.Map as M
import Data.Ratio            ((%))

import System.Exit

-- XMonad part ----------------
import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.Search (SearchEngine, searchEngine, promptSearchBrowser, selectSearchBrowser,
                              google, youtube, wikipedia, scholar, hoogle, hackage, escape)
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicProperty

import XMonad.Layout.IM (withIM)
import XMonad.Layout.PerWorkspace
import XMonad.Layout.NoBorders    (smartBorders,noBorders)
import XMonad.Layout.Reflect
import XMonad.Layout.ComboP
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid

import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig
import XMonad.Util.Run
-- import XMonad.Util.Hacks (fixSteamFlicker)
import XMonad.Prompt

----------------------------------------------------------------

-- | Move window to workspace
doWorkspace :: String -> ManageHook
doWorkspace = doF . W.shift

-- | Hook for multimedia related windows
doMedia :: ManageHook
doMedia = doF (W.greedyView "Media" . W.shift "Media")
       <> doFullFloat

run :: MonadIO m => String -> m ()
run = safeSpawnProg

wikipediaLang' :: String -> SearchEngine
wikipediaLang' lang = searchEngine (lang++".wiki") ("https://secure.wikimedia.org/wikipedia/"++lang++"/wiki/Special:Search?go=Go&search=")

-- Search string from XMonad.Actions.Search is broken
hoogle' :: String -> SearchEngine
hoogle' scope = searchEngine "hooogle"
  ("https://hoogle.haskell.org/?scope="++escape scope++"&hoogle=")

duckduckgo :: SearchEngine
duckduckgo = searchEngine "duckduckgo" "https://duckduckgo.com/?q="

mySearch :: (String, SearchEngine) -> [(String, X ())]
mySearch (key , engine) = [ (key      , promptSearchBrowser myXPConfig browser engine)
                          , ("M-"++key, selectSearchBrowser            browser engine)
                          ]
    where browser = "firefox"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = mkKeymap conf
  $ -- Move/switch to workspace
    ((upperKeys `zip` XMonad.workspaces conf) >>= makeShiftPair "M-")
  ++
    -- More move/switch to workspace
    [ ("M-w", windows $ W.view "WWW")
    , ("M-a", submap $ mkKeymap conf $
            [ ("w", "WWW")
            , ("i", "IM")
            , ("t", "Torrent")
            , ("x", "Media")
            , ("m", "e-Mail")
            , ("k", "gitk")
            , ("d", "TODO")
            , ("z", "Zettel")
            , ("a", "Audio")
            ] >>= makeShiftPair ""
      )
      -- Quit XMonad
    , ("M-S-q"       , io (exitWith ExitSuccess))
      -- Restart XMonad
    , ("M-q"         , broadcastMessage ReleaseResources >> restart "xmonad" True)
      -- Run termnal emulator
    , ("M-S-<Return>", run $ XMonad.terminal conf)
      -- Run menu
    , ("M-p"         , run "dmenu_run")
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
    , ("M-S-<Left>"  , prevScreen)
    , ("M-S-<Right>" , nextScreen)
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
    -- Sound
    , ("M-<Up>"     , spawn "amixer set Master 2%+")
    , ("M-<Down>"   , spawn "amixer set Master 2%-")
    -- MPD keybindings
    , ("M-<Page_Down>"   , spawn "mpc next   > /dev/null")
    , ("M-<Page_Up>"     , spawn "mpc prev   > /dev/null")
    , ("M-<End>"         , spawn "mpc toggle > /dev/null")
    , ("M-<Home>"        , spawn "mpc stop   > /dev/null")
    , ("M-<Insert>"      , spawn "mpc play   > /dev/null")
    , ("M-<Delete>"      , submap $ mkKeymap conf $
            [ ("<Delete>"    , spawn "mpc del 0   > /dev/null")
            , ("S-<Delete>"  , spawn "mpc clear   > /dev/null")
            , ("r"           , spawn "mpc repeat  > /dev/null")
            , ("x"           , spawn "mpc random  > /dev/null")
            , ("S-x"         , spawn "mpc shuffle > /dev/null")
            , ("1"           , spawn "mpc single  > /dev/null")
            ] )
    -- Applications shortcuts
    , ("M-M1-e"  , run "emacs")
    , ("M-M1-i"  , run "firefox")
    , ("M-M1-k"  , run "konqueror")
    , ("M-s"     , scratchpadSpawnActionCustom "exec xterm -name scratchpad -e sh -c 'screen -d -R scratch'")
    , ("<Print>" , run "spectacle")
    -- Search
    , ("M-g"     , submap $ mkKeymap conf $ concatMap mySearch
          [ ("g"  , google )
          , ("a"  , searchEngine "Яндекс" "http://yandex.ru/yandsearch?text=")
          , ("d"  , duckduckgo)
          , ("s"  , scholar)
          , ("y"  , youtube)
          , ("h"  , hoogle' "set:stackage")
          , ("w"  , wikipedia )
          , ("r"  , wikipediaLang' "ru")
          , ("S-h", hackage)
          ] )
    -- Emullation of middle mouse click.
    , ("M-z", safeSpawn "xdotool" ["keyup", "Super_L", "click", "2"])
    -- Useful action
    , ("M-x"     , submap $ mkKeymap conf $
          [ ("l"  , spawn "xscreensaver-command --lock")
          ] )
    ]
  where
    upperKeys = ["1","2","3","4","5","6","7","8","9","0"]
    -- Make pair of move/shift to workspace keybindings
    makeShiftPair :: String -> (String, String) -> [(String, X())]
    makeShiftPair p (k, name) = [ (p++k,       windows $ W.view name)
                                , (p++"S-"++k, windows $ W.shift name)
                                ]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
  , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
  , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
  ]


------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
myLayout
  = smartBorders
  $ avoidStruts
  $ onWorkspace "Media"   (noBorders Full)
  $ onWorkspace "IM"      Full
  $ onWorkspace "gitk"    Full
  $ defaultLayout
  where
    -- Default layout
    defaultLayout =  tiled
                 ||| Mirror tiled
                 ||| Full
                 where tiled = Tall 1  0.02  0.50


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
myManageHook :: ManageHook
myManageHook = mconcat $ concat
  [ -- Specific hooks
    [ isDialog --> doFloat
    -- Builtin hook stopped working. See https://github.com/xmonad/xmonad-contrib/issues/756
    , appName =? "scratchpad" --> doRectFloat (W.RationalRect (1%8) (1%6) (6%8) (2%3))
    , (do dialog <- isDialog
          kdiff  <- className =? "kdiff3"
          return $! kdiff
      ) --> (doF (W.greedyView "10" . W.shift "10"))
    , className =? "Gitk"    --> doWorkspace "gitk"
    ]
    -- Ignored windows
  , [ title     =? "plasma-desktop"
    , className =? "xfce4-panel"
    , className =? "Xfce4-Panel"
    , className =? "lxpanel"
    ] ==> doIgnore
    -- Floating windows
  , [ className =? "feh"
    , className =? "spectacle"
    , resource  =? "terminal-float"
    ] ==> doCenterFloat
    -- Media programs
  , [ className =? "MPlayer"
    , className =? "mpv"
    , className =? "mplayer2"
    , className =? "wesnoth"
    , className =? "Steam"
    ] ==> doMedia
  , [ className =? "Sonata"
    ] ==> doWorkspace "Audio"
    -- Windows placement hooks
  , [ appName =? "emacs-todo"
    ] ==> doWorkspace "TODO"
  , [ appName =? "emacs-zettel"
    ] ==> doWorkspace "Zettel"
    --
  , [ className =? "Firefox-bin"
    , className =? "Firefox-esr"
    , className =? "Firefox"
    ] ==> doWorkspace "WWW"
    --
  , [ className =? "psi"
    , className =? "Pidgin"
    , className =? "TelegramDesktop"
    ] ==> doWorkspace "IM"
    --
  , [ className =? "Ktorrent"
    , className =? "Deluge"
    ] ==> doWorkspace "Torrent"
    --
  , [ resource =? "Mail"
    ] ==> doWorkspace "e-Mail"
  ]
  where
    queries ==> hook = [ q --> hook | q <- queries ]

------------------------------------------------------------------------
-- XPromt settings
myXPConfig :: XPConfig
myXPConfig = def
  { font        = "-*-fixed-medium-*-*-*-18-*-*-*-*-*-iso10646-*"
  , historySize = 20
  , height      = 22
  , bgColor     = "gray10"
  }


----------------------------------------------------------------
-- XMonad config
myConfig
  = addEwmhWorkspaceSort (pure scratchpadFilterOutWorkspace)
  $ ewmhFullscreen
  $ ewmh
  $ docks
  $ def
  { terminal           = "konsole"
  , modMask            = mod4Mask
  , focusFollowsMouse  = True
  , borderWidth        = 1
  , workspaces         = (map show [1..10]) ++
                         ["WWW","IM","Torrent","Media","e-Mail","gitk","TODO","Zettel","Audio"]
  , normalBorderColor  = "#dddddd"
  , focusedBorderColor = "#ff0000"
    -- key bindings
  , keys               = myKeys
  , mouseBindings      = myMouseBindings
    -- hooks, layouts
  , layoutHook         = myLayout
  , manageHook         = myManageHook
                      <> manageDocks
    -- Steam
  -- , handleEventHook    = fixSteamFlicker
  }


------------------------------------------------------------------------
-- Run xmonad
main :: IO ()
main = xmonad myConfig
