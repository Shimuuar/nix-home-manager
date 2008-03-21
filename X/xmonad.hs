--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
 
import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 
import XMonad.Hooks.DynamicLog

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launch a terminal
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)
    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next/prev window
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_Left  ), windows W.focusDown)      
    , ((modMask,               xK_Right ), windows W.focusUp  )      
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modMask,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modMask,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next/prev window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink/expand the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)
    -- Inc/dec the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))
    -- toggle the status bar gap
    , ((modMask              , xK_b     ),
          modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i
                             in if n == x then (0,0,0,0) else x))

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modMask              , xK_q     ),
          broadcastMessage ReleaseResources >> restart "xmonad" True)
    ]
    ++
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    -- 
    -- MPD keybindings 
    [ ((modMask,               xK_Page_Down ), spawn "mpc next > /dev/null")
    , ((modMask,               xK_Page_Up   ), spawn "mpc prev > /dev/null")
    , ((modMask,               xK_End       ), spawn "mpc toggle > /dev/null")
    , ((modMask,               xK_Home      ), spawn "mpc stop > /dev/null")
    , ((modMask,               xK_Insert    ), spawn "mpc play > /dev/null")
    , ((modMask .|. shiftMask, xK_Delete    ), spawn "mpc del 0 > /dev/null")
    ]
    ++
    --
    -- App shorcuts
    [ ((modMask .|. mod1Mask, xK_e  ), spawn "emacs22")
    , ((modMask .|. mod1Mask, xK_i  ), spawn "iceweasel")
    , ((modMask .|. mod1Mask, xK_k  ), spawn "konqueror")
    ]
    ++
    [ ((modMask .|. mod1Mask, xK_a ), spawn "apod_show_descritption.sh")
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
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = tiled ||| Mirror tiled ||| Full
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
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore 
    , resource  =? "stalonetray"    --> doIgnore
    -- Place some applications to particular desktops
    , className =? "Akregator"      --> doF (W.shift "collect")
    , className =? "psi"            --> doF (W.shift "IM")
    ]
 
------------------------------------------------------------------------
-- Status bars and logging
 
myLogHook = dynamicLogWithPP $ PP { 
              ppCurrent         = wrap "  [" "]"
            , ppVisible         = wrap "<" ">"
            , ppHidden          = const ""
            , ppHiddenNoWindows = const ""
            , ppUrgent          = id
            , ppSep             = " "
            , ppWsSep           = " "
            , ppTitle           = escape
            , ppLayout          = wrap "| " " |"
            , ppOrder           = id
            , ppOutput          = putStrLn
            }
    where
      escape = concatMap (\x -> if x == '^' then "^^" else [x])
      
{-
      ppLayout   = dzenColor "black" "#cccccc" .
                   (\ x -> case x of
                             "TilePrime Horizontal" ->
                                 " ^i(/home/emertens/images/tile_horz.xpm) "
                             "TilePrime Vertical"   ->
                                 " ^i(/home/emertens/images/tile_vert.xpm) "
                             "Hinted Full"          ->
                                 " ^i(/home/emertens/images/fullscreen.xpm) "
                             _                      -> pad x
                   )
-}

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
-- 
-- Run xmonad with the settings you specify.
--
main = xmonad defaults

defaults = defaultConfig {
      -- simple stuff
        terminal           = "urxvt",
        modMask            = mod4Mask,
        focusFollowsMouse  = True,
        borderWidth        = 1,
        workspaces         = ["work.1","work.2","3","4","5","6","fox","collect","IM"],
        normalBorderColor  = "#dddddd",
        focusedBorderColor = "#ff0000",
        defaultGaps        = [(36,0,0,0)],
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook
    }
