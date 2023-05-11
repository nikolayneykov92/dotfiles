import XMonad
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Spacing
import XMonad.Util.SpawnOnce
import XMonad.Util.Run
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm, xK_space), sendMessage NextLayout)
    , ((modm, xK_p), spawn "dmenu_run -fn 'FiraCode Nerd Font-11' -nf '#839496' -nb '#002b36' -sf '#002b36' -sb '#d33682'")
    , ((modm, xK_r), spawn "xmonad --recompile; xmonad --restart")
    , ((modm, xK_q), kill)
    , ((modm, xK_j), windows W.focusDown)
    , ((modm, xK_k), windows W.focusUp)
    , ((modm, xK_m), windows W.focusMaster)
    , ((modm, xK_h), sendMessage Shrink)
    , ((modm, xK_l), sendMessage Expand)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    , ((modm .|. shiftMask,               xK_m), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    , ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    , ((0, xF86XK_MonBrightnessUp), spawn "lux -a 10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "lux -s 10%")
    , ((mod1Mask, xK_Shift_L), spawn "setxkbmap us")
    , ((mod1Mask, xK_Shift_R), spawn "setxkbmap bg phonetic")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

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
myLayout = avoidStruts $ spacingWithEdge 4 (tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 1/100

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
myManageHook = mempty

  ------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


main = do
  h <- spawnPipe "xmobar -x 0 ~/.config/xmonad/xmobar.conf"
  xmonad $ docks def {
    modMask = mod4Mask,
    terminal = "alacritty",
    clickJustFocuses = True,
    focusFollowsMouse = False,
    workspaces         = ["1", "2", "3", "4", "5"],
    borderWidth        = 2,
    normalBorderColor  = "#002b36",
    focusedBorderColor = "#002b36",
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    layoutHook         = myLayout,
    manageHook         = manageSpawn <+> myManageHook,
    handleEventHook    = myEventHook,
    logHook            = dynamicLogWithPP $ xmobarPP {
        ppOutput = hPutStrLn h,
        ppCurrent = \(ws) -> "<box type=Bottom width=2 mb=2 color=#d33682><fc=#d33682> " ++ ws ++ " </fc></box>",
        ppHiddenNoWindows = \(ws) -> "<box type=Bottom width=2 mb=2 color=#839496><fc=#839496> " ++ ws ++ " </fc></box>",
        ppHidden = \(ws) -> "<box type=Bottom width=2 mb=2 color=#839496><fc=#839496> " ++ ws ++ " </fc></box>",
        ppVisible =  \(ws) -> "<box type=Bottom width=2 mb=2 color=#839496><fc=#839496> " ++ ws ++ " </fc></box>",
        ppOrder = \(ws:_) -> [ws]
        },
        startupHook = do
                        spawnOnce "picom -b &"
                        spawnOnce "xset s off &"
                        spawnOnce "xset s 0 0 &"
                        spawnOnce "xset -dpms &"
                        spawnOnce "nitrogen --restore &"
                        spawnOnce "xinput set-prop 13 344 1 &"
                        spawnOnce "xinput set-prop 13 365 1 &"
                        spawnOnce "setxkbmap -option ctrl:nocaps &"
                        spawnOnce "xsetroot -cursor_name left_ptr &"
    }
