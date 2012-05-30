
import XMonad hiding (defaultConfig)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)

import Data.Monoid
import qualified Data.Map as M

import System.IO (hPutStrLn)
import System.Exit

import Graphics.X11.ExtraTypes.XF86

main = do
    spawn $ "trayer --edge top --align right --margin 0 --width " ++ show trayWidth ++ 
        " --widthtype pixel --height 16 --padding 2 --tint 0x333333 --transparent true" ++
        " --alpha 0 --SetPartialStrut true --SetDockType true"
    dzen <- spawnPipe $ "dzen2 -h 16 " ++ " -ta left -fn '" ++ font ++ "' -fg '#ffffff' -bg '#333333'"
    xmonad $ myConfig { logHook = logHook myConfig >> dynamicLogWithPP (myDzenPP dzen) }

font = "WenQuanYi Micro Hei Mono-10"

trayWidth = 50

myConfig = XConfig
  { borderWidth        = 2
  , workspaces         = myWorkspaces
  , layoutHook         = myLayout
  , terminal           = "urxvt"
  , normalBorderColor  = "#000000"
  , focusedBorderColor = "#ff0000"
  , modMask            = modm
  , keys               = myKeys
  , logHook            = updatePointer (TowardsCentre 0.3 0.3) >> fadeInactiveCurrentWSLogHook 0.7
  , startupHook        = myStartupHook
  , mouseBindings      = myMouse
  , manageHook         = myManageHook
  , handleEventHook    = const (return (All True))
  , focusFollowsMouse  = True
  }

myDzenPP dzen = dzenPP
  { ppOutput = hPutStrLn dzen
  }

modm = mod1Mask -- Left Alt
modm2 = mod4Mask -- WinKey

myWorkspaces = ["1:web", "2:term", "3:misc" ] ++ map show [4..9]

myLayout = avoidStruts $ smartBorders $ 
    (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 6/10
    delta   = 3/100

myManageHook = composeAll $
    manageDocks :
    ( isDialog --> doCenterFloat) :
    ( isFullscreen --> doFullFloat) :
    [ className =? c  --> doFloat          | c <- ["MPlayer", "Gimp"]] ++
    [ className =? c  --> doShift "1:web"  | c <- ["Firefox", "Chromium"]] ++
    [ className =? c  --> doShift "2:term" | c <- []] ++
    [ className =? c  --> doShift "3:misc" | c <- ["Evince", "Thunar", "Vlc", "Transmission-gtk"]] ++
    []

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "dmenu_run -b -f -p \">\"")
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modm .|. shiftMask, xK_c     ), kill)

    , ((modm,               xK_space ), sendMessage NextLayout)
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modm,               xK_n     ), refresh)

    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  )
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )
    , ((modm,               xK_m     ), windows W.focusMaster  )

    , ((modm,               xK_Return), dwmpromote)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)

    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    , ((modm .|. shiftMask, xK_equal ), sendMessage (IncMasterN 1))
    , ((modm              , xK_minus ), sendMessage (IncMasterN (-1)))

    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm              , xK_q     ), spawn "killall trayer; killall dzen2; xmonad --recompile && xmonad --restart")
    , ((modm .|. shiftMask, xK_l     ), spawn "xscreensaver-command -lock")

    , ((modm,               xK_b     ), viewEmptyWorkspace)
    , ((modm .|. shiftMask, xK_b     ), tagToEmptyWorkspace)

    , ((modm,               xK_Down  ), nextWS)
    , ((modm,               xK_Up    ), prevWS)
    , ((modm .|. shiftMask, xK_Down  ), shiftToNext)
    , ((modm .|. shiftMask, xK_Up    ), shiftToPrev)
    , ((modm,               xK_Right ), nextScreen)
    , ((modm,               xK_Left  ), prevScreen)
    , ((modm .|. shiftMask, xK_Right ), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left  ), shiftPrevScreen)
    , ((modm,               xK_z     ), toggleWS)
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((modm .|. mask, key), f sc)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]
    ++
    multiKeys [(modm2, xK_Left     ), (0, xF86XK_AudioPrev       )] "ncmpcpp prev" ++
    multiKeys [(modm2, xK_Right    ), (0, xF86XK_AudioNext       )] "ncmpcpp next" ++
    multiKeys [(modm2, xK_Up       ), (0, xF86XK_AudioStop       )] "ncmpcpp stop" ++
    multiKeys [(modm2, xK_Down     ), (0, xF86XK_AudioPlay       )] "ncmpcpp toggle" ++
    multiKeys [(modm2, xK_Page_Up  ), (0, xF86XK_AudioRaiseVolume)] "amixer set Master 2dB+ unmute" ++
    multiKeys [(modm2, xK_Page_Down), (0, xF86XK_AudioLowerVolume)] "amixer set Master 2dB- unmute" ++
    multiKeys [(modm2, xK_m        ), (0, xF86XK_AudioMute       )] "amixer sset Master toggle" ++

    []
  where
    multiKeys lst action = [(x, spawn action) | x <- lst]

myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse _ = M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    , ((modm, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    ]

myStartupHook = return ()
