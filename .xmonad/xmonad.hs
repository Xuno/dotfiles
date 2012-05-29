
import XMonad hiding (defaultConfig)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)

import Data.Monoid
import qualified Data.Map as M

import System.IO (hPutStrLn)
import System.Exit

import Graphics.X11.ExtraTypes.XF86

main = do
    xmproc <- spawnPipe "killall xmobar; xmobar ~/.xmonad/xmobar.rc"
    spawn "killall trayer; trayer --edge top --align right --margin 0 --width 5 --widthtype percent --height 20 \
            \--padding 2 --tint 0x000000 --transparent true --alpha 0"
    xmonad $ myConfig { logHook = dynamicLogWithPP $ myXMobarPP xmproc }

myConfig = XConfig
  { borderWidth        = 2
  , workspaces         = myWorkspaces
  , layoutHook         = myLayout
  , terminal           = "urxvt"
  , normalBorderColor  = "#000000"
  , focusedBorderColor = "#f01343"
  , modMask            = mask
  , keys               = myKeys
  , logHook            = return ()
  , startupHook        = myStartupHook
  , mouseBindings      = myMouse
  , manageHook         = myManageHook
  , handleEventHook    = const (return (All True))
  , focusFollowsMouse  = True
  }

mask = mod1Mask -- Left Alt
mask2 = mod4Mask -- WinKey

myWorkspaces = ["1:web", "2:term", "3:misc" ] ++ map show [4..9]

myLayout = avoidStruts $ smartBorders $ 
    (tiled ||| Mirror tiled ||| Full)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 1/2
    delta   = 3/100

myManageHook = composeAll $
    manageDocks :
    ( isDialog --> doCenterFloat) :
    ( isFullscreen --> doFullFloat) :
    [ className =? c  --> doFloat          | c <- ["MPlayer", "Gimp"]] ++
    [ className =? c  --> doShift "1:web"  | c <- ["Firefox", "Chromium"]] ++
    [ className =? c  --> doShift "2:term" | c <- []] ++
    [ className =? c  --> doShift "3:misc" | c <- ["Evince", "Thunar"]] ++
    []

myXMobarPP xmproc = xmobarPP
    { ppOutput  = hPutStrLn xmproc
    , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 50
    , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
    , ppSep     = "  "
    }
  where
    xmobarTitleColor = "#ffB6B0"
    xmobarCurrentWorkspaceColor = "#ceffac"

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((mask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((mask,               xK_p     ), spawn "dmenu_run -b -f -p \">\"")
    , ((mask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((mask .|. shiftMask, xK_c     ), kill)

    , ((mask,               xK_space ), sendMessage NextLayout)
    , ((mask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((mask,               xK_n     ), refresh)

    , ((mask,               xK_Tab   ), windows W.focusDown)
    , ((mask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
    , ((mask,               xK_j     ), windows W.focusDown)
    , ((mask,               xK_k     ), windows W.focusUp  )
    , ((mask,               xK_m     ), windows W.focusMaster  )

    , ((mask,               xK_Return), windows W.swapMaster)
    , ((mask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((mask .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((mask,               xK_h     ), sendMessage Shrink)
    , ((mask,               xK_l     ), sendMessage Expand)

    , ((mask,               xK_t     ), withFocused $ windows . W.sink)

    , ((mask .|. shiftMask, xK_equal ), sendMessage (IncMasterN 1))
    , ((mask              , xK_minus ), sendMessage (IncMasterN (-1)))

    , ((mask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((mask              , xK_q     ), spawn "xmonad --recompile && xmonad --restart")
    ]
    ++
    [((m .|. mask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    multiKeys [(mask2, xK_Left      ), (0, xF86XK_AudioPrev       )] "ncmpcpp prev" ++
    multiKeys [(mask2, xK_Right     ), (0, xF86XK_AudioNext       )] "ncmpcpp next" ++
    multiKeys [(mask2, xK_Up        ), (0, xF86XK_AudioStop       )] "ncmpcpp stop" ++
    multiKeys [(mask2, xK_Down      ), (0, xF86XK_AudioPlay       )] "ncmpcpp toggle" ++
    multiKeys [(mask2, xK_Page_Up   ), (0, xF86XK_AudioRaiseVolume)] "amixer set Master 2dB+ unmute" ++
    multiKeys [(mask2, xK_Page_Down ), (0, xF86XK_AudioLowerVolume)] "amixer set Master 2dB- unmute" ++
    multiKeys [(mask2, xK_m         ), (0, xF86XK_AudioMute       )] "amixer sset Master toggle" ++

    []
  where
    multiKeys lst action = [(x, spawn action) | x <- lst]

myMouse :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
myMouse _ = M.fromList
    [ ((mask, button1), \w -> focus w >> mouseMoveWindow w
                                      >> windows W.shiftMaster)
    , ((mask, button2), windows . (W.shiftMaster .) . W.focusWindow)
    , ((mask, button3), \w -> focus w >> mouseResizeWindow w
                                      >> windows W.shiftMaster)
    ]

myStartupHook = return ()
