{-# LANGUAGE DeriveDataTypeable #-}

import XMonad hiding (defaultConfig)
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.DwmPromote
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Util.ExtensibleState as S
import Graphics.X11.ExtraTypes.XF86

import Control.Monad (forM, forM_, when)
import Data.Monoid
import qualified Data.Map as M
import Data.Colour.SRGB
import Data.List (isPrefixOf)

import System.IO
import System.Exit

import qualified Screen as Scr
import XMonadBar
import Codec.Binary.UTF8.String

main = do
    screens <- Scr.getScreens
    dzens <- forM screens $ \(sid, rect) -> do
        let cmds  = Scr.rectToDzenCmdSpec rect'
            rect' = Scr.getBarPos rect Scr.TopEdge Scr.LeftA (Scr.WidthRatio 1) barHeight
            cmdline = unwords $ "dzen2" : cmds ++ 
                    [ "-fn", "'" ++ fixedFont ++ "'"
                    , "-ta", "l"
                    , "-fg", "'" ++ sRGB24show fgC ++ "'"
                    , "-bg", "'" ++ sRGB24show bgC ++ "'"
                    ]
        handle <- spawnPipe cmdline
        return (sid, handle, rect')
    xmonad (myConfig dzens)

trayWidth = 50
barHeight = 16

myConfig dzens = XConfig
  { borderWidth        = 2
  , workspaces         = myWorkspaces
  , layoutHook         = myLayout
  , terminal           = "urxvtc"
  , normalBorderColor  = "#cccccc"
  , focusedBorderColor = "#ff0000"
  , modMask            = modm
  , keys               = myKeys
  , logHook            = myLogHook dzens
  , startupHook        = myStartupHook dzens
  , mouseBindings      = myMouse
  , manageHook         = myManageHook
  , handleEventHook    = const (return (All True))
  , focusFollowsMouse  = True
  }

modm  = mod1Mask -- Left Alt
modm2 = mod4Mask -- WinKey

myWorkspaces = [web, term, misc] ++ map show [4..9]
web          = "1:web"
term         = "2:term"
misc         = "3:misc"

myLayout = avoidStruts $ smartBorders $ 
    (tiled ||| Mirror tiled ||| Full ||| Grid)
  where
    tiled   = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 6/10
    delta   = 3/100

myManageHook = composeAll $
    [ manageDocks
    , isDialog          --> doCenterFloat
    , isFullscreen      --> doFullFloat
    , myFloats          --> doFloat
    , composeOne
    [ isGmrun           -?> idHook -- do-not shift gmrun
    , myWeb             -?> doShift web
    , myTerm <&&> inWeb -?> doShift term
    , myMisc            -?> doShift misc
    , notTerm <&&> inT  -?> doShift misc
    ]
    ]
  where
    myFloats = className =? "MPlayer" <||> className =? "Gimp" <||> isTC
    myWeb    = className =? "Firefox" <||> className =? "Chromium"
    myTerm   = className =? "URxvt" <||> className =? "Gvim"
    myMisc   = foldl (<||>) isJava [className =? c | c <- ["Evince", "Thunar", "Vlc", "Transmission-gtk"]]
    inWeb    = fmap (==web) currentWs
    inT      = fmap (==term) currentWs
    isJava   = fmap ("sun-"`isPrefixOf`) appName
    isTC     = fmap ("com-topcoder"`isPrefixOf`) className
    isGmrun  = className =? "Gmrun"
    notTerm  = fmap not myTerm

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "exe=`yeganesh -x -- -b -p '>'` && eval \"exec $exe\"")
    , ((modm,               xK_F2    ), spawn "gmrun")
    -- , ((modm .|. shiftMask, xK_p     ), dmenu + NamedScratchpad
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
    , ((modm              , xK_q     ), spawn "xmonad --recompile && xmonad --restart")
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

myLogHook dzens = do
    updatePointer (TowardsCentre 0.3 0.3) 
    fadeInactiveCurrentWSLogHook 0.7
    barInfo <- obtainBarInfo
    forM_ (zip [0..] dzens) $ \(phyID, (_, handle, rect)) -> do 
        str <- xmonadBarApply barInfo phyID
        io $ hPutStrLn handle (utf8Encode str)

myStartupHook dzens = do
    setWMName "LG3D"
    forM_ (zip [0..] dzens) $ \(phyID, (_, _, Rectangle _ _ w h)) -> do
        S.modify (M.insert phyID (xmonadBarPrinter phyID (w, h)))
    myLogHook dzens
