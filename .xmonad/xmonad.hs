
import           Codec.Binary.UTF8.String
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (forM, forM_, when)
import           Data.Colour.SRGB
import           Data.List                         (isPrefixOf)
import qualified Data.Map                          as M
import           Data.Monoid
import           Data.Ratio
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit
import           System.IO
import           System.Posix.Process
import           System.Posix.Signals
import           System.Posix.Types
import           XMonad                            hiding (defaultConfig)
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DwmPromote
import           XMonad.Actions.FindEmptyWorkspace
import           XMonad.Actions.UpdatePointer
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ICCCMFocus
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Grid
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet                   as W
import qualified XMonad.Util.ExtensibleState       as S
import           XMonad.Util.Run                   (spawnPipe)

import           Monitor
import qualified Screen                            as Scr
import           System.Dzen                       hiding (rect, str)
import           XMonadBar

main = do
    screens <- Scr.getScreens
    ret <- forM screens $ \(sid, rect) -> do
        let cmdsL          = Scr.rectToDzenCmdSpec lrect
            cmdsR          = Scr.rectToDzenCmdSpec rrect
            rect'          = Scr.getBarPos rect Scr.TopEdge Scr.LeftA (Scr.WidthRatio 1) barHeight
            (lrect, rrect) = Scr.splitBar rect' (45 % 100)
            cmdlineL       = unwords $ "dzen2" : cmdsL ++
                [ "-fn", "'" ++ fixedFont ++ "'"
                , "-ta", "l"
                , "-fg", "'" ++ sRGB24show fgC ++ "'"
                , "-bg", "'" ++ sRGB24show bgC ++ "'"
                , "-e", "'onstart=lower'"
                ]
            cmdlineR       = unwords $ "dzen2" : cmdsR ++
                [ "-fn", "'" ++ symbolFont ++ "'"
                , "-ta", "r"
                , "-fg", "'" ++ sRGB24show fgC ++ "'"
                , "-bg", "'" ++ sRGB24show bgC ++ "'"
                , "-e", "'onstart=lower'"
                ]
        handleL <- spawnPipe cmdlineL
        handleR <- spawnPipe cmdlineR
        return ((sid, handleL, rect'), handleR)
    ps <- initPS
    let dzens   = map fst ret
        hrs     = map snd ret
        hputs s = forM_ hrs $ \hr -> hPutStrLn hr (utf8Encode s) >> hFlush hr
    pid <- forkProcess (applyForever (putAll 25) (threadDelay delay >> Monitor.getAll ps) hputs)
    threadDelay delay
    xmonad (myConfig (map fst screens, dzens, pid))
    killP pid

barHeight = 16
delay     = 500 * 1000

killP :: MonadIO m => ProcessID -> m ()
killP = liftIO . signalProcess killProcess

myConfig (phyScreens, dzens, pid) = XConfig
  { borderWidth        = 3
  , workspaces         = myWorkspaces
  , layoutHook         = myLayout
  , terminal           = "urxvtc"
  , normalBorderColor  = "#000000"
  , focusedBorderColor = "#ff0000"
  , modMask            = modm
  , keys               = myKeys phyScreens pid
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

myManageHook = composeAll
    [ manageDocks
    , isDialog          --> doCenterFloat
    , isFullscreen      --> doFullFloat
    , myFloats          --> doFloat
    , composeOne
    [ noShift           -?> idHook
    , myWeb             -?> doShift web
    , myTerm <&&> inWeb -?> doShift term
    , myMisc            -?> doShift misc
    , notTerm <&&> inT  -?> doShift misc
    ]
    ]
  where
    myFloats = foldl (<||>) isTC [className =? c | c <- ["MPlayer", "Gimp", "fontforge", "Nitrogen"]]
    myWeb    = className =? "Firefox" <||> className =? "Chromium"
    myTerm   = className =? "URxvt" <||> className =? "Gvim"
    myMisc   = foldl (<||>) isJava [className =? c | c <- ["Evince", "Thunar", "Vlc", "Transmission-gtk", "Xpdf"]]
    inWeb    = fmap (==web) currentWs
    inT      = fmap (==term) currentWs
    isJava   = fmap ("sun-"`isPrefixOf`) appName
    isTC     = fmap ("com-topcoder"`isPrefixOf`) className
    noShift  = className =? "Gmrun" <||> className =? "Xmessage" <||> className =? "stalonetray"
    notTerm  = fmap not myTerm

myKeys :: [ScreenId] -> ProcessID -> XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys phyScreens pid conf = M.fromList $
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modm,               xK_p     ), spawn "exe=`yeganesh -x -- -b -p '>'` && eval \"exec $exe\"")
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

    , ((modm,               xK_f     ), withFocused $ windows . W.sink)

    , ((modm .|. shiftMask, xK_equal ), sendMessage (IncMasterN 1))
    , ((modm              , xK_minus ), sendMessage (IncMasterN (-1)))

    , ((modm .|. shiftMask, xK_q     ), killP pid >> io exitSuccess)
    , ((modm .|. shiftMask, xK_r     ), killP pid >> spawn "xmonad --recompile && xmonad --restart")
    , ((modm .|. shiftMask, xK_n     ), killP pid >> spawn "nitrogen --restore; xmonad --restart")
    , ((modm .|. shiftMask, xK_l     ), spawn "xscreensaver-command -lock")
    , ((modm .|. shiftMask, xK_t     ), spawn "stalonetray")

    , ((modm,               xK_s     ), viewEmptyWorkspace)
    , ((modm .|. shiftMask, xK_s     ), tagToEmptyWorkspace)

    , ((modm,               xK_Down  ), nextWS)
    , ((modm,               xK_Up    ), prevWS)
    , ((modm .|. shiftMask, xK_Down  ), shiftToNext)
    , ((modm .|. shiftMask, xK_Up    ), shiftToPrev)
    , ((modm,               xK_Right ), nextScreen)
    , ((modm,               xK_Left  ), prevScreen)
    , ((modm .|. shiftMask, xK_Right ), shiftNextScreen)
    , ((modm .|. shiftMask, xK_Left  ), shiftPrevScreen)
    , ((modm,               xK_grave ), toggleWS)
    , ((modm,               xK_Escape), toggleWS)

    , ((modm .|. shiftMask, xK_f     ), fullScreenCurrent)
    , ((modm .|. shiftMask, xK_x     ), spawn "gksudo -m 'Enter Password to poweroff' poweroff")
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [ ((modm, k), screenWorkspace sc >>= flip whenJust (windows . W.view))
    | (k, sc) <- zip [xK_q, xK_w, xK_e, xK_r, xK_t] (phyScreens ++ repeat (last phyScreens))
    ]
    ++
    multiKeys [(modm2, xK_Left     ), (0, xF86XK_AudioPrev       )] "ncmpcpp prev" ++
    multiKeys [(modm2, xK_Right    ), (0, xF86XK_AudioNext       )] "ncmpcpp next" ++
    multiKeys [(modm2, xK_Up       ), (0, xF86XK_AudioStop       )] "ncmpcpp stop" ++
    multiKeys [(modm2, xK_Down     ), (0, xF86XK_AudioPlay       )] "ncmpcpp toggle" ++
    multiKeys [(modm2, xK_Page_Up  ), (0, xF86XK_AudioRaiseVolume),
                                           (modm, xK_bracketright)] "amixer set Master 2dB+ unmute" ++
    multiKeys [(modm2, xK_Page_Down), (0, xF86XK_AudioLowerVolume),
                                           (modm, xK_bracketleft )] "amixer set Master 2dB- unmute" ++
    multiKeys [(modm2, xK_space    ), (0, xF86XK_AudioMute       ),
                                           (modm, xK_backslash   )] "amixer sset Master toggle" ++
    multiKeys [(modm2, xK_F10      ), (0, xK_Print               )] "sleep 0.2; scrot '%Y-%m-%d-%H%M%S_$wx$h.png' -e 'mv $f ~'" ++
    multiKeys [(modm2 .|. shiftMask, xK_F10)]                       "sleep 0.2; scrot '%Y-%m-%d-%H%M%S_$wx$h.png' -s -e 'mv $f ~'" ++

    []
  where
    multiKeys lst action = [(x, spawn action) | x <- lst]
    fullScreenCurrent = do
        mw <- gets (W.peek . windowset)
        case mw of
            Nothing -> return ()
            Just w  -> runQuery doFullFloat w >>= windows . appEndo

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
    fadeInactiveCurrentWSLogHook 0.9
    takeTopFocus
    barInfo <- obtainBarInfo
    forM_ (zip [0..] dzens) $ \(phyID, (_, handle, _)) -> do
        str <- xmonadBarApply barInfo phyID
        io (hPutStrLn handle (utf8Encode str) >> hFlush handle)

myStartupHook dzens = do
    setWMName "LG3D"
    forM_ (zip [0..] dzens) $ \(phyID, (_, _, Rectangle _ _ w h)) ->
        S.modify (M.insert phyID (xmonadBarPrinter phyID (w, h)))
    myLogHook dzens
    withDisplay $ \dpy -> do
        rootw <- asks theRoot
        (_,_,wins) <- io $ queryTree dpy rootw
        forM_ wins $ \win -> do
            classname <- fmap resClass $ io (getClassHint dpy win)
            when (classname == "dzen") (fadeOut 0.8 win)
        return ()
