
import           Codec.Binary.UTF8.String
import           Control.Concurrent                (threadDelay)
import           Control.Monad                     (forM, forM_, when)
import qualified Control.Exception                 as E
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
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers        hiding (pid)
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
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
            (lrect, rrect) = Scr.splitBar rect' (3 % 7)
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
    pid <- forkProcess (applyForever (putAll 45) (threadDelay delay >> Monitor.getAll ps) hputs)
    threadDelay delay
    xmonad $ withUrgencyHook NoUrgencyHook $ ewmh $ myConfig (map fst screens, dzens, pid)
    killP pid

barHeight = 16
delay     = 250 * 1000

killP :: MonadIO m => ProcessID -> m ()
killP pid = liftIO (signalProcess killProcess pid `E.catch` (\e -> (e :: E.SomeException) `seq` return ()))

myConfig (phyScreens, dzens, pid) = XConfig
  { borderWidth        = 3
  , workspaces         = myWorkspaces
  , layoutHook         = myLayout
  , terminal           = "urxvt"
  , normalBorderColor  = "#000000"
  , focusedBorderColor = "#ff0000"
  , modMask            = modm
  , keys               = myKeys phyScreens pid
  , logHook            = myLogHook dzens
  , startupHook        = myStartupHook dzens
  , mouseBindings      = myMouse
  , manageHook         = myManageHook
  , handleEventHook    = fullscreenEventHook
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  }

modm  = mod1Mask -- Left Alt
modm2 = mod4Mask -- WinKey

myWorkspaces = [web, term, misc] ++ map show [4..9]
web          = "1:web"
term         = "2:term"
misc         = "3:misc"

myLayout = avoidStruts $ smartBorders
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
    inWeb = fmap (==web) currentWs
    inT   = fmap (==term) currentWs

    myFloats = isTC <||> isPopup <||> className `hasPrefix` ["Gimp", "fontforge", "Nitrogen", "qemu-system-x86_64"]
    myWeb    = className `hasPrefix` ["Firefox", "Chromium", "Google-chrome"]
    myTerm   = className `hasPrefix` ["URxvt", "Gvim"]
    myMisc   = isJava <||> className `hasPrefix` ["Evince", "Thunar", "Vlc", "Transmission-gtk", "Xpdf", "Wine", "mpv"]
    isJava   = appName `hasPrefix` ["sun-"]
    isTC     = className `hasPrefix` ["com-topcoder"]
    isPopup  = stringProperty "WM_WINDOW_ROLE" `hasPrefix` ["pop-up"]
    noShift  = className `hasPrefix` ["Gmrun", "Xmessage", "stalonetray"]
    notTerm  = fmap not myTerm

    hasPrefix name []       = return False
    hasPrefix name prefixes = foldl1 (<||>) [fmap (prefix`isPrefixOf`) name | prefix <- prefixes]

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
    , ((modm .|. shiftMask, xK_o     ), spawn "dm-tool switch-to-greeter")
    , ((modm .|. shiftMask, xK_t     ), spawn "stalonetray --window-type normal")

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
    multiKeys [(modm2, xK_Left        ), (0, xF86XK_AudioPrev       )] "mpc -q prev" ++
    multiKeys [(modm2, xK_Right       ), (0, xF86XK_AudioNext       )] "mpc -q next" ++
    multiKeys [(modm2, xK_Up          ), (0, xF86XK_AudioStop       )] "mpc -q stop" ++
    multiKeys [(modm2, xK_Down        ), (0, xF86XK_AudioPlay       )] "mpc -q toggle" ++
    multiKeys [(modm,  xK_bracketright), (0, xF86XK_AudioRaiseVolume)] ("card=" ++ fstCard ++ "; amixer -c $card sset " ++ controlName "$card" ++ " 5%+ unmute") ++
    multiKeys [(modm,  xK_bracketleft ), (0, xF86XK_AudioLowerVolume)] ("card=" ++ fstCard ++ "; amixer -c $card sset " ++ controlName "$card" ++ " 5%- unmute") ++
    multiKeys [(modm,  xK_backslash   ), (0, xF86XK_AudioMute       )] ("card=" ++ fstCard ++ "; amixer -c $card sset " ++ controlName "$card" ++ " toggle") ++
    multiKey_ [(modm,  xK_bracketright), (0, xF86XK_AudioRaiseVolume)] ("card=" ++ sndCard ++ "; amixer -c $card sset " ++ controlName "$card" ++ " 5%+ unmute") ++
    multiKey_ [(modm,  xK_bracketleft ), (0, xF86XK_AudioLowerVolume)] ("card=" ++ sndCard ++ "; amixer -c $card sset " ++ controlName "$card" ++ " 5%- unmute") ++
    multiKey_ [(modm,  xK_backslash   ), (0, xF86XK_AudioMute       )] ("card=" ++ sndCard ++ "; amixer -c $card sset " ++ controlName "$card" ++ " toggle") ++
    multiKeys [(modm2, xK_F10         ), (0, xK_Print               )] "sleep 0.2; scrot '%Y-%m-%d-%H%M%S_$wx$h.png' -e 'mv $f ~'" ++

    [ ((modm2 .|. shiftMask, xK_F10),   spawn "sleep 0.2; scrot '%Y-%m-%d-%H%M%S_$wx$h.png' -s -e 'mv $f ~'")
    , ((modm2 .|. shiftMask, xK_Left),  spawn "mpc -q seek -5%")
    , ((modm2 .|. shiftMask, xK_Right), spawn "mpc -q seek +5%")
    ]
  where
    -- dirty shell hack to obtain USB DAC control name, removing 'Simple mixer control' prefix.
    controlName card = "\"$(amixer -c " ++ card ++ " scontrols | head -n 1 | sed 's/^[^\\x27]*\\x27/\\x27/g')\""

    -- dirty shell hack to get main card, prefer second card.
    pred = "amixer -c DAC scontrols &>/dev/null && amixer -c PCH scontrols &>/dev/null"
    fstCard = "$(" ++ pred ++ " && echo DAC || echo 0)"
    sndCard = "$(" ++ pred ++ " && echo PCH || echo 1)"

    multiKeys lst action = [(x, spawn action) | x <- lst]
    multiKey_ lst action = [((x .|. shiftMask, y), spawn action) | (x, y) <- lst]
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
