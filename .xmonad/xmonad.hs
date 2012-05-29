
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders
import XMonad.Util.Run (spawnPipe)

import System.IO (hPutStrLn)

myTerm    = "urxvt"
myModMask = mod1Mask -- Left Alt

grey       = "#cccccc"
red        = "#ff0000"
green      = "#14ff4f"
pink       = "#ffB6B0"
lightgreen = "#ceffac"
white      = "#ffffff"
black      = "#000000"

myManageHook = manageDocks

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.rc"
    spawn "killall trayer; trayer --edge top --align right --margin 0 --width 5 --widthtype percent --height 20 \
            \--padding 2 --tint 0x000000 --transparent true --alpha 0"
    xmonad $ myConfig
      { manageHook = myManageHook <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ smartBorders $ layoutHook defaultConfig
      , logHook    = dynamicLogWithPP $ myXMobarPP xmproc
      }

xmobarTitleColor = pink
xmobarCurrentWorkspaceColor = lightgreen

myXMobarPP xmproc = xmobarPP

   { ppOutput  = hPutStrLn xmproc
   , ppTitle   = xmobarColor xmobarTitleColor "" . shorten 50
   , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
   , ppSep     = "  "
   }

myConfig = defaultConfig

  { borderWidth        = 1
  , terminal           = myTerm
  , normalBorderColor  = black
  , focusedBorderColor = green
  }


