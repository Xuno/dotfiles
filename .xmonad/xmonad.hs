
import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run (spawnPipe)

import System.IO (hPutStrLn)

myTerm    = "urxvt"
myModMask = mod1Mask -- Left Alt

grey       = "#cccccc"
green      = "#14ff4f"
pink       = "#FFB6B0"
lightgreen = "#CEFFAC"

myManageHook = manageDocks

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.rc"
    xmonad $ myConfig
      { manageHook = myManageHook <+> manageHook defaultConfig
      , layoutHook = avoidStruts $ layoutHook defaultConfig
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

  { borderWidth        = 3
  , terminal           = myTerm
  , normalBorderColor  = grey
  , focusedBorderColor = green
  }


