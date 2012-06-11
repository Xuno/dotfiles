{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module XMonadBar where

import           XMonad
import           XMonad.Hooks.UrgencyHook     (readUrgents)
import qualified XMonad.StackSet              as W
import qualified XMonad.Util.ExtensibleState  as S
import           XMonad.Util.NamedWindows
import           XMonad.Util.WorkspaceCompare

import           Control.Monad                (forM)
import qualified Data.Colour.Names            as C
import           Data.Colour.SRGB
import           Data.List                    (sortBy)
import qualified Data.Map                     as M
import           Data.Maybe                   (isNothing)
import           Data.Ord                     (comparing)
import           System.Dzen

data XMonadBarInfo = XMBarInfo
    { workspacesB :: [(String, String,    -- workspace name, layout name,
                       Bool, Bool, Bool)] -- inUrgency, isEmpty, isFocused
    , screensB    :: [(String, String)]   -- ws in this screen, title
    }
  deriving (Read, Show, Typeable)

instance (Typeable k, Typeable a) => ExtensionClass (M.Map k a) where
    initialValue = M.empty

defaultPrinter :: Printer XMonadBarInfo
defaultPrinter = simple' (const "xmonad: uninitialized")

obtainBarInfo :: X XMonadBarInfo
obtainBarInfo = do
    ws          <- gets windowset
    urgents     <- readUrgents
    sortByIndex <- getSortByIndex
    let wsps = sortByIndex (map W.workspace (W.current ws : W.visible ws) ++ W.hidden ws)
        wspb = [ (W.tag wsp, layout, inU, isE, isF)
               | wsp <- wsps
               , let layout = description (W.layout wsp)
                     inU    = any (\w -> maybe False (==W.tag wsp) (W.findTag w ws)) urgents
                     isE    = isNothing (W.stack wsp)
                     isF    = W.tag wsp == W.currentTag ws
               ]
        onsc = sortBy (comparing (f . W.screenDetail)) $ W.current ws : W.visible ws
        getTitle Nothing                = Nothing
        getTitle (Just (W.Stack f' _ _)) = Just f'
        f (SD (Rectangle x y _ _)) = (x, y)
    scrb <- forM onsc $ \scr -> do
        let wspName = W.tag $ W.workspace scr
            wid     = getTitle $ W.stack $ W.workspace scr
        title' <- case wid of
            Nothing  -> return ""
            Just xid -> fmap show $ getName xid
        return (wspName, title')
    return (XMBarInfo wspb scrb)

xmonadBarApply :: XMonadBarInfo -> Int -> X String
xmonadBarApply barInfo uid = do
    oldPrinter <- S.gets (M.findWithDefault defaultPrinter uid)
    let (output, newPrinter) = apply oldPrinter barInfo
    S.modify (M.insert uid newPrinter)
    return output

titleFont  = "WenQuanYi Micro Hei Mono-10" :: String
fixedFont  = "CtrlD:pixelsize=13"          :: String
symbolFont = "CtrlD:pixelsize=16"          :: String

fgC  = sRGB24 0xcc 0xcc 0xcc :: DColour
bgC  = sRGB24 0x33 0x33 0x33 :: DColour
fgC2 = sRGB24 0x99 0x99 0x99 :: DColour
bgC2 = C.lightslategray :: DColour
colR = C.lightcoral
colG = C.yellowgreen
colB = C.lightskyblue

layout_tall  = "\xEE00"
layout_mtall = "\xEE01"
layout_full  = "\xEE02"
layout_grid  = "\xEE03"

useFont fn = rawStr "^fn(" +++ str fn +++ rawStr ")"

xmonadBarPrinter :: Int -> (Dimension, Dimension) -> Printer XMonadBarInfo
xmonadBarPrinter uid (_, h) = printUnderline +++ ((printWS +++ str " ") +=+ (printLayout +++ str " ") +=+ printTitle)
  where
    printUnderline = rawStr $ "^ib(1)^p(_LOCK_X)^pa(;+" ++ show (h-2) ++ ")^ro(9999x2)^p(_UNLOCK_X)^p()"
    mergeDS :: DString -> DString -> DString
    mergeDS = (+++)

    concatDS :: [DString] -> DString
    concatDS = foldr mergeDS ""

    printWS :: Printer XMonadBarInfo
    printWS = simple printer
      where
        printer (XMBarInfo wsp scr) = concatDS wsp'
          where
            multiS = length scr > 1
            scr'   = map fst scr
            cur    = scr' !! uid
            wsp'   = [ if name == cur then ignoreBg False $ bg fgC $ fg bgC $ concatDS $ map ppWS scr' else ppWS name
                     | (name, _, _, e, _) <- wsp
                     , name == cur || not e && name `notElem` scr'
                     ] :: [DString]
            isU name = or [u | (name', _, u, _, _) <- wsp, name == name']
            isF name = or [f | (name', _, _, _, f) <- wsp, name == name']
            isC name = name == cur
            ppWS :: String -> DString
            ppWS name | isU name           = fg C.red ds
                      | isC name && multiS = bg bgC2 $ fg C.snow ds
                      | otherwise          = ds
              where
                ds | isF name  = fg C.red (str "[") +++ str name +++ fg C.red (str "]")
                   | otherwise = str (" " ++ name ++ " ")
    printLayout :: Printer XMonadBarInfo
    printLayout = useFont symbolFont +++ ignoreBg False (bg fgC $ fg bgC $ simple' printer)
      where
        printer (XMBarInfo wsp scr) =
            concat [" " ++ f la ++ " " | (name, la, _, _, _) <- wsp, name == cur]
          where
            cur = fst (scr !! uid)

        f "Tall"        = layout_tall
        f "Mirror Tall" = layout_mtall
        f "Full"        = layout_full
        f "Grid"        = layout_grid
        f other         = other

    printTitle :: Printer XMonadBarInfo
    printTitle = useFont titleFont +++ simple' (\conf -> snd $ screensB conf !! uid)
