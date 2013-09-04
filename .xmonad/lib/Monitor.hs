
module Monitor where

import           Data.Char.WCWidth
import           Data.Time
import qualified Network.MPD          as MPD
import           Sound.ALSA.Mixer
import           System.Locale

import           Control.DeepSeq
import           Control.Monad
import qualified Data.ByteString.UTF8 as BS
import           Data.Char            (isDigit)
import           Data.IORef
import           Data.Maybe
import           Data.Ratio
import           System.IO            (hPrint, stderr)
import           System.Posix.Files   (fileExist)

import           Control.Applicative  ((<$>))
import qualified Control.Exception    as E

import           System.Dzen

import           XMonadBar

symbolFG :: DString -> DString
symbolFG = fg colB

show' :: Show a => a -> DString
show' = str . show

getDate :: IO String
getDate = formatTime defaultTimeLocale "%F/%a %H:%M:%S" <$> getZonedTime

putDate :: String -> DString
putDate x = foldr (+++) (str "") [if ch `elem` "/-:" then fg fgC2 s else s | ch <- x, let s = str [ch]]

getVolume :: IO (Integer, Integer, Integer, Bool)
getVolume = do
    Just control <- getControlByName "default" "Master"
    let Just playbackVolume = playback $ volume control
        Just playbackSwitch = playback $ switch control
    (minv, maxv) <- getRange playbackVolume
    Just vol   <- getChannel FrontLeft $ value playbackVolume
    Just sw    <- getChannel FrontLeft playbackSwitch
    return (minv, maxv, vol, sw)

putVolume :: (Integer, Integer, Integer, Bool) -> DString
putVolume (minv, maxv, vol, enabled) = symbolFG (str lhs) +++ rhs
  where
    ratio = min 1 $ max 0 $ (vol - minv) % maxv

    lhs | not enabled   = vol_muted
        | ratio < 1 % 3 = vol_1
        | ratio < 2 % 3 = vol_2
        | otherwise     = vol_3

    rhs = show' pct +++ fg fgC2 (str "%")

    pct = round (ratio * 100) :: Int

    vol_muted = "\xEE20"
    vol_1     = "\xEE21"
    vol_2     = "\xEE22"
    vol_3     = "\xEE23"

getMem :: IO Double
getMem = do
    [total, free, buffer, cache] <- map (read . (!!1) . words) . take 4 . lines <$> readFile "/proc/meminfo"
    return $ (total - free - buffer - cache) / total

putMem :: Double -> DString
putMem ratio = symbolFG (str mem) +++ fgpct (show' pct) +++ fg fgC2 (str "%")
  where
    pct = round (min 1 ( max 0 ratio) * 100) :: Int

    fgpct | pct >= 80 = fg colR
          | pct <= 20 = fg colG
          | otherwise = id

    mem = "\xEEF2"

getUptime :: IO Integer
getUptime = max 1 . read . takeWhile isDigit . head . words <$> readFile "/proc/uptime"

putUptime :: Integer -> DString
putUptime s = symbolFG (str uptime) +++ lhs +++ rhs
  where
    uptime = "\xEEF4"

    parseSs secs = (show' (secs `mod` 60) +++ fg fgC2 (str "s")) : parseMs (secs `div` 60)
    parseMs mins = (show' (mins `mod` 60) +++ fg fgC2 (str "m")) : parseHs (mins `div` 60)
    parseHs 0    = []
    parseHs hour = (show' (hour `mod` 24) +++ fg fgC2 (str "h")) : parseDs (hour `div` 24)
    parseDs 0    = []
    parseDs days = [show' days +++ fg fgC2(str "d")]

    [lhs, rhs] = take 2 $ reverse (parseSs s)


data ProcStat = ProcStat
    { user   :: Integer
    , nice   :: Integer
    , system :: Integer
    , idle   :: Integer
    }

initPS :: IO (IORef (ProcStat, ProcStat, ProcStat))
initPS = let zero = ProcStat 0 0 0 0 in newIORef (zero, zero, zero)

getCPULoad :: IORef (ProcStat, ProcStat, ProcStat) -> IO Double
getCPULoad ref = do
    (ProcStat a b c d, e, f) <- readIORef ref
    [a', b', c', d'] <- map read . take 4 . tail . words . head . lines
                          <$> readFile "/proc/stat"
    writeIORef ref (e, f, ProcStat a' b' c' d')
    let allT  = (a' + b' + c' + d') - (a + b + c + d)
        idleT = d' - d
        ratio = fromIntegral (allT - idleT) / fromIntegral allT
    return $ if allT <= 0 || idleT < 0 || idleT > allT then 0 else ratio

putCPULoad :: Double -> DString
putCPULoad ratio = symbolFG (str cpu) +++ fgpct (show' pct) +++ fg fgC2 (str "%")
  where
    pct = round (min 1 (max 0 ratio) * 100) :: Int

    fgpct | pct >= 80 = fg colR
          | pct <= 20 = fg colG
          | otherwise = id

    cpu = "\xEEF1"

getTemp :: IO Double
getTemp = do
    temp0 <- parse zone0
    temp1 <- parse zone1
    return $ case (temp0, temp1) of
        (Nothing, Nothing) -> error "monitor: getTemp"
        (Just a,  Just b ) -> (a + b) / 2
        (Just a,  _      ) -> a
        (_,       Just b ) -> b
  where
    parse file = do
        exists <- fileExist file
        if exists then Just . (/1000) . read <$> readFile file else return Nothing
    zone0 = "/sys/class/thermal/thermal_zone0/temp"
    zone1 = "/sys/class/thermal/thermal_zone1/temp"

putTemp :: Double -> DString
putTemp temp' = symbolFG (str temperature) +++ fgtemp (show' temp) +++ fg fgC2 (str "\x00b0")
  where
    temp = round (min 200 (max 0 temp')) :: Int

    fgtemp | temp >= 80 = fg colR
           | temp <= 50 = fg colG
           | otherwise  = id

    temperature = "\xEEF5"

type MPDInfo = (MPD.State, (Maybe String, Maybe String, Maybe String), (Double, Integer))

getMPD :: IO MPDInfo
getMPD = do
    ret <- MPD.withMPDEx "localhost" 6600 "" $ do
        st <- MPD.status
        song <- MPD.currentSong
        let get tag s = fmap valueToString $ join $ fmap listToMaybe (MPD.sgGetTag tag s) :: Maybe String
            songTags  = case song of
                Nothing -> (Nothing, Nothing, Nothing)
                Just s  -> (get MPD.Artist s, get MPD.Album s, get MPD.Title s)
            valueToString (MPD.Value bs) = BS.toString bs
        return (MPD.stState st, songTags, MPD.stTime st)
    case ret of
        Left msg -> error $ "monitor: getMPD - " ++ show msg
        Right x  -> return x

putMPD :: Int -> Printer (Maybe MPDInfo)
putMPD limitLen = inputPrinter printer 0
  where
    mpd           = "\xEEF3"
    music_stopped = "\xEE30"
    music_play    = "\xEE31"
    music_paused  = "\xEE32"

    printer :: Integer -> Maybe MPDInfo -> (DString, Integer)
    printer offset Nothing                                             = (str "", offset + 1)
    printer offset (Just (MPD.Stopped, _, _))                          = (symbolFG (str music_stopped) +++ str "[off]", offset)
    printer offset (Just (st, (artist, album, title), (usedT', allT))) =
        (curSong +++ symbolFG state +++ timeInfo, offset + if st == MPD.Playing then 1 else 0)
      where
        state | st == MPD.Playing = str music_play
              | otherwise         = str music_paused
        usedT = round usedT' :: Integer
        formatT sec = show' (sec `div` 60) +++ fg fgC2 (str ":") +++
            show' (sec `mod` 60 `div` 10) +++ show' (sec `mod` 60 `mod` 10)
        timeInfo = formatT usedT +++ fg fgC2 (str "/") +++ formatT allT

        curSong :: DString
        curSong
          | curSong' == "" = str ""
          | otherwise      = foldl concatDS (symbolFG (str mpd) +++ useFont titleFont)
            [ if ch == '@' then fg fgC2 (str "-") else str [ch] | ch <- curSong']
            +++ useFont symbolFont

        concatDS :: DString -> DString -> DString
        concatDS = (+++)

        curSong' :: String
        curSong' = case (artist, album, title) of
            (Nothing, Nothing, Nothing) -> ""
            _                           -> rotate $ formatTitle artist ++ " @ " ++
                                                    formatTitle title  ++ " @ " ++
                                                    formatTitle album  ++ " @ " ++
                                                    formatTitle title  ++ " @ "

        formatTitle Nothing  = "Unknown"
        formatTitle (Just a) = a

        rotate s = let pos' = fromIntegral (offset `mod` fromIntegral (length s))
                       (lhs, rhs) = splitAt pos' s
                   in takeByWC limitLen (rhs ++ lhs)

takeByWC :: Int -> String -> String
takeByWC _ []                   = []
takeByWC len (x:xs) | len < w   = replicate len ' '
                    | otherwise = x : takeByWC (len - w) xs
  where
    w = wcwidth x

instance NFData MPD.State where

safeWrapper :: NFData a => IO a -> IO (Maybe a)
safeWrapper io = (io >>= \r -> r `deepseq` return (Just r)) `E.catch`
    (\e -> (e :: E.SomeException) `seq` return Nothing)

printWrapper :: (a -> DString) -> Maybe a -> DString
printWrapper _ Nothing  = str ""
printWrapper f (Just a) = f a

getAll ps =
    safeWrapper getMPD ##
    safeWrapper (getCPULoad ps) ##
    safeWrapper getMem ##
    safeWrapper getTemp ##
    safeWrapper getUptime ##
    safeWrapper getVolume ##
    safeWrapper getDate

putAll len =
    (putMPD len +++ str " ") +++
    autoPadL 1 (simple (printWrapper putCPULoad) +++ str " ") +++
    autoPadL 1 (simple (printWrapper putMem) +++ str " ") +++
    autoPadL 1 (simple (printWrapper putTemp)) +++
    autoPadL 1 (simple (printWrapper putUptime) +++ str " ") +++
    autoPadL 1 (simple (printWrapper putVolume) +++ str " ") +++
    autoPadL 1 (simple (printWrapper putDate))
