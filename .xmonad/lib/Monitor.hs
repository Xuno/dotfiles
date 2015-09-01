
module Monitor where

import           Data.Char.WCWidth
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Time
import qualified Network.MPD         as MPD
import           Sound.ALSA.Mixer

import           Control.DeepSeq
import           Control.Monad
import           Data.Char           (isDigit)
import           Data.IORef
import           Data.Maybe
import           Data.Ratio
import           System.IO           (hPrint, stderr)
import           System.IO.Unsafe    (unsafePerformIO)
import           System.Posix.Files  (fileExist)

import           Control.Applicative ((<$>))
import qualified Control.Exception   as E

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

commonControlNames :: [String]
commonControlNames = ["Master", "PCM", "Speaker", "Headphone"]

getVolume :: String -> IO (Integer, Integer, Integer, Bool)
getVolume card = withMixer card $ \mixer -> do
    controlWithNames <- map (\x -> (name x, x)) . sortBy (comparing index) <$> controls mixer
    let control = head $ catMaybes (map (`lookup`controlWithNames) commonControlNames) ++ map snd controlWithNames
        Just playbackVolume = playback $ volume control
        Just playbackSwitch = playback $ switch control
    (minv, maxv) <- getRange playbackVolume
    Just vol   <- getChannel FrontLeft $ value playbackVolume
    Just sw    <- getChannel FrontLeft playbackSwitch
    return (minv, maxv, vol, sw)

getVolumes :: IO [(Integer, Integer, Integer, Bool)]
getVolumes = catMaybes <$> mapM getVolumeWrapped cards
  where
    cards = ["hw:" ++ [i] | i <- "0123"]

    getVolumeWrapped card = (Just <$> getVolume card) `E.catch` (\e -> (e :: E.SomeException) `seq` return Nothing)

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

putVolumes :: [(Integer, Integer, Integer, Bool)] -> DString
putVolumes = foldl (+++) (str "") . map putVolume

getMem :: IO Double
getMem = do
    [total, free, avail] <- map (read . (!!1) . words) . take 3 . lines <$> readFile "/proc/meminfo"
    return $ (total - avail) / total

putMem :: Double -> DString
putMem ratio = symbolFG (str mem) +++ fgpct (show' pct) +++ fg fgC2 (str "%")
  where
    pct = round (min 1 ( max 0 ratio) * 100) :: Int

    fgpct | pct >= 80 = fg colR
          | pct <= 30 = fg colG
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
          | pct <= 30 = fg colG
          | otherwise = id

    cpu = "\xEEF1"

getTemp :: IO Double
getTemp = do
    temp <- mapM (\file -> (/1e3) . read <$> readFile file) actualFiles
    return $ if null temp then error "monitor: getTemp" else maximum temp
  where
    {-# NOINLINE actualFiles #-}
    actualFiles = unsafePerformIO $ filterM fileExist files

    files = concat [ [ "/sys/bus/platform/devices/coretemp." ++ show i ++ "/hwmon/hwmon1/temp" ++ show j ++ "_input"
                     , "/sys/bus/platform/devices/coretemp." ++ show i ++ "/hwmon/hwmon2/temp" ++ show j ++ "_input"
                     , "/sys/bus/platform/devices/coretemp." ++ show i ++ "/temp" ++ show j ++ "_input"
                     ] | i <- [0,1], j <- [0..16] ]

putTemp :: Double -> DString
putTemp temp' = symbolFG (str temperature) +++ fgtemp (show' temp) +++ fg fgC2 (str "\x00b0")
  where
    temp = round (min 200 (max 0 temp')) :: Int

    fgtemp | temp >= 85 = fg colR
           | temp <= 66 = fg colG
           | otherwise  = id

    temperature = "\xEEF5"

type MPDInfo = (MPD.State, (Maybe String, Maybe String, Maybe String), Maybe (Double, MPD.Seconds))

getMPD :: IO MPDInfo
getMPD = do
    ret <- MPD.withMPDEx "localhost" 6600 "" $ do
        st <- MPD.status
        song <- MPD.currentSong
        let get tag s = fmap MPD.toString $ join $ fmap listToMaybe (MPD.sgGetTag tag s) :: Maybe String
            songTags  = case song of
                Nothing -> (Nothing, Nothing, Nothing)
                Just s  -> (get MPD.Artist s, get MPD.Album s, get MPD.Title s)
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
    printer offset Nothing                    = (str "", offset + 1)
    printer offset (Just (MPD.Stopped, _, _)) = (symbolFG (str music_stopped) +++ str "[off]", offset)
    printer offset (Just (st, (artist, album, title), Just (usedT', allT))) =
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

    printer offset _                          = (str "<error>", offset + 1)

takeByWC :: Int -> String -> String
takeByWC _ []                   = []
takeByWC len (x:xs) | len < w   = replicate len ' '
                    | otherwise = x : takeByWC (len - w) xs
  where
    w = wcwidth x

instance NFData MPD.State where rnf x = seq x ()

safeWrapper :: NFData a => IO a -> IO (Maybe a)
safeWrapper io = (io >>= \r -> r `deepseq` return (Just r)) `E.catch`
    (\e -> (e :: E.SomeException) `seq` return Nothing)

printWrapper :: (a -> DString) -> Maybe a -> DString
printWrapper _ Nothing  = str ""
printWrapper f (Just a) = str " " +++ f a

getAll ps =
    safeWrapper getMPD ##
    safeWrapper (getCPULoad ps) ##
    safeWrapper getMem ##
    safeWrapper getTemp ##
    safeWrapper getUptime ##
    safeWrapper getVolumes ##
    safeWrapper getDate

putAll len =
    (putMPD len) +++
    autoPadL 1 (simple (printWrapper putCPULoad)) +++
    autoPadL 1 (simple (printWrapper putMem)) +++
    autoPadL 1 (simple (printWrapper putTemp)) +++
    autoPadL 1 (simple (printWrapper putUptime)) +++
    autoPadL 1 (simple (printWrapper putVolumes)) +++
    autoPadL 1 (simple (printWrapper putDate))
