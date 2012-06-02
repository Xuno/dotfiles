
import Data.Time
import System.Locale
import Sound.ALSA.Mixer
import Data.IORef

import System.Posix.Unistd (usleep)
import System.Posix.Files (fileExist)

import Control.Applicative ((<$>))
import Control.Monad (forever)

getDate :: String -> IO String
getDate format = getZonedTime >>= return . formatTime defaultTimeLocale format

getVolume :: IO (Integer, Integer, Integer, Bool)
getVolume = do
    Just control <- getControlByName "default" "Master"
    let Just playbackVolume = playback $ volume control
        Just playbackSwitch = playback $ switch control
    (min, max) <- getRange playbackVolume
    Just vol <- getChannel FrontLeft $ value $ playbackVolume
    Just sw <- getChannel FrontLeft playbackSwitch
    return (min, max, vol, sw)

getMem :: IO Double
getMem = do
    [total, free] <- map (read . (!!1) . words) . take 2 . lines <$> readFile "/proc/meminfo"
    return $ (total - free) / total

getUptime :: IO Double
getUptime = read . head . words <$> readFile "/proc/uptime"

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
    [a', b', c', d'] <- map read . take 4 . tail . words . head . lines <$> readFile "/proc/stat"
    writeIORef ref (e, f, ProcStat a' b' c' d')
    let all  = (a' + b' + c' + d') - (a + b + c + d)
        idle = d' - d
        ratio = (fromIntegral (all - idle) / fromIntegral all)
    return $ if all <= 0 || idle < 0 || idle > all then 0 else ratio

getTemp :: IO Double
getTemp = do
    temp0 <- parse zone0
    temp1 <- parse zone1
    return $ case (temp0, temp1) of
        (Nothing, Nothing) -> undefined
        (Just a,  Just b ) -> (a + b) / 2
        (Just a,  _      ) -> a
        (_,       Just b ) -> b
  where
    parse file = do
        exists <- fileExist file
        if exists then Just . (/1000) . read <$> readFile file else return Nothing
    zone0 = "/sys/class/thermal/thermal_zone0/temp"
    zone1 = "/sys/class/thermal/thermal_zone1/temp"

-- getMPD :: IO 
-- getBattery

main = do
    getDate "%F/%a %H:%M:%S" >>= putStrLn
    getVolume >>= print
    getMem >>= print
    getUptime >>= print
    getTemp >>= print
    ref <- initPS
    forever $ do
        usleep (1000 * 1000)
        getCPULoad ref >>= print . round . (*100)
    return ()
