{-# LANGUAGE PatternGuards #-}

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process

readTagsCmd :: FilePath -> IO String
readTagsCmd file
    | extname `elem` [".hs", ".lhs"] =
        readProcess "hasktags" ["-c", "-o", "-", file] ""
    | otherwise                      =
        readProcess "ctags" ["-o", "-", file] ""
  where
    extname = takeExtensions file

filterByDir :: (String -> Bool) -> String -> Bool
filterByDir test line
    | (_:fileName:_) <- splitsByTab line = test fileName
    | otherwise                          = False
  where
    splitsByTab []        = [""]
    splitsByTab ('\t':xs) = "" : splitsByTab xs
    splitsByTab (x:xs)    = let sp = splitsByTab xs in (x:head sp):tail sp

genTags :: FilePath -> FilePath -> IO ()
genTags tags src = do
    hPutStrLn stderr ("generate tags for " ++ src ++ " to " ++ tags)
    let dir     = takeDirectory tags
        isSrc x = getAbsolutePath dir x `equalFilePath` src
    origin <- readFile tags
    updates <- readTagsCmd src
    let originL  = filter (filterByDir (not.isSrc)) (lines origin)
        updatesL = filter (filterByDir isSrc) (lines updates)
        newTags  = unlines $ sort (originL ++ updatesL)
    newTags `deepseq` writeFile tags newTags

autogen :: FilePath -> IO ()
autogen path = loop pathList
  where
    (directory, file) = splitFileName path
    pathList = splitPath directory

    loop [] = hPutStrLn stderr ("tags file for " ++ path ++ " not found")
    loop xs = do
        let tagsTarget = normalise (joinPath xs </> "tags")
        perm <- isWritable tagsTarget
        if perm then genTags tagsTarget path else loop (init xs)

getAbsolutePath :: FilePath -> FilePath -> FilePath
getAbsolutePath _ path | isAbsolute path = normalise path
getAbsolutePath pwd path = normalise (pwd </> path)

(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
a <&&> b = do
    ra <- a
    if ra then b else return False

isReadable :: FilePath -> IO Bool
isReadable path = doesFileExist path <&&> (readable <$> getPermissions path)

isWritable :: FilePath -> IO Bool
isWritable path = doesFileExist path <&&> (writable <$> getPermissions path)

main :: IO ()
main = do
    args <- filter isValid <$> getArgs
    files <- filterM isReadable args
    pwd <- getCurrentDirectory
    mapM_ (autogen . getAbsolutePath pwd) files
