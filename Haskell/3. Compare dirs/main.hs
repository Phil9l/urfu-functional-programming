import System.Directory
import System.FilePath ((</>))
import Control.Monad(forM)
import Data.List
import System.Posix.Files
import Data.ByteString.Lazy as Bytes (readFile)
import System.Environment (getArgs)


doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    directoryExists <- doesDirectoryExist path
    fileExists <- doesFileExist path
    return(directoryExists || fileExists)
    

compareFileStatus :: FileStatus -> FileStatus -> IO String
compareFileStatus stats1 stats2 = do
    -- TODO: https://www.haskell.org/hoogle/?hoogle=FileStatus
    if not(fileMode(stats1) == fileMode(stats2)) then do
        return "Filemode"
    else if not(fileGroup(stats1) == fileGroup(stats2)) then do
        return "fileGroup"
    else if not(fileSize(stats1) == fileSize(stats2)) then do
        return "File size"
    else if not(accessTime(stats1) == accessTime(stats2)) then do
        return "Access time"
    else if not(modificationTime(stats1) == modificationTime(stats2)) then do
        return "Modification time"
    else
        return ""


compareFiles :: FilePath -> FilePath -> IO ()
compareFiles path1 path2 = do
    file1Contents <- Bytes.readFile path1
    file2Contents <- Bytes.readFile path2
    
    
    if not(file1Contents == file2Contents) 
        then do
            putStrLn("Contents of " ++ path1 ++ " and " ++ path2 ++ " differs")
        else do
            stat1 <- getFileStatus path1
            stat2 <- getFileStatus path2
            
            error <- compareFileStatus stat1 stat2
            
            if not(error == "") then do
                putStrLn(error ++ " of " ++ path1 ++ " and " ++ path2 ++ " differs.")
                return()
            else
                return ()


compareFolders :: FilePath -> FilePath -> IO ()
compareFolders root1 root2 = do
    names1 <- getDirectoryContents root1
    names2 <- getDirectoryContents root2
    
    let mergedNames = nub (concat [names1, names2]) 
    let names = filter (`notElem` [".", ".."]) mergedNames


    forM names $ \name -> do
        let path1 = root1 </> name
        let path2 = root2 </> name

        path1Exists <- doesPathExist path1
        path2Exists <- doesPathExist path2

        if path1Exists && path2Exists
            then do
                path1IsDirectory <- doesDirectoryExist path1
                path2IsDirectory <- doesDirectoryExist path2
                
                if path1IsDirectory && path2IsDirectory
                    then do
                        compareFolders path1 path2
                        return ()
                    else do
                        if (path1IsDirectory && not (path2IsDirectory)) || (not (path1IsDirectory) && path2IsDirectory)
                            then putStrLn("One of [" ++ path1 ++ ", " ++ path2 ++ "] is directory, the other is file")
                            else do
                                compareFiles path1 path2
                                return ()
            else putStrLn("Exists only one from [" ++ path1 ++ ", " ++ path2 ++ "]")
    return ()


main = do
    args <- getArgs
    if not((length args) == 2) then
        putStrLn("2 arguments required")
    else
        compareFolders (args !! 0) (args !! 1)
