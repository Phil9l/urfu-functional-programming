import System.Directory
import System.FilePath ((</>))
import Control.Monad(forM)
import Data.List
import System.Posix.Files
import Data.ByteString.Lazy as Bytes (readFile)
import System.Environment (getArgs)


-- Function to check if file or folder exists.
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    -- Check if path is directory.
    directoryExists <- doesDirectoryExist path
    -- Check if path is file.
    fileExists <- doesFileExist path
    -- If either first or second is true, then return true.
    return(directoryExists || fileExists)
    

-- Function to compare file statuses (access time, modification time, etc).
-- Return name of attributes that differs.
-- Returns empty string, if all attributes are equal.
compareFileStatus :: FileStatus -> FileStatus -> IO String
compareFileStatus stats1 stats2 = do
    -- https://hackage.haskell.org/package/unix-2.7.2.1/docs/System-Posix-Files.html
    -- Compare file modes (such as permission).
    if not(fileMode(stats1) == fileMode(stats2)) then do
        return "Filemode"
    -- Compare file groups.
    else if not(fileGroup(stats1) == fileGroup(stats2)) then do
        return "fileGroup"
    -- Compare file sizes (doesn't really matter, because we alreay compared files contents, but never mind).
    else if not(fileSize(stats1) == fileSize(stats2)) then do
        return "File size"
    -- Compare access times.
    else if not(accessTime(stats1) == accessTime(stats2)) then do
        return "Access time"
    -- Compare modification times.
    else if not(modificationTime(stats1) == modificationTime(stats2)) then do
        return "Modification time"
    -- Return that attributes are equal.
    else
        return ""


-- Function to compare 2 files: contents and attributes.
compareFiles :: FilePath -> FilePath -> IO ()
compareFiles path1 path2 = do
    -- Read binary file from path1.
    file1Contents <- Bytes.readFile path1
    -- Read binary file from path2.
    file2Contents <- Bytes.readFile path2
    
    if not(file1Contents == file2Contents) 
        -- If contents differs, print this error.
        then do
            putStrLn("Contents of " ++ path1 ++ " and " ++ path2 ++ " differs")
        -- Compare attributes otherwise.
        else do
            -- Get attributes of first file.
            stat1 <- getFileStatus path1
            -- Get attributes of second file.
            stat2 <- getFileStatus path2
            
            -- Compare attributes of these files.
            error <- compareFileStatus stat1 stat2
            
            -- Print error if not equal.
            if not(error == "") then do
                putStrLn(error ++ " of " ++ path1 ++ " and " ++ path2 ++ " differs.")
                return()
            else
                return ()


-- Compare 2 folders recursively.
compareFolders :: FilePath -> FilePath -> IO ()
compareFolders root1 root2 = do
    -- Get contents of both folders.
    names1 <- getDirectoryContents root1
    names2 <- getDirectoryContents root2
    
    -- Get list of all file and folder names in both directories, remove duplicates.
    let mergedNames = nub (concat [names1, names2]) 
    -- Remove "." and ".." from file list.
    let names = filter (`notElem` [".", ".."]) mergedNames

    -- Iterate over all file and folder names.
    forM names $ \name -> do
        -- Get full path of both files.
        let path1 = root1 </> name
        let path2 = root2 </> name

        -- Check if this file exists in both first and second folder.
        path1Exists <- doesPathExist path1
        path2Exists <- doesPathExist path2

        if path1Exists && path2Exists
            -- If exists do following.
            then do
                -- Check if both are directories.
                path1IsDirectory <- doesDirectoryExist path1
                path2IsDirectory <- doesDirectoryExist path2
                
                if path1IsDirectory && path2IsDirectory
                    -- If both are directories, then compare them recursively.
                    then do
                        compareFolders path1 path2
                        return ()
                    else do
                        if (path1IsDirectory && not (path2IsDirectory)) || (not (path1IsDirectory) && path2IsDirectory)
                            -- If one is file and the other is directory, print error.
                            then putStrLn("One of [" ++ path1 ++ ", " ++ path2 ++ "] is directory, the other is file")
                            -- Compare as files otherwise.
                            else do
                                compareFiles path1 path2
                                return ()
            -- If file exists only in one folder, print error.
            else putStrLn("Exists only one from [" ++ path1 ++ ", " ++ path2 ++ "]")
    return ()


main = do
    -- Get arguments.
    args <- getArgs
    -- Print error if number of arguments not equal to 2.
    if not((length args) == 2) then
        putStrLn("2 arguments required")
    -- Run function to compare folders.
    else
        compareFolders (args !! 0) (args !! 1)
