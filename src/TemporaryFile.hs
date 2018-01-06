module TemporaryFile
  where
import           Path             (fromAbsFile, parseAbsDir, parseRelFile,
                                   (</>))
import           System.Directory (getTemporaryDirectory)

getTemporaryFile :: String -> IO FilePath
getTemporaryFile filename = do
  tempDir <- getTemporaryDirectory
  tempDirPath <- parseAbsDir tempDir
  tempFilePath <- parseRelFile filename
  return $ fromAbsFile $ tempDirPath </> tempFilePath
