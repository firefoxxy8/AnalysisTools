module Lichen.Implementation.Sources.Submitty where

import System.IO
import System.Process
import System.Directory
import System.FilePath

import Data.Aeson
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Arrow (second)

import Text.Read

import Lichen.Utility.Miscellaneous
import Lichen.Architecture.Error
import Lichen.Architecture.Lexer
import Lichen.Architecture.Language
import Lichen.Implementation.Plagiarism.Config

data VersionTime = VersionTime { version :: Int, time :: String }
instance FromJSON VersionTime where parseJSON = withObject "version_time" $ \o -> VersionTime <$> o .: "version" <*> o .: "time"
data AssignmentSettings = AssignmentSettings { activeVersion :: Int, history :: [VersionTime]}
instance FromJSON AssignmentSettings where parseJSON = withObject "assignment_settings" $ \o -> AssignmentSettings <$> o .: "active_version" <*> o .: "history"

getAssignmentSettings :: FilePath -> Plagiarism AssignmentSettings
getAssignmentSettings p = do
        b <- liftIO $ doesFileExist p
        if b
            then do c <- liftIO $ BS.readFile p
                    case eitherDecode c of Left e -> throwError . JSONDecodingError $ T.pack e
                                           Right t -> return t
            else throwError . JSONDecodingError . T.pack $ "Assignment settings file \"" ++ p ++ "\" not found."

getVersionTime :: AssignmentSettings -> Int -> String
getVersionTime a v = go (history a) where
    go (VersionTime v' s:xs) | v == v' = s
                             | otherwise = go xs
    go [] = ""

getStudentTime :: FilePath -> Plagiarism String
getStudentTime p = do
        as <- getAssignmentSettings p
        return . getVersionTime as $ activeVersion as

data Semester = Semester { semesterName :: T.Text, semesterCourses :: [Course] }
data Course = Course { courseName :: T.Text, courseAssignments :: [Assignment] }
data Assignment = Assignent { assignmentName :: T.Text, assignmentStudents :: [Student] }
data Student = Student { studentName :: T.Text, studentActiveSubmission :: Integer, studentSubmissions :: [Submission] }
data Submission = Submission { submissionVersion :: Integer, submissionFingerprints :: [Fingerprint] }
type Fingerprint = Tagged Int

-- Given a student submission directory, parse the students
-- user_assignment_settings.json file and return the path to the directory
-- containing that student's active submission.
findActive :: FilePath -> Plagiarism FilePath
findActive p = do
        active <- activeVersion <$> getAssignmentSettings (p </> "user_assignment_settings.json")
        if active == 0 then findLatest p else return $ p </> show active

findLatest :: FilePath -> Plagiarism FilePath
findLatest p = liftIO $ (p </>) . show . maximum . purify . fmap rint <$> listDirectory p
    where rint :: String -> Maybe Integer
          rint = readMaybe

findAll :: FilePath -> Plagiarism [FilePath]
findAll p = liftIO $ listDirectory p

-- Given a destination file and a list of source files, call UNIX cat to
-- concatenate the source files and output the result in the destination
-- file.
runCat :: FilePath -> [FilePath] -> IO ()
runCat dst srcs | null srcs = return ()
                | otherwise = do
                    out <- openFile dst WriteMode
                    (_, _, _, ph) <- createProcess (proc "/bin/cat" srcs) { std_out = UseHandle out }
                    void $ waitForProcess ph

concatenateActive :: FilePath -> Plagiarism ()
concatenateActive p = do
        config <- ask
        studentDirs <- liftIO $ fmap (\x -> p </> x) <$> listDirectory p
        students <- mapM (clean . second findActive) $ zip studentDirs studentDirs
        dstSrc <- liftIO $ mapM (toDstSrc config) students
        liftIO . mapM_ (\x -> removeDir x >> createDirectoryIfMissing True x) . Set.fromList $ containingDir . fst <$> dstSrc
        liftIO $ mapM_ (uncurry runCat) dstSrc
    where wd c = outputDir c </> concatDir c
          clean (s, m) = (,) <$> pure s <*> m
          toDstSrc c (s, active) = (,) <$> ((wd c ++) <$> canonicalizePath s)
                                       <*> (filter (\x -> takeExtension x `elem` exts (language c)) . fmap (\x -> active </> x) <$> listDirectory active)

concatenateAll :: FilePath -> Plagiarism ()
concatenateAll p = do
        config <- ask
        studentDirs <- liftIO $ fmap (\x -> p </> x) <$> listDirectory p
        students <- mapM (clean . second findAll) $ zip studentDirs studentDirs
        dstSrc <- liftIO $ mconcat <$> mapM (toDstSrc config) students
        liftIO . mapM_ (\x -> removeDir x >> createDirectoryIfMissing True x) . Set.fromList $ containingDir . fst <$> dstSrc
        liftIO $ mapM_ (uncurry runCat) dstSrc
    where wd c = outputDir c </> concatDir c
          clean (s, m) = (,) <$> pure s <*> m
          toDstSrc :: Config -> (FilePath, [FilePath]) -> IO [(FilePath, [FilePath])]
          toDstSrc c (s, active) = mapM (\a -> (,) <$> ((++ a) . (++ "_") . (wd c ++) <$> canonicalizePath s)
                                                  <*> (filter (\x -> takeExtension x `elem` exts (language c)) . fmap (\x -> a </> x) <$> listDirectory a))
                                        active
