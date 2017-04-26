{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes             #-}

module Sgf.System.Configs.Options
    ( Config (..)
    , excludeFiles
    , opts
    )
  where

import           Prelude                hiding (FilePath)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import           Control.Applicative
import           Control.Monad.Reader
import           Options.Applicative
import qualified Data.Attoparsec.Text   as A
import qualified Data.Text              as T
import           Turtle.Prelude
import           Turtle.Shell
import           Turtle.Line
import           Filesystem.Path.CurrentOS (FilePath, fromText)
import           Control.Monad.Except
import           Control.Foldl (Fold (..))

import Sgf.Control.Lens
import Sgf.System.Configs.Types
import Sgf.System.Configs.Parser
import Sgf.System.Configs.Common
import Sgf.System.Configs.System


-- | Program own config.
data Config         = Config
                        { dpkgOutput        :: Shell (Either Line Line)
                        , dbFile            :: Maybe FilePath
                        , etcPath           :: FilePath
                        , targetPath        :: Maybe FilePath
                        , hashesList        :: CInfo -> [Md5]
                        , hashFilter        :: (CInfo -> [Md5]) -> CInfo -> Bool
                        , fileTypeFilter    :: CInfo -> Bool
                        , fileFilter        :: Maybe FilePath
                        , packageFilter     :: Package -> Bool
                        , targetFilter      :: CInfo -> Bool
                        }

-- | Options related to target directory.
targetFilterOpts :: Parser (CInfo -> Bool)
targetFilterOpts    =
        flag' (cmpBy (==))
            (  long "target-equal"
            <> help
                    ("Show source files, which are the same in target."))
    <|> flag' (fromMaybe False <$> cmpBy (liftA2 (/=)))
            (  long "target-differ"
            <> help
                    ("Show source files differing from target ones."))
    <|> flag' (isNothing . viewA targetFileHash)
            (  long "target-missed"
            <> help
                    ("Show source files missed in target."))
    <|> pure (fromMaybe True <$> cmpBy (liftA2 (/=)))
  where
    cmpBy :: (Maybe (Hash Computed) -> Maybe (Hash Computed) -> b)
           -> CInfo -> b
    cmpBy eq        = eq <$> viewA fileHash <*> viewA targetFileHash

-- | Options for generating list of hashes to filter from.
hashesListOpts :: Parser (CInfo -> [Md5])
hashesListOpts      = fromMaybe ((++) <$> allStored <*> allObsolete)
    . foldr mappend mempty
        <$> (flag Nothing (Just allStored)
                (   long "stored"
                <>  help
                      ("Search among files with not an obsolete hashes"
                    ++ " (may be combined with '--obsolete')."))
        <:> flag Nothing (Just allObsolete)
                (   long "obsolete"
                <>  help
                       ("Search among files with obsolete hashes"
                     ++ " (may be combined with '--stored')."))
        <:> pure [])

-- | Options for filtering list generated by 'hashesListOpts'.
hashFilterOpts :: Parser ((CInfo -> [Md5]) -> CInfo -> Bool)
hashFilterOpts      =
        flag' (viewAmaybe computed `neq`)
            (  long "changed"
            <> help
                      ("Show changed configs instead"
                    ++ " (not matching to selected hashes). "
                    ++ " By default show matched configs."))
    <|> flag' (\f -> null . f)
            (   long "other"
            <>  help
                    "Show files, which does not have hashes in 'dpkg' db.")
    <|> flag' (const (isNothing . viewAmaybe computed))
            (   long "errors-read"
            <>  help
                    "Show files, which can't be read.")
    <|> flag' (const (     (== mempty) . viewA package
                          <&&> not . null . viewA loadedHashes))
            (   long "errors-orphaned"
            <>  help
                    "Show files does not belonging to any package.")
    <|> flag' (const (isJust . viewAmaybe computed))
            (   long "all"
            <>  help
                    ("Do not filter files by computed hash,"
                    ++ " but still require, that i _can_ compute hash."))
    <|> pure (viewAmaybe computed `eq`)
  where
    -- | Generalize an `elem`. Empty list is a failure (default value is
    -- returned). Default value is 'False'.
    eq :: Eq a =>
             (CInfo -> Maybe a) -- ^ Obtain an element.
          -> (CInfo -> [a])     -- ^ Obtain a list to match with.
          -> CInfo -> Bool
    eq mh mhs x    = fromMaybe False . flip runReaderT x $ do
        h  <- ReaderT mh
        hs <- ReaderT (maybeList <$> mhs)
        return (h `elem` hs)
    -- | Generalize a `notElem`. Empty list is a failure (default value is
    -- returned). Default value is 'False'.
    neq :: Eq a =>
             (CInfo -> Maybe a) -- ^ Obtain an element.
          -> (CInfo -> [a])     -- ^ Obtain a list to match with.
          -> CInfo -> Bool
    neq mh mhs x    = fromMaybe False . flip runReaderT x $ do
        h  <- ReaderT mh
        hs <- ReaderT (maybeList <$> mhs)
        return (h `notElem` hs)

-- | Options for filtering by filetype.
fileTypeOpts :: Parser (CInfo -> Bool)
fileTypeOpts        = maybe (const True) (fmap getAny) . foldr mappend mempty
    <$> flag Nothing (Just $ Any <$> isFileInfo)
            (  long "files"
            <> help
                "Only search among files (may be combined '--symlinks').")
    <:> flag Nothing (Just $ Any <$> isSymLinkInfo)
            (  long "symlinks"
            <> help
                  ("Only search among symbolic links"
                ++ " (may be combined '--files')."))
    <:> pure []

packageOpts :: Parser (Package -> Bool)
packageOpts         = liftA2 (&&)
    <$> (pkgF    <$> ((some $ option readParser
            (  long "package"
            <> metavar "PACKAGE"
            <> help ("Filter `dpkg-query` output by package name "
                         ++ " (exact match only).")))
        <|> pure [A.takeText]))   -- This 'pure' is at 'Parser' level.
    <*> (statusF <$> ((some $ option readParser
            (  long "status"
            <> metavar "STATUS"
            <> help ("Filter `dpkg-query` output by package install status"
                         ++ " (exact match only).")))
        <|> pure [A.takeText]))    -- This 'pure' is at 'Parser' level.
  where
    readParser :: ReadM (A.Parser T.Text)
    readParser      = eitherReader (return . A.string . T.pack)
    viewM :: Monoid b => LensA a (Maybe b) -> a -> b
    viewM l         = fromMaybe mempty . viewA l
    pkgF :: [A.Parser T.Text] -> Package -> Bool
    pkgF ps         = byField (viewM pkgName)   (A.choice ps <* A.endOfInput)
    statusF :: [A.Parser T.Text] -> Package -> Bool
    statusF ps      = byField (viewM pkgStatus) (A.choice ps <* A.endOfInput)

opts :: Parser Config
opts                = Config
    <$> (   flag' (Right <$> stdin)
                (  long "stdin"
                <> help "Read `dpkg-query` output from stdin.")
            <|> fmap Right . input . fromString <$> strOption
                (  long "file"
                <> metavar "FILE"
                <> help "Read `dpkg-query` output from file.")
            <|> pure systemConfsWithPkg
        )
    <*> (   option (eitherReader readFilePath)
                (  long "db"
                <> value Nothing
                <> metavar "FILE"
                <> help "Store db to file.")
        )
    <*> (fromMaybe "/etc/"
            <$> option (eitherReader readFilePath)
                (  long "source"
                <> value Nothing
                <> metavar "DIR"
                <> help "Path to directory, where `/etc` is.")
        )
    <*> (option (eitherReader readFilePath)
            (  long "target"
            <> value Nothing
            <> metavar "DIR"
            <> help ("Directory to compare source files with.")))
    <*> hashesListOpts
    <*> hashFilterOpts
    <*> fileTypeOpts
    <*> (option (eitherReader readFilePath)
            (  long "filter"
            <> value Nothing
            <> metavar "FILE"
            <> help
                (   "Exclude filter file."
                ++  " Pathes must match *exactly* "
                ++  " and must be relative to source path.")))
    <*> packageOpts
    <*> targetFilterOpts
  where
    readFilePath :: String -> Either String (Maybe FilePath)
    readFilePath          = Right . Just . fromText . fromString

-- | Make a predicate from a list of excluded files read from a file.
excludeFiles :: Maybe FilePath -> P (CInfo -> Bool)
excludeFiles mf     = flip catchError def $ do
    f <- liftMaybe mf
    b <- testfile f
    if (not b)
      then throwError "Exclude file does not exist."
      else fold (readExcludes f) (Fold mappend mempty (fmap getAll))
  where
    def :: MonadError e m => e -> m (CInfo -> Bool)
    def _           = return (const True)
    -- | Read file with excludes and build predicates from read filenames.
    readExcludes :: FilePath -> Shell (CInfo -> All)
    readExcludes f  = do
        x <- fromText . lineToText <$> input f
        return (All <$> (/= x) . viewA filePath)

