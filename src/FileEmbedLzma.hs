{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
------------------------------------------------------------
-- |
-- Module      :  FileEmbedLzma
-- Copyright   :  (c) 2015-2018 Futurice, 2018 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
----------------------------------------------------------------------------
module FileEmbedLzma (
    -- * Embed files
    embedByteString,
    embedLazyByteString,
    embedText,
    embedLazyText,
    -- * Embed directories
    embedDir,
    embedRecursiveDir,
    -- * Internal
    -- ** Directory listing
    listDirectoryFiles,
    listRecursiveDirectoryFiles,
    listDirectoryFilesF,
    -- ** Template Haskell
    lazyBytestringE,
    ) where

import Prelude ()
import Prelude.Compat

import Control.Arrow                    (first)
import Control.Monad                    (forM)
import Control.Monad.Trans.State.Strict (runState, state)
import Data.Foldable                    (for_)
import Data.Functor.Compose             (Compose (..))
import Data.Int                         (Int64)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax       (qAddDependentFile)
import System.Directory
       (doesDirectoryExist, getDirectoryContents)
import System.FilePath                  (makeRelative, (</>))
import System.IO.Unsafe                 (unsafePerformIO)

import qualified Codec.Compression.Lzma  as LZMA
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BSL
import qualified Data.ByteString.Unsafe  as BS.Unsafe
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TLE

import Instances.TH.Lift ()

listRecursiveDirectoryFiles :: FilePath -> IO [(FilePath, BSL.ByteString)]
listRecursiveDirectoryFiles = listDirectoryFilesF listRecursiveDirectoryFiles

listDirectoryFiles  :: FilePath -> IO [(FilePath, BSL.ByteString)]
listDirectoryFiles = listDirectoryFilesF (\_ -> return [])

listDirectoryFilesF
    :: (FilePath -> IO [(FilePath, BSL.ByteString)]) -- ^ what to do with a sub-directory
    -> FilePath -> IO [(FilePath, BSL.ByteString)]
listDirectoryFilesF go topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
        then go path
        else do
            contents <- BSL.readFile path
            return [(path, contents)]
    return (concat paths)

makeAllRelative :: FilePath -> [(FilePath, a)] -> [(FilePath, a)]
makeAllRelative topdir = map (first (("/" ++) . makeRelative topdir))

-- | Makes lazy 'BSL.ByteString' expression.
-- Embedded value is compressed with LZMA.
lazyBytestringE :: BSL.ByteString -> Q Exp
lazyBytestringE lbs =
    [| LZMA.decompress
    $ BSL.fromStrict
    $ unsafePerformIO
    $ BS.Unsafe.unsafePackAddressLen $l $s
    :: BSL.ByteString
    |]
  where
    bs = BSL.toStrict $ LZMA.compressWith params lbs
    s = litE $ stringPrimL $ BS.unpack bs
    l = litE $ integerL $ fromIntegral $ BS.length bs

    params = LZMA.defaultCompressParams
        {- doesn't seem to affect much
        { LZMA.compressLevel = LZMA.CompressionLevel9
        , LZMA.compressLevelExtreme = True
        }
        -}

makeEmbeddedEntry :: Name -> (FilePath, (Int64, Int64)) -> Q Exp
makeEmbeddedEntry name (path, (off, len)) =
    [| (path, BSL.toStrict $ BSL.take len $ BSL.drop off $(varE name)) |]

concatEntries :: Traversable t => t BSL.ByteString -> (BSL.ByteString, t (Int64, Int64))
concatEntries xs = (bslEndo BSL.empty, ys)
  where
    (ys, (_, bslEndo)) = runState (traverse (state . single) xs) (0, id)

    single
        :: BSL.ByteString                             -- file bytestring
        -> (Int64, BSL.ByteString -> BSL.ByteString)  -- current offset, buffer so far
        -> ((Int64, Int64), (Int64, BSL.ByteString -> BSL.ByteString))
    single bsl (off, endo) = ((off, l), (off + l, endo . BSL.append bsl))
      where
        l = fromIntegral $ BSL.length bsl

-------------------------------------------------------------------------------
-- Directories
-------------------------------------------------------------------------------

-- | Embed a @[('FilePath', 'Data.ByteString.ByteString')]@ list, traversing given directory.
embedDir :: FilePath -> Q Exp
embedDir topdir = do
    pairs' <- runIO $ listDirectoryFiles topdir
    for_ pairs' $ qAddDependentFile . fst
    let pairs = makeAllRelative topdir pairs'
    embedPairs pairs

embedPairs :: [(FilePath, BSL.ByteString)] -> Q Exp
embedPairs pairs = do
    -- we do a hop to only embed single big bytestring.
    -- it's beneficial as lzma have more stuff to compress
    let (bsl, Compose offsets) = concatEntries (Compose pairs)
    bslName <- newName "embedBsl"
    bslExpr <- lazyBytestringE bsl
    let e = letE [ return $ ValD (VarP bslName) (NormalB bslExpr) [] ] $
                listE $ map (makeEmbeddedEntry bslName) offsets
    sigE e [t| [(FilePath, BS.ByteString)] |]

-- | Embed a @[('FilePath', 'Data.ByteString.ByteString')]@ list, recursively traversing given directory path.
--
-- For example, with @wai-static-app@ this can be used as:
--
-- @
-- staticApp $ embeddedSettings $('embedRecursiveDir' "static")
-- -- is an embedded (no data-files!) equivalent of
-- staticApp $ defaultFileServerSettings "static"
-- @
embedRecursiveDir :: FilePath -> Q Exp
embedRecursiveDir topdir = do
    pairs' <- runIO $ listRecursiveDirectoryFiles topdir
    for_ pairs' $ qAddDependentFile . fst
    let pairs = makeAllRelative topdir pairs'
    embedPairs pairs

-------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------

-- | Embed a lazy 'Data.ByteString.Lazy.ByteString' from a file.
embedLazyByteString :: FilePath -> Q Exp
embedLazyByteString fp = do
    qAddDependentFile fp
    bsl <- runIO $ BSL.readFile fp
    lazyBytestringE bsl

-- | Embed a strict 'Data.ByteString.ByteString' from a file.
embedByteString :: FilePath -> Q Exp
embedByteString fp = [| BSL.toStrict $(embedLazyByteString fp) :: BS.ByteString |]

-- | Embed a lazy 'Data.Text.Lazy.Text' from a UTF8-encoded file.
embedLazyText :: FilePath -> Q Exp
embedLazyText fp = do
    qAddDependentFile fp
    bsl <- runIO $ BSL.readFile fp
    case TLE.decodeUtf8' bsl of
        Left e  -> reportError (show e)
        Right _ -> return ()
    [| TLE.decodeUtf8 $ $(lazyBytestringE bsl) :: TL.Text |]

-- | Embed a strict 'Data.Text.Text' from a UTF8-encoded file.
embedText :: FilePath -> Q Exp
embedText fp = [| TL.toStrict $(embedLazyText fp) :: T.Text |]
