{-# LANGUAGE CPP                   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
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
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.ByteString.Unsafe  as BS.Unsafe
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LTE

#if MIN_VERSION_template_haskell(2,16,0)
import qualified Data.ByteString.Internal as BS.Internal

import Language.Haskell.TH.Syntax (Bytes (..))
#endif

listRecursiveDirectoryFiles :: FilePath -> IO [(FilePath, LBS.ByteString)]
listRecursiveDirectoryFiles = listDirectoryFilesF listRecursiveDirectoryFiles

listDirectoryFiles  :: FilePath -> IO [(FilePath, LBS.ByteString)]
listDirectoryFiles = listDirectoryFilesF (\_ -> return [])

listDirectoryFilesF
    :: (FilePath -> IO [(FilePath, LBS.ByteString)]) -- ^ what to do with a sub-directory
    -> FilePath -> IO [(FilePath, LBS.ByteString)]
listDirectoryFilesF go topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
        then go path
        else do
            contents <- LBS.readFile path
            return [(path, contents)]
    return (concat paths)

makeAllRelative :: FilePath -> [(FilePath, a)] -> [(FilePath, a)]
makeAllRelative topdir = map (first (("/" ++) . makeRelative topdir))

-- | Makes lazy 'LBS.ByteString' expression.
-- Embedded value is compressed with LZMA.
lazyBytestringE :: LBS.ByteString -> Q Exp
lazyBytestringE lbs =
    [| LZMA.decompress . LBS.fromStrict . unsafePerformIO |] `appE`
    ([| BS.Unsafe.unsafePackAddressLen |] `appE` l `appE` s)
  where
    bs = LBS.toStrict $ LZMA.compressWith params lbs
#if MIN_VERSION_template_haskell(2,16,0)
    s = litE $ bytesPrimL $ bsToBytes bs
#else
    s = litE $ stringPrimL $ BS.unpack bs
#endif
    l = litE $ integerL $ fromIntegral $ BS.length bs

    params = LZMA.defaultCompressParams
        {- doesn't seem to affect much
        { LZMA.compressLevel = LZMA.CompressionLevel9
        , LZMA.compressLevelExtreme = True
        }
        -}

#if MIN_VERSION_template_haskell(2,16,0)
bsToBytes :: BS.ByteString -> Bytes
bsToBytes (BS.Internal.PS fptr off len) = Bytes fptr (fromIntegral off) (fromIntegral len)
#endif

makeEmbeddedEntry :: Name -> (FilePath, (Int64, Int64)) -> Q Exp
makeEmbeddedEntry name (path, (off, len)) = do
    let y = [| LBS.toStrict . LBS.take len . LBS.drop off |] `appE` varE name
    [| (,) path |] `appE` y

concatEntries :: Traversable t => t LBS.ByteString -> (LBS.ByteString, t (Int64, Int64))
concatEntries xs = (bslEndo LBS.empty, ys)
  where
    (ys, (_, bslEndo)) = runState (traverse (state . single) xs) (0, id)

    single
        :: LBS.ByteString                             -- file bytestring
        -> (Int64, LBS.ByteString -> LBS.ByteString)  -- current offset, buffer so far
        -> ((Int64, Int64), (Int64, LBS.ByteString -> LBS.ByteString))
    single bsl (off, endo) = ((off, l), (off + l, endo . LBS.append bsl))
      where
        l = fromIntegral $ LBS.length bsl

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

embedPairs :: [(FilePath, LBS.ByteString)] -> Q Exp
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
    bsl <- runIO $ LBS.readFile fp
    lazyBytestringE bsl

-- | Embed a strict 'Data.ByteString.ByteString' from a file.
embedByteString :: FilePath -> Q Exp
embedByteString fp = [| LBS.toStrict |] `appE` embedLazyByteString fp

-- | Embed a lazy 'Data.Text.Lazy.Text' from a UTF8-encoded file.
embedLazyText :: FilePath -> Q Exp
embedLazyText fp = do
    qAddDependentFile fp
    bsl <- runIO $ LBS.readFile fp
    case LTE.decodeUtf8' bsl of
        Left e  -> reportError (show e)
        Right _ -> return ()
    [| LTE.decodeUtf8 |] `appE` lazyBytestringE bsl

-- | Embed a strict 'Data.Text.Text' from a UTF8-encoded file.
embedText :: FilePath -> Q Exp
embedText fp = [| LT.toStrict |] `appE` embedLazyText fp
