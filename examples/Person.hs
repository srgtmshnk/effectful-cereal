{-# LANGUAGE DoAndIfThenElse   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
module Person where


import qualified ByteString.StrictBuilder as Builder
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Word
import           Effectful
import           Effectful.Cereal
import           Effectful.Error.Static


data Technology
    = Haskell
    | Elm
    | Git
    deriving Show


data Person = Person
    { name         :: ByteString
    , age          :: Word8
    , salaryBTC    :: Word32
        -- ^ in satoshi
    , workingStack :: [Technology]
    } deriving Show


-- | (length_FF_max)(data)
decodePascalString
    :: BinGet :> e
    => Eff e ByteString
decodePascalString =
    getByteString . fromIntegral =<< getWord8


-- | (length_FF_max)(data)
decodeList
    :: BinGet :> e
    => Eff e a -> Eff e [a]
decodeList f = do
    len <- getWord8
    replicateM (fromIntegral len) f


decodeTechnology
    :: (BinGet :> e, Error Text :> e)
    => Eff e Technology
decodeTechnology = do
    n <- getWord8
    if | n == 0    -> pure Haskell
       | n == 1    -> pure Elm
       | n == 2    -> pure Git
       | otherwise -> throwError ("Unknown technology " <> T.pack (show n))


decodePerson
    :: (BinGet :> e, Error Text :> e)
    => Eff e Person
decodePerson =
    Person
        <$> decodePascalString
        <*> getWord8
        <*> getWord32
        <*> decodeList decodeTechnology


encodePascalString
    :: (BinPut :> e, Error Text :> e)
    => ByteString
    -> Eff e ()
encodePascalString bs = do
    let len = BS.length bs

    if len > 0xFF then do
        throwError @Text "Your name is too long"
    else do
        putWord8 $ fromIntegral len
        putByteString bs


encodeTechnology
    :: BinPut :> e
    => Technology
    -> Eff e ()
encodeTechnology tech =
    case tech of
        Haskell -> putWord8 0
        Elm     -> putWord8 1
        Git     -> putWord8 2


encodeList
    :: (BinPut :> e, Error Text :> e)
    => (a -> Eff e ())
    -> [a]
    -> Eff e ()
encodeList f xs = do
    let len = length xs

    if len > 0xFF then do
        throwError @Text "Your list is too long"
    else do
        putWord8 $ fromIntegral len
        mapM_ f xs


encodePerson
    :: (BinPut :> e, Error Text :> e)
    => Person
    -> Eff e ()
encodePerson Person{..} = do
    encodePascalString name
    putWord8 age
    putWord32 salaryBTC
    encodeList encodeTechnology workingStack


main :: IO ()
main = do
    let person = Person "Bob" 32 0xAABBCCDD [Haskell, Git]
    print person
    mbBuilder <- runEff (runError @Text $ runBinPutLittleEndian (encodePerson person))
    case mbBuilder of
        Right builder -> do
            let blob = Builder.builderBytes builder
            print $ BS.unpack blob
            decodedPerson <- runEff $ runError @Text $ runBinGetLittleEndian blob decodePerson
            print decodedPerson

        Left err -> print err
