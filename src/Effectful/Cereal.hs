{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE BlockArguments      #-}

module Effectful.Cereal
    ( BinGet(..)
    , BinPut
    , StateGet(..)
    , eof
    , getByteString
    , getBytesRead
    , getInt16
    , getInt32
    , getInt64
    , getInt8
    , getWord16
    , getWord32
    , getWord64
    , getWord8
    , interposeBinGetWithProgress
    , interposeBinPutWithProgress
    , lookAhead
    , putByteString
    , putInt16
    , putInt32
    , putInt64
    , putInt8
    , putWord16
    , putWord32
    , putWord64
    , putWord8
    , runBinGetBigEndian
    , runBinGetLittleEndian
    , runBinPutBigEndian
    , runBinPutLength
    , runBinPutLittleEndian
    , size
    , skip
    ) where

import qualified ByteString.StrictBuilder     as Builder
import           Control.Monad                (when)
import qualified Data.ByteString              as BS
import           Data.Generics.Labels         ()
import           Data.Int                     (Int16, Int32, Int64, Int8)
import           Data.Text                    (Text)
import           Data.Word                    (Word16, Word32, Word64, Word8)
import           Effectful
import           Effectful.Concurrent.STM
import           Effectful.Dispatch.Dynamic
import           Effectful.Error.Static
import           Effectful.State.Static.Local
import           Effectful.TH
import           GHC.Generics
import qualified Word


data StateGet = StateGet
    { input     :: BS.ByteString
    , bytesRead :: Int
    }
    deriving (Generic)


data BinGet :: Effect where
    GetWord8 :: BinGet m Word8
    GetWord16 :: BinGet m Word16
    GetWord32 :: BinGet m Word32
    GetWord64 :: BinGet m Word64
    LookAhead :: m a -> BinGet m a
    Skip :: Int -> BinGet m ()
    GetByteString :: Int -> BinGet m BS.ByteString
    GetBytesRead :: BinGet m Int
    Eof :: BinGet m Bool

makeEffect ''BinGet


{-# INLINE getWord8 #-}
{-# INLINE getWord16 #-}
{-# INLINE getWord32 #-}
{-# INLINE getWord64 #-}
{-# INLINE lookAhead #-}
{-# INLINE skip #-}
{-# INLINE getByteString #-}
{-# INLINE getBytesRead #-}

data BinPut :: Effect where
    PutWord8 :: Word8 -> BinPut m ()
    PutWord16 :: Word16 -> BinPut m ()
    PutWord32 :: Word32 -> BinPut m ()
    PutWord64 :: Word64 -> BinPut m ()
    Size :: m () -> BinPut m Int64
    PutByteString :: BS.ByteString -> BinPut m ()

makeEffect ''BinPut


{-# INLINE putWord8 #-}
{-# INLINE putWord16 #-}
{-# INLINE putWord32 #-}
{-# INLINE putWord64 #-}
{-# INLINE size #-}
{-# INLINE putByteString #-}


data Endianness
    = BigEndian
    | LittleEndian


runBinGetLittleEndian
    :: Error Text :> es
    => BS.ByteString
    -> Eff (BinGet : es) a
    -> Eff es a
runBinGetLittleEndian bs =
    reinterpret (evalState $ StateGet bs 0) $ \env -> \case
        GetWord8 ->
            getWord8'

        GetWord32 ->
            getWord32' LittleEndian

        GetWord16 ->
            getWord16' LittleEndian

        GetWord64 ->
            getWord64' LittleEndian

        GetByteString n ->
            getByteString' n

        Skip n ->
            modify @StateGet (\(StateGet input x) -> StateGet input $! x + n)

        GetBytesRead -> do
            StateGet {..} <- get
            return bytesRead

        Eof ->
            eof'

        LookAhead f ->
            localSeqUnlift
                env
                (\unlift -> do
                    !s <- get @StateGet
                    !res <- unlift f
                    put @StateGet $! s
                    return res
                )


runBinGetBigEndian
    :: Error Text :> es
    => BS.ByteString
    -> Eff (BinGet : es) a
    -> Eff es a
runBinGetBigEndian bs =
    reinterpret (evalState (StateGet bs 0)) $ \env -> \case
        GetWord8 ->
            getWord8'

        GetWord16 ->
            getWord16' BigEndian

        GetWord32 ->
            getWord32' BigEndian

        GetWord64 ->
            getWord64' BigEndian

        Skip n ->
            modify @StateGet (\(StateGet input x) -> StateGet input $! x + n)

        GetBytesRead -> do
            StateGet {..} <- get
            return bytesRead

        Eof ->
            eof'

        GetByteString n ->
            getByteString' n

        LookAhead f ->
            localSeqUnlift
                env
                (\unlift -> do
                    !s <- get @StateGet
                    !res <- unlift f
                    put @StateGet $! s
                    return res)


-- | Tiny progress showing how many bytes were read
interposeBinGetWithProgress
    :: (IOE :> es, Concurrent :> es, Error Text :> es)
    => TVar Int64
    -> Eff (BinGet : es) a
    -> Eff (BinGet : es) a
interposeBinGetWithProgress progress =
    let
        advanceIfAllowed :: (Concurrent :> e, State Bool :> e) => Int64 -> Eff e ()
        advanceIfAllowed n =
            get >>= flip when (atomically $ modifyTVar' progress (+ n))
    in impose
            (evalState True)
            \env -> \case
                GetWord8 ->
                    advanceIfAllowed 1 >> send GetWord8

                GetWord16 ->
                    advanceIfAllowed 2 >> send GetWord16

                GetWord32 ->
                    advanceIfAllowed 4 >> send GetWord32

                GetWord64 ->
                    advanceIfAllowed 8 >> send GetWord64

                Skip n ->
                    advanceIfAllowed (fromIntegral n) >> send (Skip n)

                GetByteString n ->
                    advanceIfAllowed (fromIntegral n) >> send (GetByteString n)

                GetBytesRead ->
                    send GetBytesRead

                Eof ->
                    send Eof

                LookAhead f -> do
                    put False
                    r <- send $
                        LookAhead $ localSeqUnlift
                            env
                            \unlift -> unlift f
                    put True
                    return r


{-# INLINE getWord8' #-}
getWord8'
    :: (State StateGet :> es, Error Text :> es)
    => Eff es Word8
getWord8' = do
    StateGet {..} <- get

    case BS.uncons $ BS.drop bytesRead input of
        Just (byte, _) -> do
            put $ StateGet input $! bytesRead + 1
            return $! byte

        Nothing ->
            throwError @Text "Not enough data"


{-# INLINE getWord16' #-}
getWord16'
    :: (State StateGet :> es, Error Text :> es)
    => Endianness
    -> Eff es Word16
getWord16' endianness = do
    !b1 <- getWord8'
    !b2 <- getWord8'

    case endianness of
        LittleEndian ->
            return $! Word.twoBytesToWord16LE b1 b2

        BigEndian ->
            return $! Word.twoBytesToWord16LE b1 b2


{-# INLINE getWord32' #-}
getWord32'
    :: ( State StateGet :> es, Error Text :> es)
    => Endianness
    -> Eff es Word32
getWord32' endianness = do
    !b1 <- getWord8'
    !b2 <- getWord8'
    !b3 <- getWord8'
    !b4 <- getWord8'

    case endianness of
        LittleEndian ->
            return $! Word.fourBytesToWord32LE b1 b2 b3 b4

        BigEndian ->
            return $! Word.fourBytesToWord32BE b1 b2 b3 b4


{-# INLINE getWord64' #-}
getWord64'
    :: ( State StateGet :> es, Error Text :> es)
    => Endianness
    -> Eff es Word64
getWord64' endianness = do
    !b1 <- getWord8'
    !b2 <- getWord8'
    !b3 <- getWord8'
    !b4 <- getWord8'
    !b5 <- getWord8'
    !b6 <- getWord8'
    !b7 <- getWord8'
    !b8 <- getWord8'

    case endianness of
        LittleEndian ->
            pure $! Word.eightBytesToWord64LE b1 b2 b3 b4 b5 b6 b7 b8

        BigEndian ->
            pure $! Word.eightBytesToWord64BE b1 b2 b3 b4 b5 b6 b7 b8


getByteString'
    :: (State StateGet :> es, Error Text :> es)
    => Int
    -> Eff es BS.ByteString
getByteString' n = do
    StateGet {..} <- get

    let bs = BS.take n $ BS.drop bytesRead input

    if BS.length bs == n then do
        put $ StateGet input $! bytesRead + n
        return bs
    else
        throwError @Text "Not enough data"


eof'
    :: State StateGet :> es
    => Eff es Bool
eof' = do
    StateGet{..} <- get
    return $ BS.length input == bytesRead


runBinPutLittleEndian
    :: Eff (BinPut : es) ()
    -> Eff es Builder.Builder
runBinPutLittleEndian =
    reinterpret (execState (mempty :: Builder.Builder)) $ \env -> \case
        PutWord8 w ->
            putWord8' w

        PutWord16 w ->
            putWord16' LittleEndian w

        PutWord32 w ->
            putWord32' LittleEndian w

        PutWord64 w ->
            putWord64' LittleEndian w

        PutByteString bs ->
            putByteString' bs

        Size f ->
            localSeqUnlift env \unlift -> unlift $ runBinPutLength f


runBinPutBigEndian
    :: Eff (BinPut : es) ()
    -> Eff es Builder.Builder
runBinPutBigEndian =
    reinterpret (execState (mempty :: Builder.Builder)) $ \env -> \case
        PutWord8 w ->
            putWord8' w

        PutWord16 w ->
            putWord16' BigEndian w

        PutWord32 w ->
            putWord32' BigEndian w

        PutWord64 w ->
            putWord64' BigEndian w

        PutByteString bs ->
            putByteString' bs

        Size f ->
            localSeqUnlift env \unlift -> unlift $ runBinPutLength f


runBinPutLength
    :: BinPut :> e
    => Eff e ()
    -> Eff e Int64
runBinPutLength = impose (execState (0 :: Int64)) $ \env -> \case
    PutWord8 _ ->
        modify @Int64 (($!) (+ 1))

    PutWord16 _ ->
        modify @Int64 (($!) (+ 2))

    PutWord32 _ ->
        modify @Int64 (($!) (+ 4))

    PutWord64 _ ->
        modify @Int64 (($!) (+ 8))

    PutByteString bs ->
        modify @Int64 (($!) (+ fromIntegral (BS.length bs)))

    Size f ->
        localSeqUnlift env \unlift -> unlift $ runBinPutLength f


-- | Tiny progress showing how many bytes were written
interposeBinPutWithProgress
    :: (IOE :> es, Concurrent :> es, Error Text :> es)
    => TVar Int64
    -> Eff (BinPut : es) a
    -> Eff (BinPut : es) a
interposeBinPutWithProgress progress =
    let
        advanceIfAllowed :: (Concurrent :> e, State Bool :> e) => Int64 -> Eff e ()
        advanceIfAllowed n =
            get >>= flip when (atomically $ modifyTVar' progress (+ n))
    in impose
            (evalState True)
            \env -> \case
                PutWord8 x ->
                    advanceIfAllowed 1 >> send (PutWord8 x)

                PutWord16 x ->
                    advanceIfAllowed 2 >> send (PutWord16 x)

                PutWord32 x ->
                    advanceIfAllowed 4 >> send (PutWord32 x)

                PutWord64 x ->
                    advanceIfAllowed 8 >> send (PutWord64 x)

                PutByteString bs -> do
                    advanceIfAllowed (fromIntegral $ BS.length bs)
                    send (PutByteString bs)

                Size f -> do
                    put False
                    r <- send $ Size $ localSeqUnlift env (\unlift -> unlift f)
                    put True
                    return r


{-# INLINE putWord8' #-}
putWord8'
    :: State Builder.Builder :> es
    => Word8
    -> Eff es ()
putWord8' w =
    modify (flip mappend $ Builder.storable w)


{-# INLINE putWord16' #-}
putWord16'
    :: State Builder.Builder :> es
    => Endianness
    -> Word16
    -> Eff es ()
putWord16' endianness w = do
    case endianness of
        LittleEndian ->
            modify $! flip mappend (Builder.storable w)

        BigEndian ->
            modify $! flip mappend (Builder.word16BE w)


{-# INLINE putWord32' #-}
putWord32'
    :: State Builder.Builder :> es
    => Endianness
    -> Word32
    -> Eff es ()
putWord32' endianness w = do
    case endianness of
        LittleEndian ->
            modify $! flip (<>) (Builder.storable w)

        BigEndian ->
            modify $! flip (<>) (Builder.word32BE w)


{-# INLINE putWord64' #-}
putWord64'
    :: State Builder.Builder :> es
    => Endianness
    -> Word64
    -> Eff es ()
putWord64' endianness w = do
    case endianness of
        LittleEndian ->
            modify $! flip mappend (Builder.storable w)

        BigEndian ->
            modify $! flip mappend (Builder.word64BE w)


{-# INLINE putByteString' #-}
putByteString'
    :: State Builder.Builder :> es
    => BS.ByteString
    -> Eff es ()
putByteString' bs =
    modify (flip mappend $! Builder.bytes bs)


{-# INLINE getInt8 #-}
{-# INLINE getInt16 #-}
{-# INLINE getInt32 #-}
{-# INLINE getInt64 #-}
{-# INLINE putInt8 #-}
{-# INLINE putInt16 #-}
{-# INLINE putInt32 #-}
{-# INLINE putInt64 #-}

getInt8
    :: BinGet :> e
    => Eff e Int8
getInt8 =
    fromIntegral <$> getWord8

getInt16
    :: BinGet :> e
    => Eff e Int16
getInt16 =
    fromIntegral <$> getWord16

getInt32
    :: BinGet :> e
    => Eff e Int32
getInt32 =
    fromIntegral <$> getWord32

getInt64
    :: BinGet :> e
    => Eff e Int64
getInt64 =
    fromIntegral <$> getWord64

putInt8
    :: BinPut :> e
    => Int8
    -> Eff e ()
putInt8 =
    putWord8 . fromIntegral

putInt16
    :: BinPut :> e
    => Int16
    -> Eff e ()
putInt16 =
    putWord16 . fromIntegral

putInt32
    :: BinPut :> e
    => Int32
    -> Eff e ()
putInt32 =
    putWord32 . fromIntegral

putInt64
    :: BinPut :> e
    => Int64
    -> Eff e ()
putInt64 =
    putWord64 . fromIntegral
