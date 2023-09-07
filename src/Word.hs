module Word where

import           Data.Bits (shiftL, shiftR, (.&.), (.|.))
import           Data.Word (Word16, Word32, Word64, Word8)


twoBytesToWord16LE :: Word8 -> Word8 -> Word16
twoBytesToWord16LE b1 b2 =
    shiftL (fromIntegral b2) 8
        .|. fromIntegral b1


twoBytesToWord16BE :: Word8 -> Word8 -> Word16
twoBytesToWord16BE b1 b2 =
    shiftL (fromIntegral b1) 8
        .|. fromIntegral b2


fourBytesToWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fourBytesToWord32LE b1 b2 b3 b4 =
    shiftL (fromIntegral b4) 24
        .|. shiftL (fromIntegral b3) 16
        .|. shiftL (fromIntegral b2) 8
        .|. fromIntegral b1


fourBytesToWord32BE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
fourBytesToWord32BE b1 b2 b3 b4 =
    shiftL (fromIntegral b1) 24
        .|. shiftL (fromIntegral b2) 16
        .|. shiftL (fromIntegral b3) 8
        .|. fromIntegral b4


eightBytesToWord64LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
eightBytesToWord64LE b1 b2 b3 b4 b5 b6 b7 b8 =
    shiftL (fromIntegral b8) 56
        .|. shiftL (fromIntegral b7) 48
        .|. shiftL (fromIntegral b6) 40
        .|. shiftL (fromIntegral b5) 32
        .|. shiftL (fromIntegral b4) 24
        .|. shiftL (fromIntegral b3) 16
        .|. shiftL (fromIntegral b2) 8
        .|. fromIntegral b1


eightBytesToWord64BE :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word64
eightBytesToWord64BE b1 b2 b3 b4 b5 b6 b7 b8 =
    shiftL (fromIntegral b1) 56
        .|. shiftL (fromIntegral b2) 48
        .|. shiftL (fromIntegral b3) 40
        .|. shiftL (fromIntegral b4) 32
        .|. shiftL (fromIntegral b5) 24
        .|. shiftL (fromIntegral b6) 16
        .|. shiftL (fromIntegral b7) 8
        .|. fromIntegral b8


word16ToTwoBytesLE :: Word16 -> (Word8, Word8)
word16ToTwoBytesLE x =
    ( fromIntegral $ shiftR (fromIntegral x :: Word16) 8 .&. 0xFF
    , fromIntegral $ (fromIntegral x :: Word16) .&. 0xFF
    )

word16ToTwoBytesBE :: Word16 -> (Word8, Word8)
word16ToTwoBytesBE x =
    ( fromIntegral $ (fromIntegral x :: Word16) .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word16) 8 .&. 0xFF
    )

word32ToFourBytesLE :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToFourBytesLE x =
    ( fromIntegral $ shiftR (fromIntegral x :: Word32) 24 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 16 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 8 .&. 0xFF
    , fromIntegral $ (fromIntegral x :: Word32) .&. 0xFF
    )

word32ToFourBytesBE :: Word32 -> (Word8, Word8, Word8, Word8)
word32ToFourBytesBE x =
    ( fromIntegral $ (fromIntegral x :: Word32) .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 8 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 16 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 24 .&. 0xFF
    )

word64ToEightBytesLE :: Word32 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
word64ToEightBytesLE x =
    ( fromIntegral $ shiftR (fromIntegral x :: Word32) 56 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 48 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 40 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 32 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 24 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 16 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 8 .&. 0xFF
    , fromIntegral $ (fromIntegral x :: Word32) .&. 0xFF
    )

word64ToEightBytesBE :: Word32 -> (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8)
word64ToEightBytesBE x =
    ( fromIntegral $ (fromIntegral x :: Word32) .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 8 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 16 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 24 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 32 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 40 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 48 .&. 0xFF
    , fromIntegral $ shiftR (fromIntegral x :: Word32) 56 .&. 0xFF
    )
