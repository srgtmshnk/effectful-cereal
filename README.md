# effectful-cereal

Tiny library for binary serialization intended to use with `effectful`

# Warning
This library was not properly tested and benchmarked (although it's still pretty efficient), I use it for my pet projects only. **Do not use it in production**.

# Example
```haskell
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
```

```haskell
decodePascalString
    :: BinGet :> e
    => Eff e ByteString
decodePascalString =
    getByteString . fromIntegral =<< getWord8
```

Also check out the examples directory.
