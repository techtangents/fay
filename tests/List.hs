{-# LANGUAGE NoImplicitPrelude #-}

module List where

import           Language.Fay.FFI
import           Language.Fay.Prelude

main = print (showList (take 5 (let ns = 1 : map' (\x -> x + 1) ns in ns)))

take 0 _      = []
take n (x:xs) = x : take (n - 1) xs

map' f []     = []
map' f (x:xs) = f x : map' f xs

print :: String -> Fay ()
print = ffi "console.log(%1)"

showList :: [Double] -> String
showList = ffi "JSON.stringify(%1)"
