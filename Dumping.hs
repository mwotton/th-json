module Dumping where
import Data.Maybe
parseRec k (Dict d) = lookup k d
parseRec _ _ = Nothing

parseInt (Number c) = return c
parseInt _ = Nothing
parseFloat (Flt f) = return f
parseFloat _ = Nothing                     

parseArr (Arr r) = return r
parseArr _ = Nothing

parseString (Str s) = return s
parseString _ = Nothing

data Struct = Arr [Struct]
            | Dict [(String,Struct)]
            | Number Int
            | Flt Float
            | Str String

other (Dict d) = listToMaybe . mapMaybe (\(key,p) -> lookup key d >>= p)
other _ = \_ -> Nothing
