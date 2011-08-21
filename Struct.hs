{-# LANGUAGE TemplateHaskell #-}
module Struct where
import Dumping

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe
import Data.Char
import Control.Applicative
import Debug.Trace

data Simple = SCon Int
            | SF Float
            deriving Show

newtype F = F1 Int
-- parseSimple = (flip other) parseables
--   where parseables = [("scon", \s -> SCon <$> parseInt s )]
--         Other = \d -> listToMaybe . mapMaybe (\(key,p) -> lookup key d >>= p) 
buildParser x = do 
  info <- reify x 
  trace (show info) (return ())
  let parseable = 
          case info of
                 TyConI (DataD _ _ _ (NormalC s [(con1,ty)]:rest) _ ) -> do
                   l  <- trace (show (ty,parsers,con1,lookup (show ty) parsers, rest)) $ newName "foo"
                   -- (canonicalise s,
                   -- \s -> SCon <$> parseInt s )]
                   listE [tupE [ stringE $ canonicalise s,
                          lamE [varP l] ((varE '(<$>))    `appE`
                                         (conE 'SCon) `appE`

                                         --(varE 'parseInt `appE` varE l))
                                         (varE (fromJust $ lookup (show ty) parsers) `appE` varE l))
                        ]]
                 _ -> error "don't tase me, bro"
  appE [| flip other |] parseable
    where 
      -- other :: [(String, Struct)] -> Maybe Simple
      canonicalise  = map toLower . reverse.takeWhile (/= '.') . reverse . show
      parsers = [("ConT GHC.Types.Int", 'parseInt)
                ,("ConT GHC.Types.Float", 'parseFloat)
                ]

-- getCons :: Q [Con]
getCons s = do
  info <- reify s
  case info of
    TyConI (DataD _ _ _ cons _) -> return cons
    _ -> return [] -- or throw an error

--
-- A more detailed example where I turn constructors into
-- patterns with one or more wildcard patterns in them. e.g.
-- 
-- @Str String@ becomes pattern @(Str _)@
-- @A Int Float@ becomes pattern @(A _ _)@
--
consToPats :: Q [Pat]
consToPats = do
  info <- reify ''Struct
  case info of
    TyConI (DataD _ _ _ cons _) -> do
       pats <- mapM go cons
       return $ catMaybes pats
    _ -> return []
  where
    go :: Con -> Q (Maybe Pat)
    go (NormalC conName typs) = do
        pat <- conP conName (take (length typs) $ repeat wildP)
        return $ Just pat
    go _ = return Nothing

