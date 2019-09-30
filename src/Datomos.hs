{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Datomos where

import           Data.Char
import           Data.Text (Text())
import qualified Data.Text as T

data Term where
  Var :: Text -> Term
  Sym :: Text -> Term
  deriving (Eq, Show)

var :: Text -> Either SyntaxError Term
var x
  | T.null x           = Left EmptyVariableName
  | isLower $ T.head x =
    Left $ InvalidVariableName "Variable names must begin with an uppercase letter"
  | otherwise          = Right $ Var x

sym :: Text -> Either SyntaxError Term
sym x
  | T.null x           = Left EmptySymbolName
  | isUpper $ T.head x =
    Left $ InvalidSymbolName "Symbol names must begin with a lowercase letter"
  | otherwise          = Right $ Sym x

data Atom
  = Atom
  { _atomName :: Text
  , _atomArgs :: [Term]
  }
  deriving (Eq, Show)

atom :: Text -> [Term] -> Either SyntaxError Atom
atom name args
  | T.null name           = Left EmptyAtomName
  | null args             = Left EmptyAtomArgs
  | isUpper $ T.head name =
    Left $ InvalidAtomName "Atom names must be lowercase"
  | otherwise             = Right $ Atom name args

data Rule
  = Rule
  { _ruleHead :: Atom
  , _ruleBody :: [Atom]
  }
  deriving (Eq, Show)

data SyntaxError
  = EmptyVariableName
  | EmptySymbolName
  | InvalidVariableName Text
  | InvalidSymbolName Text
  | EmptyAtomName
  | EmptyAtomArgs
  | InvalidAtomName Text
  deriving (Eq, Show)
