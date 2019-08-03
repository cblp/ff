{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Cpp
  ( MainWindow,
    ffCtx,
    includeDependent
    )
where

import qualified Data.Map.Strict as Map
import qualified FF.Storage as Storage
import Foreign.StablePtr (StablePtr)
import Language.C.Inline (include)
import Language.C.Inline.Context (Context, ctxTypesTable)
import Language.C.Types (TypeSpecifier (TypeName))
import Language.Haskell.TH (DecsQ)
import Language.Haskell.TH.Syntax (addDependentFile)

data MainWindow

ffCtx :: Context
ffCtx =
  mempty
    { ctxTypesTable = Map.fromList
                        [ (TypeName "bool", [t|Bool|]),
                          (TypeName "MainWindow", [t|MainWindow|]),
                          ( TypeName "StorageHandle",
                            [t|StablePtr Storage.Handle|]
                            )
                          ]
      }

includeDependent :: String -> DecsQ
includeDependent file = include file <* addDependentFile file
