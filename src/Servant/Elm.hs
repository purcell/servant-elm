{-# LANGUAGE FlexibleContexts #-}

module Servant.Elm
       ( elmForAPI
       ) where

import           Data.Proxy          (Proxy)
import           Servant.Elm.HasCode
import           Servant.Elm.ToElm
import           Servant.Elm.Types

elmForAPI :: (HasCode a, ToElm (Elm a)) => Proxy a -> String
elmForAPI proxy = toElm (codeFor proxy defaultEndpoint)



