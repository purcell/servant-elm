{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Servant.Elm.HasCode where

import           Data.Proxy        (Proxy (Proxy))
import           GHC.TypeLits      (KnownSymbol, symbolVal)
import           Servant.API       ((:<|>) ((:<|>)), (:>))

import qualified Servant.API       as S

import           Servant.Elm.Types

class HasCode a where
    type Elm a
    codeFor :: Proxy a -> Endpoint -> Elm a

instance HasCode S.Raw where
    type Elm S.Raw = ()
    codeFor _ _ = ()

instance (HasCode a, HasCode b) => HasCode (a :<|> b) where
    type Elm (a :<|> b) = Elm a :<|> Elm b
    codeFor _ e = codeFor a e :<|> codeFor b e where
        a = Proxy :: Proxy a
        b = Proxy :: Proxy b

instance (HasCode c) => HasCode (S.ReqBody a b :> c) where
    type Elm (S.ReqBody a b :> c) = Elm c
    codeFor _ e = codeFor c (e { endpointHasBody = True}) where
        c = Proxy :: Proxy c

-- Methods

instance HasCode (S.Delete a b) where
    type Elm (S.Delete a b) = Endpoint
    codeFor _ e = e { endpointMethod = Delete }

instance HasCode (S.Get a b) where
    type Elm (S.Get a b) = Endpoint
    codeFor _ e = e { endpointMethod = Get }

instance HasCode (S.Patch a b) where
    type Elm (S.Patch a b) = Endpoint
    codeFor _ e = e { endpointMethod = Patch }

instance HasCode (S.Post a b) where
    type Elm (S.Post a b) = Endpoint
    codeFor _ e = e { endpointMethod = Post }

instance HasCode (S.Put a b) where
    type Elm (S.Put a b) = Endpoint
    codeFor _ e = e { endpointMethod = Put }

-- Path segments

instance (KnownSymbol s, HasCode a) => HasCode (s :> a) where
    type Elm (s :> a) = Elm a
    codeFor _ e = codeFor a (e { endpointPathSegments = segments }) where
        a = Proxy :: Proxy a
        segments = endpointPathSegments e ++ [segment]
        segment = PathLiteral (symbolVal s)
        s = Proxy :: Proxy s

instance (KnownSymbol s, HasCode b) => HasCode (S.Capture s a :> b) where
    type Elm (S.Capture s a :> b) = Elm b
    codeFor _ e = codeFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathCapture (symbolVal s)
        s = Proxy :: Proxy s

-- Matrix items

instance (KnownSymbol s, HasCode a) => HasCode (S.MatrixFlag s :> a) where
    type Elm (S.MatrixFlag s :> a) = Elm a
    codeFor _ e = codeFor a (e { endpointPathSegments = segments }) where
        a = Proxy :: Proxy a
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixFlag (symbolVal s))
        s = Proxy :: Proxy s

instance (KnownSymbol s, HasCode b) => HasCode (S.MatrixParam s a :> b) where
    type Elm (S.MatrixParam s a :> b) = Elm b
    codeFor _ e = codeFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixParam (symbolVal s))
        s = Proxy :: Proxy s

instance (KnownSymbol s, HasCode b) => HasCode (S.MatrixParams s a :> b) where
    type Elm (S.MatrixParams s a :> b) = Elm b
    codeFor _ e = codeFor b (e { endpointPathSegments = segments }) where
        b = Proxy :: Proxy b
        segments = endpointPathSegments e ++ [segment]
        segment = PathMatrix (MatrixParams (symbolVal s))
        s = Proxy :: Proxy s

-- Query items

instance (KnownSymbol s, HasCode a) => HasCode (S.QueryFlag s :> a) where
    type Elm (S.QueryFlag s :> a) = Elm a
    codeFor _ e = codeFor a (e { endpointQueryItems = items }) where
        a = Proxy :: Proxy a
        items = endpointQueryItems e ++ [item]
        item = QueryFlag (symbolVal s)
        s = Proxy :: Proxy s

instance (KnownSymbol s, HasCode b) => HasCode (S.QueryParam s a :> b) where
    type Elm (S.QueryParam s a :> b) = Elm b
    codeFor _ e = codeFor b (e { endpointQueryItems = items }) where
        b = Proxy :: Proxy b
        items = endpointQueryItems e ++ [item]
        item = QueryParam (symbolVal s)
        s = Proxy :: Proxy s

instance (KnownSymbol s, HasCode b) => HasCode (S.QueryParams s a :> b) where
    type Elm (S.QueryParams s a :> b) = Elm b
    codeFor _ e = codeFor b (e { endpointQueryItems = items }) where
        b = Proxy :: Proxy b
        items = endpointQueryItems e ++ [item]
        item = QueryParams (symbolVal s)
        s = Proxy :: Proxy s

-- Headers

instance (KnownSymbol s, HasCode b) => HasCode (S.Header s a :> b) where
    type Elm (S.Header s a :> b) = Elm b
    codeFor  _ e = codeFor b (e { endpointHeaders = headers }) where
        b = Proxy :: Proxy b
        headers = endpointHeaders e ++ [header]
        header = Header (symbolVal s)
        s = Proxy :: Proxy s
