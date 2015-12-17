module Servant.Elm.Types where

newtype Header = Header String
    deriving (Eq, Ord, Read, Show)

data MatrixItem
    = MatrixFlag String
    | MatrixParam String
    | MatrixParams String
    deriving (Eq, Ord, Read, Show)

data Method
    = Delete
    | Get
    | Patch
    | Post
    | Put
   deriving (Bounded, Enum, Eq, Ord, Read, Show)

data QueryItem
    = QueryFlag String
    | QueryParam String
    | QueryParams String
    deriving (Eq, Ord, Read, Show)

data Endpoint = Endpoint
    { endpointMethod       :: Method
    , endpointPathSegments :: [PathSegment]
    , endpointQueryItems   :: [QueryItem]
    , endpointHeaders      :: [Header]
    , endpointHasBody      :: Bool
    } deriving (Eq, Ord, Read, Show)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint
    { endpointMethod = Get
    , endpointPathSegments = []
    , endpointQueryItems = []
    , endpointHeaders = []
    , endpointHasBody = False
    }

data PathSegment
    = PathLiteral String
    | PathCapture String
    | PathMatrix MatrixItem
    deriving (Eq, Ord, Read, Show)

isPathMatrix :: PathSegment -> Bool
isPathMatrix (PathMatrix _) = True
isPathMatrix _ = False
