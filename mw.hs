module Main where
import Haste
import Haste.DOM
import Haste.Events
import Data.List
import Text.Printf
import Debug.Trace

type LocationName = String

data Connection = Connection {
    origin :: LocationName
  , destination :: LocationName
  , ctype :: ConnectionType
} deriving (Show, Eq)

data ConnectionType =
    Mage
  | Silt
  | Almsivi
  | Divine
  | Index
  | Boat
  | None
  deriving (Show, Eq)

data Path = Path {
    pconns :: [Connection]
  , pdest :: LocationName
} deriving (Show, Eq)

reducePaths :: [LocationName] -> [Path] -> [Path]
reducePaths dests (path:paths) =
  if (pdest path) `elem` dests then
    [] ++ reducePaths dests paths
  else
    [path] ++ reducePaths (dests ++ [(pdest path)]) paths
reducePaths _ [] = []

expandPaths :: [Path] -> [Path]
expandPaths paths =
  paths ++ concatMap addPaths paths

addPaths :: Path -> [Path]
addPaths start =
  map (extendMatchingPath start) connections

extendMatchingPath :: Path -> Connection -> Path
extendMatchingPath path conn =
  if (pdest path) == (origin conn) then
    Path { pconns=((pconns path) ++ [conn]), pdest=(destination conn) }
  else
    path

findPath :: LocationName -> LocationName -> [Path] -> Path
findPath start end paths = trace (printf "In findPath: start: %s, end: %s, paths: %s" start end (show paths)) $
  case maybePath of
    Nothing ->
      -- If we didn't add any new destinations, we've exhausted the
      -- search tree; give up
      if paths == newPaths then
        error $ printf "No path to %s found!" end
      else
        findPath start end newPaths
    Just path -> path
  where maybePath = find (\x -> (pdest x) == end) paths
        -- We inject the start path, even if we have other paths,
        -- every time.  This way we don't have to make any
        -- decisions, and it's pretty cheap.
        newPaths = reducePaths [] $ expandPaths $ Path { pdest=start, pconns=[Connection { origin=start, destination=start, ctype=None }] } : paths

connections :: [Connection]
connections = [
    Connection { origin="Balmora", destination="Vivec", ctype=Mage }
  , Connection { origin="Balmora", destination="Sadrith Mora", ctype=Mage }
  , Connection { origin="Sadrith Mora", destination="Vivec", ctype=Mage }
  , Connection { origin="Sadrith Mora", destination="Balmora", ctype=Mage }
  , Connection { origin="Sadrith Mora", destination="Tel Mora", ctype=Boat }
  , Connection { origin="Vivec", destination="Balmora", ctype=Mage }
  ]

locations :: [LocationName]
locations = [
    "Balmora"
  , "Sadrith Mora"
  , "Tel Mora"
  , "Vivec"
  ]

makeOption :: String -> IO Elem
makeOption opt = do
  optElem <- newElem "option"
  set optElem [attr "value" =: opt]
  inner <- newTextElem opt
  addChild inner optElem
  return optElem

makeLocationElems :: IO [Elem]
makeLocationElems = mapM makeOption locations

addChildren :: Elem -> [Elem] -> IO ()
addChildren parent childs = sequence_ [addChild c parent | c <- childs]

handleFind :: Elem -> Elem -> Elem -> EventData BasicEvent -> IO ()
handleFind start end result _ = do
    clearChildren result
    maybeStart <- getValue start
    maybeEnd <- getValue end
    case (maybeStart, maybeEnd) of
      (Nothing, _) -> error "Couldn't get value for starting point."
      (_, Nothing) -> error "Couldn't get value for ending point."
      (Just startName, Just endName) -> do
        textbit <- newTextElem (show $ findPath startName endName [])
        addChild textbit result
        preventDefault

handleSelection :: Elem -> Elem -> Elem -> IO ()
handleSelection start end result = do
  _ <- onEvent start Change $ handleFind start end result
  _ <- onEvent end Change $ handleFind start end result
  return ()

main :: IO ()
main = do
    maybeStart <- elemById "start"
    maybeEnd <- elemById "end"
    maybeResult <- elemById "result"
    startLocationElems <- makeLocationElems
    endLocationElems <- makeLocationElems
    case (maybeStart, maybeEnd, maybeResult) of
      (Nothing, _, _) -> error "Start dropdown not found"
      (_, Nothing, _) -> error "End dropdown not found"
      (_, _, Nothing) -> error "Result element not found"
      (Just start, Just end, Just result) -> do 
          addChildren start startLocationElems
          addChildren end endLocationElems
          handleSelection start end result
          return ()
