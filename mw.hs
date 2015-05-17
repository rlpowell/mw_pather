module Main where
import Haste
import Haste.DOM
import Haste.Events
import Control.Monad.IO.Class
import Data.List

type LocationName = String

-- data Location = Location {
--     name :: LocationName
--   , connections :: [Connection]
-- } deriving (Show, Eq)

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
  deriving (Show, Eq)

connections = [
    Connection { origin="Balmora", destination="Vivec", ctype=Mage }
  , Connection { origin="Balmora", destination="Sadrith Mora", ctype=Mage }
  , Connection { origin="Sadrith Mora", destination="Vivec", ctype=Mage }
  , Connection { origin="Sadrith Mora", destination="Balmora", ctype=Mage }
  , Connection { origin="Sadrith Mora", destination="Tel Mora", ctype=Boat }
  , Connection { origin="Vivec", destination="Balmora", ctype=Mage }
  ]

locations = [
    "Balmora"
  , "Sadrith Mora"
  , "Tel Mora"
  , "Vivec"
  ]

-- locations = [
--     Location { name="Balmora", connections=[
--           Connection { destination="Vivec", ctype=Mage }
--         , Connection { destination="Sadrith Mora", ctype=Mage }
--     ] }
--   , Location { name="Sadrith Mora", connections=[
--           Connection { destination="Vivec", ctype=Mage }
--         , Connection { destination="Balmora", ctype=Mage }
--         , Connection { destination="Tel Mora", ctype=Boat }
--     ] }
--   , Location { name="Tel Mora", connections=[
--     ] }
--   , Location { name="Vivec", connections=[ Connection { destination="Balmora", ctype=Mage } ] }
--   ]

makeOption :: String -> IO Elem
makeOption opt = do
  elem <- newElem "option"
  set elem [attr "value" =: opt]
  inner <- newTextElem opt
  addChild inner elem
  return elem

makeLocationElems = mapM makeOption locations

addChildren :: Elem -> [Elem] -> IO ()
addChildren parent children = sequence_ [addChild c parent | c <- children]

findMatchingConn :: LocationName -> LocationName -> [Connection] -> Maybe Connection
findMatchingConn start end conns = find (\x -> ((origin x) == start) && ((destination x) == end)) conns

findConnsByOrigin :: LocationName -> [Connection] -> [Connection]
findConnsByOrigin wanted conns = filter (\x -> (origin x) == wanted) conns

findPath :: LocationName -> LocationName -> Integer -> [Connection] -> [Connection]
findPath originName destName depth path =
    if depth > 10 then
      []
    else
      case findMatchingConn originName destName connections of
        Just conn -> path ++ [conn]
        Nothing -> case find (/= []) (map (\x -> findPath (destination x) destName (depth + 1) (path ++ [x])) (findConnsByOrigin originName connections)) of
          Nothing -> []
          Just found -> found

handleFind :: Elem -> Elem -> Elem -> EventData BasicEvent -> IO ()
handleFind start end result event = do
    clearChildren result
    maybeStart <- getValue start
    maybeEnd <- getValue end
    case (maybeStart, maybeEnd) of
      (Nothing, _) -> error "Couldn't get value for starting point."
      (_, Nothing) -> error "Couldn't get value for ending point."
      (Just startName, Just endName) -> do
        textbit <- newTextElem (show $ findPath startName endName 0 [])
        addChild textbit result
        preventDefault

handleSelection :: Elem -> Elem -> Elem -> IO ()
handleSelection start end result = do
  onEvent start Change $ handleFind start end result
  onEvent end Change $ handleFind start end result
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
    
    

{-
main = withElems ["a","b","op","result"] calculator

calculator [a,b,op,result] = do
    onEvent a  KeyUp $ \_ -> recalculate
    onEvent b  KeyUp $ \_ -> recalculate
    onEvent op Change $ \_ -> recalculate
  where
    recalculate = do
      ma <- getValue a
      mb <- getValue b
      Just op' <- getValue op
      case (ma, mb) of
        (Just a', Just b') -> setProp result "innerHTML" (toString $ calc op' a' b')
        _                  -> return ()

    calc "+" = (+)
    calc "-" = (-)
    calc "*" = (*)
    calc "/" = (/)
    calc _   = \_ _ -> 0 :: Double

main :: IO ()
main = do
  ul <- elemsByQS document "ul#demo-list"
  case ul of
    (el:_) -> mapQS_ document "#demo-list li" (handleRemove el)
    _      -> error "Element 'ul#demo-list' not found"

handleRemove :: Elem -> Elem -> IO HandlerInfo
handleRemove ul li = do
  onEvent li Click $ \_ -> do
    removeChild li ul
    preventDefault
-}
