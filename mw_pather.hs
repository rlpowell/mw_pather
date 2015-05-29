{-# LANGUAGE CPP #-}
module Main where
import Haste.App
import Haste.DOM
import Haste.Events
import Data.List
import Control.Applicative
import Text.Printf
import Debug.Trace
import Text.Regex.PCRE
import qualified Data.Set as Set
import Debug.Trace
import Data.IORef
#ifndef __HASTE__
import Data.String.Utils
import Network.HTTP hiding (Connection)
import Text.HTML.TagSoup
#endif

data API = API {
    apiGetConns :: Remote (Server [[String]])
  }

data TraceLevel = Crazy | Debug | Info | Error deriving (Show, Eq, Ord)
traceLevel = Crazy
rlpTraceM :: (Monad m) => TraceLevel -> String -> m ()
rlpTraceM level msg
  | level >= traceLevel = traceM msg
  | otherwise          = return ()

data Path = Path {
    pconns :: [Connection]
  , pdest :: LocationName
} deriving (Show, Eq)

type LocationName = String

type ConnectionType = String

data Connection = Connection {
    origin :: LocationName
  , destination :: LocationName
  , ctype :: ConnectionType
} deriving (Eq, Ord)

-- Saves us having to write a Binary instance for Connection
connToList :: Connection -> [String]
connToList conn = [
    (origin conn), (destination conn), (ctype conn)
  ]

-- Saves us having to write a Binary instance for Connection
listToConn :: [String] -> Connection
listToConn [orig, dest, ctypeStr] = Connection { origin=orig, destination=dest, ctype=ctypeStr }

fixLocName :: LocationName -> LocationName
fixLocName "" = "Balmora"
fixLocName "Vivec" = "Vivec (city)"
fixLocName "Fort Darius" = "Gnisis"
fixLocName "Fort Pelagiad" = "Pelagiad"
fixLocName "Moonmoth Legion fort" = "Moonmoth Legion Fort"
fixLocName "Ald'Ruhn" = "Ald'ruhn"
fixLocName lname = lname

fixPageForWeb :: LocationName -> String
fixPageForWeb pname =
#ifdef __HASTE__
  pname
#else
  replace " " "_" pname
#endif

-- Example of what we're parsing here:
--
-- <td style="text-align:left;"><b>Transport:</b><br />
-- <p><a href="/wiki/Morrowind:Almsivi_Intervention" title="Morrowind:Almsivi Intervention">Almsivi Intervention</a>:<br /></p>
-- <ul>
-- <li><a href="/wiki/Morrowind:Ald%27ruhn" title="Morrowind:Ald'ruhn">Ald'ruhn</a></li>
-- </ul>
-- <p><a href="/wiki/Morrowind:Divine_Intervention" title="Morrowind:Divine Intervention">Divine Intervention</a>:<br /></p>
-- <ul>
-- <li><a href="/wiki/Morrowind:Wolverine_Hall" title="Morrowind:Wolverine Hall">Wolverine Hall</a><br /></li>
-- </ul>
-- <p><a href="/wiki/Morrowind:Transport#Boat" title="Morrowind:Transport">Boat</a>:<br /></p>
-- <ul>
-- <li><a href="/wiki/Morrowind:Dagon_Fel" title="Morrowind:Dagon Fel">Dagon Fel</a></li>
-- <li><a href="/wiki/Morrowind:Sadrith_Mora" title="Morrowind:Sadrith Mora">Sadrith Mora</a></li>
-- <li><a href="/wiki/Morrowind:Tel_Aruhn" title="Morrowind:Tel Aruhn">Tel Aruhn</a></li>
-- <li><a href="/wiki/Morrowind:Vos" title="Morrowind:Vos">Vos</a></li>
-- </ul>
-- </td>

pageToConnections :: LocationName -> IO [Connection]
pageToConnections originPage = do
#ifdef __HASTE__
    return [ Connection { origin="Balmora", destination="Vivec", ctype="Guild Guide" } ]
#else
    let openURL x = getResponseBody =<< simpleHTTP (getRequest x)
    tags <- fmap parseTags $ openURL (printf "http://www.uesp.net/wiki/Morrowind:%s" $ fixPageForWeb originPage)
    -- Get a list of all the transports
    let transports1 = filter (\x -> Data.List.isInfixOf "Transport:" (innerText x)) $ partitions (~== "<td>") tags
    let transports = if (length transports1) > 0
        then partitions (~== "<p>") $ head transports1
        else []
    return $ concat $ map transToConns transports
      where
        transToConns x = map destToConn tDests
          where
            -- Get the transport's name
            tName1 = partitions (~== "<a>") x
            tName = if (length tName1) > 0
                then innerText $ takeWhile (not . isTagClose) $ head tName1
                else "None"
            -- Get the transport's destinations
            tDests = filter (/= "") $ lines $ replace "'''" "" $ replace " (split)" "" $ replace "/" "\n" $ innerText $ concat $ partitions (~== "<li>") $ concat $ partitions (~== "<ul>") x
            destToConn dest = Connection { origin=(fixLocName originPage), destination=(fixLocName dest), ctype=tName }
            -- trace (printf "In destToConn: %s, %s, %s" originPage dest tName) $ 
#endif

-- await :: Server State -> Server Message
-- await state = do
--   sid <- getSessionID
--   (clients, _) <- state
--   liftIO $ readIORef clients >>= maybe (return ("","")) C.takeMVar . lookup sid
getConns :: Server (IORef [[String]]) -> Server [[String]]
getConns remoteConnsIORef = do
  remoteConnsRef <- remoteConnsIORef
  remoteConns <- liftIO $ readIORef remoteConnsRef
  if length remoteConns > 1 then do
    return remoteConns
  else do
    conns <- liftIO $ makeConnections Set.empty Set.empty $ Set.fromList ["Ald'ruhn","Balmora","Ebonheart","Sadrith Mora","Vivec","Caldera","Gnisis","Maar Gan","Molag Mar","Pelagiad","Suran","Tel Mora","Ald Velothi","Dagon Fel","Gnaar Mok","Hla Oad","Khuul","Tel Aruhn","Tel Branora","Seyda Neen","Vos","Tel Fyr","Tel Vos","Buckmoth Legion Fort","Moonmoth Legion Fort","Wolverine Hall","Ahemmusa Camp","Erabenimsun Camp","Urshilaku Camp","Zainab Camp","Indarys Manor","Rethan Manor","Tel Uvirith"]
    _ <- liftIO $ writeIORef remoteConnsRef $ map connToList conns
    newConns <- liftIO $ readIORef remoteConnsRef
    return newConns

-- Make: (connections, done, to-do)
-- Repeatedly take the first to-do item, add all its connections, stick it in done, stick anything that comes up that isn't already in done or to-do in to-do
-- when to-do is empty, return connections
makeConnections :: Set.Set Connection -> Set.Set LocationName -> Set.Set LocationName -> IO [Connection]
makeConnections connections doneLocs toDoLocs
  | Set.null toDoLocs = do
      _ <- rlpTraceM Crazy (printf "makeConnections is done: %s" $ show $ Set.toList connections)
      return $ Set.toList connections
  | otherwise = do
      _ <- rlpTraceM Crazy (printf "makeConnections: %s -- %s -- %s -- %s" (show $ Set.toList connections) (show $ Set.toList doneLocs) (show $ Set.toList toDoLocs) (show $ Set.findMin toDoLocs))
      let nextLoc = fixLocName $ Set.findMin toDoLocs
      let toDoRemainder = Set.deleteMin toDoLocs
      newConns <- pageToConnections nextLoc
      let newDoneLocs = Set.insert nextLoc doneLocs
      -- the todo list becomes everything left in the todo list
      -- plus all the new destinations minus all the locations
      -- we've done before
      let newToDo = (Set.difference (Set.union toDoRemainder $ Set.fromList $ map fixLocName $ map destination newConns) newDoneLocs)
      makeConnections
                (Set.union connections $ Set.fromList newConns)
                newDoneLocs
                newToDo

instance Show Connection where
  show (Connection _ sdest "None") = printf "You start in %s." sdest
  show (Connection sorigin sdestination sctype) = printf "Go from %s to %s by %s." sorigin sdestination sctype
--  show (Connection sorigin sdestination sctype) = printf "/  %s--%s--%s  /" sorigin sdestination sctype

reducePaths :: [LocationName] -> [Path] -> [Path]
reducePaths dests (path:paths) =
  if (pdest path) `elem` dests then
    [] ++ reducePaths dests paths
  else
    [path] ++ reducePaths (dests ++ [(pdest path)]) paths
reducePaths _ [] = []

expandPaths :: [Connection] -> [Path] -> [Path]
expandPaths connections paths =
  paths ++ concatMap (addPaths connections) paths

addPaths :: [Connection] -> Path -> [Path]
addPaths connections start =
  map (extendMatchingPath start) connections

extendMatchingPath :: Path -> Connection -> Path
extendMatchingPath path conn =
  if (pdest path) == (origin conn) then
    Path { pconns=((pconns path) ++ [conn]), pdest=(destination conn) }
  else
    path

findPath :: [Connection] -> LocationName -> LocationName -> [Path] -> Path
findPath connections start end paths = trace (printf "In findPath: start: %s, end: %s, paths: %s" start end (show paths)) $
  case maybePath of
    Nothing ->
      -- If we didn't add any new destinations, we've exhausted the
      -- search tree; give up
      if paths == newPaths then
        error $ printf "No path to %s found!" end
      else
        findPath connections start end newPaths
    Just path -> path
  where maybePath = find (\x -> (pdest x) == end) paths
        -- We inject the start path, even if we have other paths,
        -- every time.  This way we don't have to make any
        -- decisions, and it's pretty cheap.
        newPaths = reducePaths [] $ expandPaths connections $ Path { pdest=start, pconns=[Connection { origin=start, destination=start, ctype="None" }] } : paths

-- Old stuff, here for reference as to what the structures look
-- like.
--
-- connections :: [Connection]
-- connections = [
--     Connection { origin="Balmora", destination="Vivec", ctype="Guild Guide" }
--   , Connection { origin="Balmora", destination="Sadrith Mora", ctype="Guild Guide" }
--   , Connection { origin="Sadrith Mora", destination="Vivec", ctype="Guild Guide" }
--   , Connection { origin="Sadrith Mora", destination="Balmora", ctype="Guild Guide" }
--   , Connection { origin="Sadrith Mora", destination="Tel Mora", ctype="Boat" }
--   , Connection { origin="Vivec", destination="Balmora", ctype="Guild Guide" }
--   ]
-- 
-- locations :: [LocationName]
-- locations = [
--     "Balmora"
--   , "Sadrith Mora"
--   , "Tel Mora"
--   , "Vivec"
--   ]

makeOption :: String -> IO Elem
makeOption opt = do
  optElem <- newElem "option"
  set optElem [attr "value" =: opt]
  inner <- newTextElem opt
  addChild inner optElem
  return optElem

makeLocationElems :: [LocationName] -> IO [Elem]
makeLocationElems locations = mapM makeOption locations

addChildren :: Elem -> [Elem] -> IO ()
addChildren parent childs = sequence_ [addChild c parent | c <- childs]

makeP :: String -> IO Elem
makeP string = do
  p <- newElem "p"
  text <- newTextElem string
  setChildren p [text]
  return p

rowsHtmlShow :: [Connection] -> IO [Elem]
rowsHtmlShow sconns = mapM (makeP . show) sconns

handleFind :: [Connection] -> Elem -> Elem -> Elem -> EventData BasicEvent -> IO ()
handleFind connections start end result _ = do
    clearChildren result
    maybeStart <- getValue start
    maybeEnd <- getValue end
    case (maybeStart, maybeEnd) of
      (Nothing, _) -> error "Couldn't get value for starting point."
      (_, Nothing) -> error "Couldn't get value for ending point."
      (Just startName, Just endName) -> do
        pathPrints <- rowsHtmlShow $ pconns $ findPath connections startName endName []
        addChildren result pathPrints
        preventDefault

handleSelection :: [Connection] -> Elem -> Elem -> Elem -> IO ()
handleSelection connections start end result = do
  _ <- onEvent start Change $ handleFind connections start end result
  _ <- onEvent end Change $ handleFind connections start end result
  return ()

clientMain :: API -> Client ()
clientMain api = do
    connectionStr <- onServer $ apiGetConns api
    let connections = map listToConn connectionStr
    let locations = sort $ nub $ map destination connections

    maybeStart <- elemById "start"
    maybeEnd <- elemById "end"
    maybeResult <- elemById "result"
    startLocationElems <- liftIO $ makeLocationElems locations
    endLocationElems <- liftIO $ makeLocationElems locations
    case (maybeStart, maybeEnd, maybeResult) of
      (Nothing, _, _) -> error "Start dropdown not found"
      (_, Nothing, _) -> error "End dropdown not found"
      (_, _, Nothing) -> error "Result element not found"
      (Just start, Just end, Just result) -> liftIO $ do 
          addChildren start startLocationElems
          addChildren end endLocationElems
          handleSelection connections start end result
          return ()

main :: IO ()
main = do
  runApp (mkConfig "vrici.lojban.org" 24601) $ do
    remoteConns <- liftServerIO $ newIORef [["nothing"]]

    -- Create an API object holding all available functions
    api <- API <$> remote (getConns remoteConns)

    runClient $ clientMain api
