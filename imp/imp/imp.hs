-- Notice: this follows the outline in the slides here:
-- http://community.haskell.org/~simonmar/slides/cadarache2012/5%20-%20server%20apps.pdf
-- JDH

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}

import Network
import Control.Monad
import Control.Exception.Base
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as Map
import System.IO
import Data.Time.Clock
import Text.Printf
import Data.Int

-- modules related to share arrays
import Data.Array
import Data.Ix

-- Our own class for allowed primitive element types
class Primitive
instance Primitive Float
instance Primitive Double
instance Primitive Int8
instance Primitive Int16
instance Primitive Int32
instance Primitive Int64

-- TODO: Need an existential type for all types of array I think -JDH
data ShareArray = forall i e. (Ix i, Primitive e) => ShareArray (TArray i e)

-- A share is just a ShareArray (array of some shape and element type), along with some metadata
-- TODO: make Share a recursively defined tree type.  That will allow the
-- "folder"-like view while maintaining easy subscription to complex shares
-- TODO: stuff like the Share types belongs in a library.  This file should
-- only contain server/client related stuff
data Share = Share { mTime :: UTCTime, payload :: ShareArray }

-- publisher account. holds info about the publisher, and a list of their shares
data PublisherAcct = PublisherAcct { shares :: Map.Map String Share }

type ClientName = String

-- info about connected clients
data Client = Client
    { clientHandle :: Handle
    , clientSubbedShares :: [Share]
    , clientSendChan :: TChan ServerMessage
    }

newClient :: Handle -> STM Client
newClient handle = do
    return Client { clientHandle=handle
        , clientSubbedShares=[]
        , clientSendChan=newTChan
        }

-- server just holds a bunch of accounts, indexed by user name
-- TODO: I think we'll put each level inside a tvar for more granular
-- atomicity-JDH
data Server = Server { publishers :: TVar (Map.Map String PublisherAcct)
                    , clients :: TVar (Map.Map ClientName Client) }

-- creating a new TVar requires us to be in IO, hence this helper fn
newServer :: IO Server
newServer = do
    p <- newTVarIO Map.empty
    return Server { publishers = p }

-- request and response types
data ClientRequest = GetShare String | ListShares

data ServerMessage = Notice String
    | Deliver Share
    | ServerError String

-- hardcoded port. TODO: yaml config files ala physicali
port :: Int
port = 4444

main :: IO ()
main = withSocketsDo $ do
    -- TOOD: Load config file
    -- TODO: Load server state from data directory
    server <- newServer
    sock <- listenOn $ PortNumber $ fromIntegral port
    printf "Listening on port %d\n" port
    forever $ do
        (handle, host, port) <- accept sock
        printf "Accepted connection from %s:%s\n" host (show port)
        -- finally ensures handle is closed no matter what happens in talk
        let name0 = host ++ ":" ++ show port ++ "_"
        forkIO $ talk server handle name0 `finally` hClose handle
        printf "Closed connection from %s:%s\n" host (show port)

-- straight from Simon Marlow's walkthrough
checkAddClient :: Server -> ClientName -> Handle -> IO (Maybe Client)
checkAddClient server@Server{..} name handle = atomically $ do
    clientmap <- readTVar clients
    if Map.member name clientmap
       then return Nothing
       else do
         client <- newClient name handle
         writeTVar clients (Map.insert name client clientmap)
         --TODO: Log this sort of stuff with an appropriate threadsafe
         -- logging facility
         --broadcast server $ Notice $ name ++ " has connected"
         return (Just client)

removeClient :: Server -> ClientName -> IO ()
removeClient server@Server{..} name = atomically $ do
    clientmap <- readTVar clients
    writeTVar clientmap (Map.delete name clientmap)
    --broadcast server (Notice (name ++ " has disconnected"))

runClient :: Server -> Client -> IO ()
runClient server@Server{..} client@Client{..}
 = concurrently send receive
 where
    send = join $ atomically $ do
        msg <- readTChan clientSendChan
        return $ do
            continue <- handleMessage server client msg
            when continue $ send
    receive = forever $ do
       msg <- hGetLine clientHandle
       atomically $ sendMessage client $ Command msg

-- The String here is the prefix of this client's name.  It's just
-- host:port_, to which we append an integer
talk :: Server -> Handle -> String -> IO ()
talk server@Server{..} handle name0 = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    newName 0
  where
    newName i = do
        let name = printf "%s%04d" name0 i
        m <- checkAddClient server name handle
        case m of
            Nothing -> do
                hPrintf handle "The name %s is in use" name
                newName $ i+1
            Just client -> do
                runClient server client
                    `finally` removeClient server name
