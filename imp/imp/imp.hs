{-# LANGUAGE ExistentialQuantification #-}
-- Notice: this follows the outline in the slides here:
-- http://community.haskell.org/~simonmar/slides/cadarache2012/5%20-%20server%20apps.pdf
-- JDH

import Network
import Control.Monad
import Control.Exception.Base
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as Map
import System.IO
import Data.Time.Clock
import Text.Printf

-- Our own class for allowed primitive types
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
data Share = Share { mTime :: UTCTime, payload :: ShareArray }

-- publisher account. holds info about the publisher, and a list of their shares
data PublisherAcct = PublisherAcct { shares :: Map.Map String Share }

-- server just holds a bunch of accounts, indexed by user name
-- TODO: I think we'll put each level inside a tvar for more granular
-- atomicity-JDH
data Server = Server { publishers :: TVar (Map.Map String PublisherAcct) }

-- creating a new TVar requires us to be in IO, hence this helper fn
newServer :: IO Server
newServer = do
    p <- newTVarIO Map.empty
    return Server { publishers = p }

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
        forkIO $ talk server handle `finally` hClose handle

-- the Marlow slides give the following as a definition of forkFinally
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then = 
    mask $ \restore ->
        forkIO $ try (restore action) >>= and_then
-- I think this is a more general pattern that exists already, like we
-- could use forkIOWithUnmask or bracket or something, but I am unfamiliar
-- here so I'm just blindly using this until I understand it better -JDH
