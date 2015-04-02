{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type, as well
as all the things that need to happen in the background to get a working
Kademlia instance.
-}

module Network.Kademlia.Instance
    ( KademliaInstance(..)
    , start
    ) where

import Control.Concurrent
import Control.Monad (void, forever)
import Control.Monad.Trans.State
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (catchIOError)

import Network.Kademlia.Networking
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types

-- | The handle of a running Kademlia Node
data KademliaInstance i a = KI {
      handle :: KademliaHandle i a
    , tree   :: T.NodeTree i
    }

-- | Start the background process for a KademliaInstance
start :: (Serialize i, Ord i, Serialize a) =>
    KademliaInstance i a -> IO ()
start inst = void $ forkIO $
        -- There is a very high chance that there will arise an Exception,
        -- caused by closing the KademliaHandle in another thread.
        -- This acts as the stopping signal for the background process.
        backgroundProcess inst `catchIOError` \e -> return ()


-- | The actual process running in the background
backgroundProcess :: (Serialize i, Ord i, Serialize a) =>
    KademliaInstance i a -> IO ()
backgroundProcess inst = forever . flip execStateT inst  $ do
    -- Receive the next signal
    h <- gets handle
    sig <- liftIO . recv $ h

    -- Insert the node into the tree, if it's allready known, it will be
    -- refreshed
    let node = source sig
    modify $ \inst -> inst { tree = T.insert (tree inst) node }

    -- Handle the signal
    handleSignal sig

-- | Handles the different Kademlia Signals appropriately
handleSignal :: (Serialize i, Serialize a) =>
    Signal i a -> StateT (KademliaInstance i a) IO ()
handleSignal _ = return ()
