{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where


    ------------------------------------------------------------------------------
import           Control.Category
import           Control.Monad
import           Control.Exception (SomeException, try)
import           Data.ByteString (ByteString)
import           Data.ByteString.UTF8 (fromString)
import           Data.Maybe
import           Data.Text as T
import           Prelude hiding ((.))
import           System.IO

import           Snap
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Config

import           Snap.Loader.Dynamic

import qualified Data.ByteString.Char8 as C
import           Control.Monad.Writer (runWriter)

import           Snap.Snaplet.Scatter
import           Snap.Snaplet.Scatter.Internal




----------------------------------------------------
----- App Stuff
----------------------------------------------------
data App = App
    { _scatter   :: Snaplet (Scatter App)
    }
makeLens ''App


routes :: [(ByteString, Handler App App ())]
routes  = [ ("", with scatter scatterServe)
          ]


app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    s <- nestSnaplet "scatter" scatter $ initScatter myURLs
    addRoutes routes
    return $ App s

main :: IO ()
main = serveSnaplet defaultConfig app
        
        
--------------

data Zing = Zing ByteString
    deriving (Show)
instance URLFragment Zing where
    urlize (Zing a) = a
    
data Zong = Zong ByteString
    deriving (Show)
instance URLFragment Zong where
    urlize (Zong a) = a


--- Ignore ScatterHandler Requirement for now. Will change Later
trialFunc :: Zong -> Zing -> ScatterHandler App ()
trialFunc a b = do
    writeBS $ "Current inputs: " `C.append` " " `C.append` (fromString $ show $  a) `C.append` " "`C.append` (fromString $ show $  b)
    writeBS $ "\nBuilt URL:  " `C.append` ((renderURL trial) (Zong "Zongy") (Zing "Zingy"))



--- This is our example URL
trial = ("/r" :: C.ByteString) :/: (Just . Zong) :/: (Just . Zing)

--- We build our URLs
myURLs :: ScatterBuilder (ScatterHandler App ()) ()
myURLs = do
    putURL trial trialFunc

         
    
