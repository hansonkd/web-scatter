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

data Zang = Zang ByteString
    deriving (Show)
instance URLFragment Zang where
    urlize (Zang a) = a
    
data Zung = Zun | Zug
    deriving (Show)
instance URLFragment Zung where
    urlize Zun = "zun"
    urlize Zug = "zug"

makeZung :: ByteString -> Maybe Zung
makeZung a | a == "zun" = Just Zun
           | a == "zug" = Just Zug
           | otherwise = Nothing
    

--- Ignore ScatterHandler Requirement for now. Will change Later
trialFunc :: Zong -> Zing -> ScatterHandler App ()
trialFunc a b = do
    writeBS $ "Current inputs: " `C.append` " " `C.append` (fromString $ show $  a) `C.append` " "`C.append` (fromString $ show $  b)
    writeBS $ "\nBuilt URL:  " `C.append` ((renderURL trial) (Zong "Zongy") (Zing "Zingy"))

trialFunc2 :: Zang -> ScatterHandler App ()
trialFunc2 (Zang a) = do
    writeBS $ a

patMatch :: Zung -> ScatterHandler App ()
patMatch Zun = writeBS "You put in zun"
patMatch Zug = writeBS "You put in zug"


--- This is our example URL
trial = ("/r" :: C.ByteString) :/: (Just . Zong) :/: (Just . Zing)
paturl = ("/rc" :: C.ByteString) :/: (makeZung)
trial2 = ("/rc" :: C.ByteString) :/: (Just . Zang)

--- We build our URLs
myURLs :: ScatterBuilder (ScatterHandler App ()) ()
myURLs = do
    putURL trial trialFunc
    putURL paturl patMatch -- If url is /rc/zun or /rc/zug will match here
    putURL trial2 trialFunc2 -- else it will match here.
         
    
