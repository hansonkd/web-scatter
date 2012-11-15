{-# LANGUAGE OverloadedStrings   #-}

module Snap.Snaplet.Scatter where

import           Control.Monad.Writer
import           Data.ByteString

import           Snap
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Scatter.Internal

-- | Snaplet initialization
initScatter :: (ScatterBuilder (ScatterHandler b ()) ()) -> SnapletInit b (Scatter b)
initScatter urls = makeSnaplet "scatter" "Type safe URLs in Snap" Nothing $ do
  return $ Scatter $ (snd . runWriter) urls

filterThrough :: [(ByteString -> Maybe (Handler b (Scatter b) ()))] -> Handler b (Scatter b) ()
filterThrough (cur:[]) = do
    url <- getRequest
    case cur $ rqURI url of
        Nothing -> writeBS $ rqURI url -- "No paths matched 404"
        Just h -> h
filterThrough (cur:rest) = do
    url <- getRequest
    case cur $ rqURI url of
        Nothing -> filterThrough rest
        Just h -> h

scatterServe :: Handler b (Scatter b) ()
scatterServe = do
    Scatter paths <- get
    filterThrough paths
    
    