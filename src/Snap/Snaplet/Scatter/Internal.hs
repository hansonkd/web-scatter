{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Scatter.Internal where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad.Writer
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.UTF8 (fromString, ByteString)
import           Data.ByteString (append)
import           Data.List.Split (splitOn)
import           Data.String.Utils (startswith, join)
import           Text.Regex.Posix
import           Snap
import           System.IO.Unsafe (unsafePerformIO)


data Scatter a = Scatter [(ByteString -> Maybe (Handler a (Scatter a) ()))]
type ScatterHandler a = Handler a (Scatter a)
type ScatterBuilder a b = Writer [(ByteString -> Maybe a)] b

---------------------------------------
--- DSL Operator to Build Values
---------------------------------------
infixr 9 :/:
data a :/: b = a :/: !b
  deriving (Show, Eq, Ord)
 
  
----------------------------------------------------------
--- URL Piece to Mutate our function into something useful
----------------------------------------------------------
{-
    * urlConstruct -- Makes a string to parse into a url
    
    * regexize -- creates the regex formula to match against incoming urls
    
    * flatten -- removes all the ByteStrings in it to create a new type
                 that is a tuple chain of constructor values.
-}


class URLPiece a b | a -> b where
    urlConstruct :: a -> ByteString
    regexize :: a -> ByteString
    flatten :: a -> b
    
instance (() ~ r) => URLPiece (ByteString :/: ByteString) r where
    urlConstruct (a :/: b) = a `append` (fromString "/") `append` b
    regexize (a :/: b) = a `append` (fromString "/") `append` b
    flatten _ = ()
instance (URLPiece a r) => URLPiece (ByteString :/: a) r where
    urlConstruct (a :/: rest) = a `append` (fromString "/") `append` (urlConstruct rest)
    regexize (a :/: rest) = a `append` (fromString "/") `append` (regexize rest)
    flatten (_ :/: rest) = flatten rest
instance (a ~ r) => URLPiece (a :/: ByteString) r where
    urlConstruct (x :/: b) = (urlConstruct x) `append` (fromString "/") `append` b
    regexize (x :/: b) = (regexize x) `append` (fromString "/") `append` b
    flatten (x :/: _) = x
instance ((a :/: c) ~ r, URLPiece b c)=> URLPiece (a :/: b) r where
    urlConstruct (x :/: rest) = (urlConstruct x) `append` (fromString "/") `append` (urlConstruct rest)
    regexize (x :/: rest) = (regexize x) `append` (fromString "/") `append` (regexize rest)
    flatten (x :/: rest) = (x :/: flatten rest)
instance (a ~ r, URLPiece a r) => URLPiece a r where
    urlConstruct x = ":var"
    regexize x = "([^/]+)"
    flatten x = x
instance URLPiece ByteString ByteString where
    urlConstruct x = x
    regexize x = x
    flatten x = x
    
----------------------------------
--- Application of Tuple Functions
----------------------------------

class TupleApply a b c | a -> c where
    tupleApply :: a -> b -> c

{- 
    The class is implemented with the Monadic Maybe
-}

instance TupleApply (a -> Maybe b) (a) (Maybe b) where
    tupleApply f inp = f inp
instance TupleApply ((a -> Maybe b) :/: (a1 -> Maybe b1)) (a, a1) (Maybe (b, b1))  where
    tupleApply (f1 :/: f2) (inp, inp2) = (,) <$> (f1 inp) <*> (f2 inp2) 
instance (TupleApply c c1 (Maybe c2)) => TupleApply ((a -> Maybe b) :/: c) (a, c1) (Maybe (b, c2))  where
    tupleApply (f1 :/: f2) (inp, inp2) = (,) <$> (f1 inp) <*> (tupleApply f2 inp2)
    
---------------------------------------------------
-- Chained Tuple Function Application
-- For Tuples in the format (a, (b, (c, ...)))
---------------------------------------------------

{-
    This function takes a curried function and a tuple chain and
    applys it. 
    
    E.g. 
    
    func :: ByteString -> ByteString -> ByteString -> ByteString -> Int
    
    -> unChain ("a", ("b", ("c", "d")))) func
    Will produce type Int
-}

class TupleChained a b c | a -> c where
    unChain :: a -> b -> c

instance TupleChained a (a -> b) b where
    unChain a func = func a

instance TupleChained (a, b) (a -> b -> c) c where
    unChain (a, b) func = func a b
    
instance (TupleChained c c1 c2) => TupleChained (a, c) (a -> c1) c2 where
    unChain (a, rest) func = unChain rest (func a)

---------------------------------------------------
-- TypeSafe Recursive URL Generation
---------------------------------------------------

class URLGen a b | a -> b where
    generateURLFunc :: a -> ByteString -> b

instance (URLFragment a) => URLGen (ByteString -> Maybe a) (a -> ByteString) where
    generateURLFunc _ s = (\x -> replace (urlize x) s)

instance (URLFragment a, URLFragment b) => URLGen ((ByteString -> Maybe a) :/: (ByteString -> Maybe b)) (a -> b -> ByteString) where
    generateURLFunc _ s = (\x y -> replace (urlize y) $ replace (urlize x) s)
    
instance (URLFragment a, URLGen b (c -> d)) => URLGen ((ByteString -> Maybe a) :/: b) (a -> c -> d) where
    generateURLFunc (_ :/: rest) s = (\x -> (generateURLFunc rest (replace (urlize x) s)))


-----------------
--- A Param Tree
-----------------
{-
    What this does is takes a tuple chain (a, (b, (c, ..)))
    And creates a function of type signature:
    
    [String] -> Maybe (String, (String, (String, ..))) The size of the original

-}


class ParamTree a b | a -> b where
    fillParams :: a -> ([ByteString] -> b)
    
instance ParamTree (ByteString -> a) (Maybe (ByteString)) where
    fillParams _ = (\params -> do
        case params of
            []        -> Nothing
            (f:[])    -> Just f)
                
instance ParamTree ((ByteString -> a) :/: (ByteString -> b)) (Maybe (ByteString, ByteString)) where
    fillParams _ = (\params -> do
        case params of
            []        -> Nothing
            (_:[])    -> Nothing
            (f:(s:_)) -> Just (f, s))
    
instance (ParamTree b (Maybe d)) => ParamTree ((ByteString -> a) :/: b) (Maybe (ByteString, d)) where
    fillParams (_ :/: rest) = (\params -> do
        case params of
            []        -> Nothing
            (f:t)    -> (,) <$> Just f <*> ((fillParams rest) t))

---------------------------------------
-- Function that replace the first :var
-- In the URL
---------------------------------------

replace :: ByteString -> ByteString -> ByteString
replace var url = ret
    where urlparts = C.split '/' url
          parts = break ((==':') . C.head) urlparts
          ret   = case parts of
                (_, []) -> url
                (h, (tar:rest)) -> C.intercalate "/" $ h ++ [var] ++ rest

---------------------------------
---- Regex Function
---------------------------------

isMatch :: ByteString -> ByteString -> Maybe [ByteString]
isMatch st pat = do
    let pMatches = st =~ pat :: [[ByteString]]
    case pMatches of
        [] -> Nothing
        ((h:t):_) -> Just t
        _ -> Nothing

----------------------------------
---- Simple URLFragment Type
----------------------------------
class URLFragment a where
    urlize :: a -> ByteString

---------------------------------
---- Convience Functions
---------------------------------

logit :: a -> IO a
logit a = print "got it" >> return a
    
urlMapBuilder :: (URLPiece a b, ParamTree b (Maybe f), TupleApply b f (Maybe g), TupleChained g d e) => a -> d -> (ByteString -> Maybe e)
urlMapBuilder piece f =
    let flattened = flatten $ piece
        regexPat  = "^" `C.append` (regexize piece)
        paramTree = fillParams $ flattened
    in
    
    (\x -> isMatch x regexPat >>= paramTree >>= (tupleApply flattened) >>= (\y -> Just (unChain y f)))
    
putURL :: (URLPiece a b, ParamTree b (Maybe f), TupleApply b f (Maybe g), TupleChained g d e) => a -> d -> ScatterBuilder e ()
putURL piece f = do
    let url = urlMapBuilder piece f
    tell [url]
    
    
renderURL :: (URLPiece a b, URLGen b c) => a -> c
renderURL piece =
    let flattened = flatten piece
        url = urlConstruct piece
    in
    (generateURLFunc flattened url)
