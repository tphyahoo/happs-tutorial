module Misc where 

import HAppS.Server 
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.Trans
import Data.List
import qualified Data.Set as S
import Data.Digest.Pure.MD5
import Debug.Trace
import Text.PrettyPrint as PP
import System.FilePath (pathSeparator, takeDirectory)
import System.Directory (createDirectoryIfMissing)
import Safe (readMay)
import System.Time
import Data.Char (isAlphaNum)

-- import Text.StringTemplate
import Data.Monoid
import Control.Monad.Reader
import Data.Char (toLower)

newtype HtmlString = HtmlString String
instance ToMessage HtmlString where
  toContentType _ = B.pack "text/html"
  toMessage (HtmlString s) = L.pack s 

exactdir staticPath = spsIf (\rq -> rqURL rq == staticPath) 

spsIf :: (Monad m) => (Request -> Bool) -> [ServerPartT m a] -> ServerPartT m a
spsIf p sps = withRequest $ \rq ->
  if p rq
    then unServerPartT (mconcat sps) rq
    else mempty

pp[] = ( PP.render . vcat . map text . map show )


----------------- reading data ----------------
readData :: RqData a -> Request -> Maybe a
readData rqDataReader rq = runReaderT rqDataReader $ (rqInputs rq,rqCookies rq)  

pathPartsSp pps f = ServerPartT $ \rq ->
  if rqPaths rq == pps
    then f rq 
    else noHandle

-- Do something when the request has exactly one path segment left.
-- lastPathPartSp :: (Request -> String -> WebT IO Response) -> ServerPartT IO Response
lastPathPartSp0 f = ServerPartT $ \rq ->
  case rqPaths rq of
            [lastpart] -> f rq lastpart
            _ -> noHandle

{-
ifFirstPathPartSp pathpart f = ServerPartT $ \rq ->
  case rqPaths rq of
            (x:xs) -> f rq pathpart
            _ -> noHandle
-}

-- store passwords as md5 hash, as a security measure
scramblepass :: String -> String
scramblepass = show . md5 . L.pack

-- HStringTemplate modifications -- copy/paste/tweak from HStringTemplate.
-- same as directoryGroup, but throws an error for template names with punctuation



tFromTo = fromTo 10 20 [1..1000] == [10..20] 
fromTo fr to xs = take (to-(fr-1)) . drop (fr-1) $ xs 


quote x = '\"' : x ++ "\"" 





-- Windows/Unix/Mac compatible 
pathstring pathparts =
  let sep :: String
      sep = [pathSeparator] 
  in intercalate sep pathparts

safeRead :: (Monad m, Read a) => [Char] -> m a
safeRead s = 
  maybe (fail $ "safeRead: " ++ s)
        return 
        (readMay s)

timeSince x = do
  currTime <- getClockTime
  return . diffClockTimes currTime $ x

-- splitList 3 [1..11]
-- [([(1,1),(2,2),(3,3)],(1,3)),([(4,4),(5,5),(6,6)],(4,6)),([(7,7),(8,8),(9,9)],(7,9)),([(10,10),(11,11)],(10,11))]
-- the result is a list of (indexed sublist,(fist index, last index))
splitList :: Int -> [b] -> [([b], (Int, Int))]
splitList n x = let
  part = splitList' n ( zip [1..] x) 
  bounded = map (\l -> (map snd l,bounds l)) part
  bounds [] = (0,0)
  bounds l@((_,_):_) = (fst . head $ l,fst . last $ l)
  in bounded
  where 
    splitList' :: Int -> [a] -> [[a]]
    splitList' _ [] = []
    splitList' n l@(x:xs) =
      let (a,b') = genericSplitAt n l
          b = splitList' n b'
      in  a : b


lc = map toLower

-- write a file, creating parent directories if necessary
writeFileForce fp contents = do
  createDirectoryIfMissing True (takeDirectory fp)
  B.writeFile fp contents

--t = map isalphanum_S  ["asdf-","asdf_"]
-- check if a string is made of alphanumeric plus _
isalphanum_S = null . filter (not . isAlphaNum_)
isAlphaNum_ c = isAlphaNum c || '_' == c || '-' == c

allowedCharactersSnip = "please use only numbers, letters, '-', and '_'."