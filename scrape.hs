module SiolSpored (main) where

import           Control.Monad
import qualified Data.List
import qualified Data.Map          as Map
import           Data.Time
import qualified Data.Time.Format
import           System.Locale
import           Text.HandsomeSoup
import           Text.XML.HXT.Core
import Control.Concurrent.Timer
import Control.Concurrent.Suspend
import Data.Acid

import Control.Monad.State                   ( get, put )
import Control.Monad.Reader                  ( ask )
import Control.Applicative                   ( (<$>) )
import System.Environment                    ( getArgs )
import Data.SafeCopy

type EntryId = String
data Entry = Entry { time  :: UTCTime
                   , eId   :: EntryId
                   , title :: String
                   } deriving (Eq, Show)
type Descriptions = Map.Map EntryId [String]

data Spored = Spored [Entry]

$(deriveSafeCopy 0 'base ''Spored)

addEntries :: [Entry] -> Update Spored ()
addEntries newEntries = do
    entries <- get
    put $ Database $ nub (entries ++ newEntries)



getTime :: (ArrowXml a) => a XmlTree String
getTime = css "span.i3" >>> (deep getText)

getTitleAndLink :: (ArrowXml a) => a XmlTree (String, String)
getTitleAndLink = css "span.i5" >>> ((deep getText) &&&
                                    ((css "a" ! "href")))

getDescription :: String -> IO ([String])
getDescription url = do
    let doc = fromUrl url
    descs <- runX $ doc >>> css "article#spored" >>> css "p"  /> getText
    return $ tail descs -- don't take head, which is date

getEntries :: String -> IO ([Entry])
getEntries url = do
    let doc = fromUrl url
    date <- runX $ doc >>> css "article" >>> css "p.sct" /> getText
    entriesRaw <- runX $ doc >>> css "div.def" >>> (getTime &&& getTitleAndLink)
    let entries = map (\(time, (title, eId)) -> Entry { time = readTime defaultTimeLocale "%H:%M, %d. %m. %Y" (time ++ (head date))
                                                      , eId=eId
                                                      , title=title
                                                      })
                      entriesRaw
    return entries

getDescriptions :: String -> [Entry] -> IO (Descriptions)
getDescriptions baseUrl entries = do
    descs <- mapM (\entry -> getDescription $ baseUrl ++ (eId entry)) entries
    return $ Map.fromList $ zip (map eId entries) descs

getNew :: IO ()
getNew = do
    let baseUrl = "http://www.siol.net/tv-spored.aspx"
    entries <- getEntries $ baseUrl ++ "?ch=TV+SLO+1&p1=1&p3=0&p4=0"
    descriptions <- getDescriptions baseUrl [(head entries)]
    mapM_ (putStrLn . show) [(head entries)]
    --putStrLn $ show descriptions

main :: IO ()
main = do
    ---timer <- repeatedTimer getNew (sDelay 1)
    timer <- repeatedTimer getNew (sDelay 1)
    suspend (sDelay 15)
    stopTimer timer
    putStrLn "finished"
    ---return ()
