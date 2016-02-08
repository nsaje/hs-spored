module Scraper (
    EntryId,
    Entry(Entry, time, eId, title),
    Descriptions,
    Spored(Spored),
    Program
) where

import qualified Data.Map          as Map
import           Data.Time

type EntryId = String
type Program = String
data Entry = Entry { time  :: UTCTime
                   , eId   :: EntryId
                   , title :: String
                   } deriving (Eq, Show)
type Descriptions = Map.Map EntryId [String]

newtype Spored = Spored (Map.Map Program [Entry])

