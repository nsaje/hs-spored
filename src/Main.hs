{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import SiolScraper (main)


$(deriveSafeCopy 0 'base ''Entry)
$(deriveSafeCopy 0 'base ''Spored)

addEntries :: [Entry] -> Update Spored ()
addEntries newEntries = do
    Spored entries <- get
    put $ Spored $ Data.List.nub (entries ++ newEntries)
$(makeAcidic ''Spored ['addEntries])

-- Hello World
exeMain = do
    SiolScraper.main

main = exeMain
