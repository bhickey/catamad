module Main where

import Curses
import Simulate

import System.Locale.SetLocale

main :: IO ()
main = do
  setLocale LC_ALL (Just "en_US.UTF-8") >> runMain simulate
