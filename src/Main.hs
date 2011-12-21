module Main where

import Curses
import Hello

import System.IO
import System.Locale.SetLocale

main :: IO ()
main = do
  locale <- setLocale LC_ALL (Just "en_US.UTF-8")
  runMain helloWorld
