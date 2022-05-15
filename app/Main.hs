module Main where

--import Lib
import Lucid
import LibHtml

main :: IO ()
--main = webAppEntry
main = renderToFile "index.html" mainHtml
