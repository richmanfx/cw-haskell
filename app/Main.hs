module Main where
  
import LibHtml

main :: IO ()
main = do
  webAppEntry 8085       -- TODO: Порт брать из конфига
