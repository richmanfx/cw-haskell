{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module LibHtml where

import Lucid

mainHtml :: Html ()
mainHtml = html_ $ do
  head_ $ do
    title_ "CW Haskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]
    script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"] ""
  body_ [style_ "text-align:center;", id_ "body"] $ do
    h1_ "Welcome to 'CW Haskell'!"
    h5_ $ span_ "R5AM, Alex"
    br_ []
    hr_ []
    p_ $ do
      label_ "Количество слов: "
      input_ [type_ "text", id_ "number", placeholder_ "Number"]
      br_ []
      br_ []
      label_ "Скорость: "
      input_ [type_ "text", id_ "speed", placeholder_ "Speed"]
      br_ []
      br_ []
      label_ "Длительность пауз: "
      input_ [type_ "text", id_ "pause", placeholder_ "Pause"]
      br_ []
      br_ []
      button_ [onclick_ "sendJSON()"] "Проверить JSON"
    hr_ []
    p_ [class_ "result", style_ "color:blue"] ""

    hr_ []


stylesheet :: Html ()
stylesheet = link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]




--data HTML = HTML
--newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }
--
--instance Accept HTML where
--  contentType _ = "text" // "html" /: ("charset", "utf-8")
--
--instance MimeRender HTML RawHtml where
--  mimeRender _ = unRaw
--
---- EndPoint "cw"
--type MyAPI = "cw" :> Get '[HTML] RawHtml
