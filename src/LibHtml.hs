{-# LANGUAGE DataKinds, MultiParamTypeClasses, TypeOperators, OverloadedStrings, ExtendedDefaultRules #-}

module LibHtml where

--import Lucid
--import Network.Wai(Application)
--import Network.Wai.Handler.Warp(run)
--import Data.ByteString.Lazy as Lazy
--import Network.HTTP.Media ((//), (/:))
--import Servant.API.ContentTypes (Accept(..), MimeRender)
--import Servant.Server.StaticFiles (serveDirectoryWebApp)
--import Servant(Proxy(..), (:>))

import Data.ByteString.Lazy as Lazy
--import qualified Data.Map as M
--import Data.Proxy
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
--import Servant.API
--import Servant.Server
--import Servant.Server.StaticFiles (serveDirectoryWebApp)
import qualified Data.ByteString.Char8 as BS

-- Сгенерировать HTML
startHtmlPage :: Html ()
startHtmlPage = html_ $ do
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

-- Создать тип содержимого для возврата HTML. У Servant этого по умолчанию нет.
data HTML = HTML    -- Фиктивный тип без данных, эквивалент типа JSON. Использовать его в Эндпоинтах.

newtype RawHtml = RawHtml { unRaw :: Lazy.ByteString }    -- Простой тип-оболочка для байтовой строки HTML.

-- Заменить в Accept contentType по умолчанию (JSON-ный) на HTML-ный
--instance Accept HTML where
--  contentType _ = "text" // "html" /: ("charset", "utf-8")

--instance MimeRender HTML RawHtml where
--  mimeRender _ = unRaw

--instance Show a => MimeRender HTML a where
--  mimeRender _ val = unRaw

---- EndPoint "cw"
--type MyAPI = "cw" :> Get '[HTML] RawHtml
--type MyAPI =
--  "users"  :> Capture "uid" Int :> Get '[HTML] RawHtml :<|>
--  Raw


 -- Имплементировать API и создать WAI-application (WAI - Web Application Interface).
 --webApplication = serve (Proxy :: Proxy MyAPI) myServer
webApplication :: Application
webApplication _ respond = respond $
  responseLBS status200     -- Статус
              [(hContentType, "text/html; charset=utf-8")]    -- Заголовки
              (renderBS startHtmlPage)        -- Тело

--myServer :: Server MyAPI
--myServer = myHandler

--myHandler :: Html
--myHandler = do
--  return startHtmlPage

-- Запустить WAI-приложение в warp-сервере на заданном порту 
webAppEntry :: IO ()
webAppEntry = run 8085 $ withLogging webApplication   -- TODO: Порт брать из конфига


withLogging :: Middleware
withLogging app req respond =
  app req $ \response -> do
    Prelude.putStrLn $ status response ++ ": " ++ query
    respond response
  where
    query = BS.unpack
          $ BS.concat [ rawPathInfo    req
                      , rawQueryString req ]
    status = show . statusCode . responseStatus

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
