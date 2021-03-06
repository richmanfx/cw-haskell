{-# LANGUAGE DataKinds, MultiParamTypeClasses, OverloadedStrings, ExtendedDefaultRules #-}

module LibHtml where

import Lucid
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS

-- Стартовая HTML страница
startHtmlPage :: Html ()
startHtmlPage = html_ $ do
  head_ $ do
    title_ "CW Haskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "cw.css"]
    script_ [src_ "https://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"] ""
  body_ [style_ "text-align:center;", id_ "body"] $ do
    h1_ "Welcome to 'CW Haskell'!"
    h5_ $ span_ "(R5AM, Alex)"
    p_ $ do
      form_ [method_ "GET", action_ "/cw", enctype_ "text/plain"] $ do
        fieldset_ $ do
          legend_ "Параметры тренировочного текста"
          label_ "Количество слов: "
          input_ [type_ "number", min_ "1", max_ "999", name_ "number", placeholder_ "Number", pattern_ "[0-9]{,3}"
                , required_ "", value_ "50"]
          br_ []
          br_ []
          label_ "Скорость: "
          input_ [type_ "number", min_ "40", max_ "200", step_ "10", name_ "speed", placeholder_ "Speed"
                , pattern_ "[0-9]{2,3}", required_ "", value_ "100"]
          br_ []
          br_ []
          label_ "Длительность пауз: "
          input_ [type_ "number", min_ "1", max_ "30", name_ "pause", placeholder_ "Pause", pattern_ "[0-9]{,2}"
                , required_ "", value_ "1"]
          br_ []
          br_ []
          button_ "Сгенерировать текст"
    p_ [class_ "result", style_ "color:blue"] ""

--    hr_ []

-- Страница с сообщением от сервера
messageHtmlPage :: Html ()    -- TODO: параметриризовать текстом сообщения
messageHtmlPage = html_ $ do
  head_ $ do
    title_ "CW Haskell"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "cw.css"]
  body_ [style_ "text-align:center;", id_ "body"] $ do
    br_ []
    br_ []
    h1_ [style_ "color:red"] "Only GET method is allowed!"

stylesheet :: Html ()
stylesheet = link_ [rel_ "stylesheet", type_ "text/css", href_ "screen.css"]

-- Имплементировать API и создать WAI-application (WAI - Web Application Interface).
webApplication :: Application
webApplication req respond = respond $
  if requestMethod req /= methodGet
  then responseBadRequest (renderBS messageHtmlPage)    -- Обрабатывать только GET-запросы, на другие ругаться
  else responseLBS status200     -- Статус
                     [(hContentType, "text/html; charset=utf-8")]    -- Заголовки
                     (renderBS startHtmlPage)        -- Тело

-- Запустить WAI-приложение в warp-сервере на заданном порту
webAppEntry :: IO ()
webAppEntry = do
--  tcpPort = 8085
  putStrLn "\nStart server on 8085 port ..."
  run 8085 $ withLogging webApplication   -- TODO: Порт брать из конфига

-- Логирование запросов
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


-- Функции для работы с ответами
responseOk, responseNotFound, responseBadRequest :: LBS.ByteString -> Response
responseOk         = responsePlainText status200
responseNotFound   = responsePlainText notFound404
responseBadRequest = responsePlainText badRequest400

responsePlainText :: Status -> LBS.ByteString -> Response
responsePlainText = (`responseLBS` [(hContentType, "text/html; charset=utf-8")])
