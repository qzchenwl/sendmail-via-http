{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.ShQQ
import           Network.CGI (liftIO)
import           Control.Applicative ((<$>))
import           Data.Int
import           Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import qualified Data.ByteString.Char8 as S (unpack, pack)

main :: IO ()
main = quickHttpServe $
    route [ ("mailto/:receiver/:subject", sendMailHandler)
          , ("static"                   , fileServer)
          ]
  where
    fileServer = serveDirectory "/home/admin/BizCardCallCount"

sendMailHandler :: Snap()
sendMailHandler = do
    receiver <- decodeParam "receiver"
    subject  <- decodeParam "subject"
    body     <- readRequestBody (maxBound :: Int64)
    result <- liftIO $ sendMail (S.unpack receiver)
                                (S.unpack subject)
                                (L.unpack body)
    writeBS $ S.pack result
  where
    decodeParam p = fromMaybe "" <$> getParam p

sendMail :: String -> String -> String -> IO String
sendMail receiver subject body =
    [sh| echo $body | mail -s $subject $receiver |]
