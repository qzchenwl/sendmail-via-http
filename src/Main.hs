{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Snap.Core
import           Snap.Http.Server
import           Snap.Util.FileServe
import           System.ShQQ
import           Data.Int
import           Network.CGI (liftIO)
import           Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import qualified Data.ByteString.Char8 as S (unpack)

main :: IO ()
main = quickHttpServe $
    route [ ("mailto/:receiver/:subject", sendMailHandler)
          , ("static"                   , fileServer)
          ]
  where
    fileServer = serveDirectory "/home/admin/BizCardCallCount"

sendMailHandler :: Snap()
sendMailHandler = do
    receiver <- getParam "receiver"
    subject  <- getParam "subject"
    body     <- readRequestBody (maxBound :: Int64)
    liftIO $ sendMail (S.unpack $ fromJust receiver)
                      (S.unpack $ fromJust subject)
                      (L.unpack body)
    writeBS "mail sent\n"

sendMail :: String -> String -> String -> IO String
sendMail receiver subject body =
    [sh| echo $body | mail -s $subject $receiver |]
