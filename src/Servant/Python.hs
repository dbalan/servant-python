{-# LANGUAGE FlexibleContexts #-}

module Servant.Python (
  pythonForAPI
  -- debug
  , requestsForAPI
  ) where

import qualified Data.Text as Text
import qualified Servant.Foreign as SF
import qualified Data.Proxy as Proxy
import           Control.Lens
import           Data.Monoid ((<>))

-- types that changes in 0.8
type Request = ()
type Language = SF.NoTypes

-- proxy request/lang info
languageProxy :: Proxy.Proxy Language
languageProxy = Proxy.Proxy

requestProxy :: Proxy.Proxy Request
requestProxy = Proxy.Proxy

requestsForAPI :: (SF.HasForeign Language Request api,
                  SF.GenerateList Request (SF.Foreign Request api))
                  => Proxy.Proxy api -> [SF.Req Request]
requestsForAPI api = SF.listFromAPI languageProxy requestProxy api

-- conversion functions
hasBody :: SF.Req Request -> Bool
hasBody r = case (r ^. SF.reqBody) of
  Nothing -> False
  Just _ -> True

functionName :: SF.Req Request -> Text.Text
functionName req = SF.snakeCase $ req ^. SF.reqFuncName

functionArguments :: SF.Req Request -> Text.Text
functionArguments req =
  Text.concat [ "(", Text.intercalate ", " args, ")"]
  where
    args = captures ++ qparam ++ body ++ headers

    captures = map (view SF.argPath . SF.captureArg)
               $ filter SF.isCapture
               $ req ^. SF.reqUrl . SF.path

    qparam = map (view $ SF.queryArgName . SF.argPath) queryParams

    body = if hasBody req
           then ["body"]
           else []
    queryParams = req ^.. SF.reqUrl . SF.queryStr . traverse
    headers = map ((<>) "header"
                    . view (SF.headerArg . SF.argPath)
                  ) $ req ^. SF.reqHeaders

-- TODO: use <>
renderRequest :: SF.Req Request -> Text.Text
renderRequest req = "def " <> functionName req <> functionArguments req <> ":\n"
                    <> "path = " <> pathName req <> "\n"
--                     <> "res = requests." <> methodName req <> "(\n"
--                    <> ""

renderRequests :: [SF.Req Request] -> Text.Text
renderRequests reqs = Text.intercalate "\n\n" $ map renderRequest reqs

-- main entry point for conversion.
pythonForAPI :: (SF.HasForeign Language Request api,
                 SF.GenerateList Request (SF.Foreign Request api))
                => Proxy.Proxy api -> Text.Text
pythonForAPI api = renderRequests $ requestsForAPI api
