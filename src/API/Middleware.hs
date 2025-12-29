{-# LANGUAGE OverloadedStrings #-}

module API.Middleware
  ( apiMiddleware
  , corsPolicy
  , customFormatters
  ) where

import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant
import Data.Aeson (encode, object, (.=))
import Data.Text (Text)

corsPolicy :: CorsResourcePolicy
corsPolicy = simpleCorsResourcePolicy
  { corsRequestHeaders = ["Content-Type", "Authorization"]
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsOrigins = Nothing
  }

apiMiddleware :: Middleware
apiMiddleware = cors (const $ Just corsPolicy) . logStdoutDev

customFormatters :: ErrorFormatters
customFormatters = defaultErrorFormatters
  { notFoundErrorFormatter = notFoundFormatter
  , bodyParserErrorFormatter = bodyParserFormatter
  }

notFoundFormatter :: NotFoundErrorFormatter
notFoundFormatter req =
  err404 { errBody = encode $ object
    [ "error" .= ("Route not found" :: Text)
    , "path" .= rawPathInfo req
    ]
  }

bodyParserFormatter :: BodyParserErrorFormatter
bodyParserFormatter _ _ errMsg =
  err400 { errBody = encode $ object
    [ "error" .= ("Invalid request body" :: Text)
    , "details" .= errMsg
    ]
  }
