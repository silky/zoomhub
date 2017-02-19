{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.APISpec
  ( main
  , spec
  ) where

import           Control.Concurrent           (getNumCapabilities)
import qualified Data.ByteString.Char8        as BC
import           Data.Monoid                  ((<>))
import           Data.Time.Units              (Second)
import           Network.HTTP.Types           (methodGet)
import           Network.URI                  (URI, parseURIReference)
import           Network.Wai                  (Middleware)
import           System.IO.Unsafe             (unsafePerformIO)
import           Test.Hspec                   (Spec, describe, hspec, it)
import           Test.Hspec.Wai               (MatchHeader, ResponseMatcher,
                                               get, matchHeaders, matchStatus,
                                               post, put, request,
                                               shouldRespondWith, with, (<:>))

import           ZoomHub.API                  (app)
import           ZoomHub.Config               (Config (..), ExistingContentStatus (IgnoreExistingContent), NewContentStatus (NewContentDisallowed))
import qualified ZoomHub.Config               as Config
import           ZoomHub.Storage.PostgreSQL   (ConnectInfo (..),
                                               defaultConnectInfo)
import           ZoomHub.Storage.PostgreSQL   (createConnectionPool)
import           ZoomHub.Types.BaseURI        (BaseURI (BaseURI))
import           ZoomHub.Types.ContentBaseURI (mkContentBaseURI)
import           ZoomHub.Types.ContentId      (ContentId, fromString,
                                               unContentId)
import           ZoomHub.Types.DatabasePath   (DatabasePath (DatabasePath))
import           ZoomHub.Types.StaticBaseURI  (StaticBaseURI (StaticBaseURI))
import           ZoomHub.Types.TempPath       (TempPath (TempPath))

main :: IO ()
main = hspec spec

-- Helpers
toURI :: String -> URI
toURI s =
  case parseURIReference s of
    Just uri -> uri
    _ -> error $ "ZoomHub.APISpec.toURI: Failed to parse URI: " ++ s

existingContent :: (ContentId, String)
existingContent =
  ( fromString "h"
  , "http://upload.wikimedia.org/wikipedia/commons/3/36/\
      \SeattleI5Skyline.jpg#zoomhub=h"
  )

-- Matchers
applicationJSON :: MatchHeader
applicationJSON = "Content-Type" <:> "application/json"

plainTextUTF8 :: MatchHeader
plainTextUTF8 = "Content-Type" <:> "text/plain; charset=utf-8"

plainText :: MatchHeader
plainText = "Content-Type" <:> "text/plain"

javaScriptUTF8 :: MatchHeader
javaScriptUTF8 = "Content-Type" <:> "application/javascript;charset=utf-8"

invalidURL :: ResponseMatcher
invalidURL =
  "Please give us the full URL, including ‘http://’ or ‘https://’."
  { matchStatus = 400
  , matchHeaders = [plainTextUTF8]
  }

invalidHTTPMethod :: ResponseMatcher
invalidHTTPMethod =
  "Only GET or HEAD is supported"
  { matchStatus = 405
  , matchHeaders = [plainText]
  }

noNewContent :: ResponseMatcher
noNewContent =
  "We are currently not processing new content."
  { matchStatus = 503
  , matchHeaders = [plainTextUTF8]
  }

restRedirect :: ContentId -> ResponseMatcher
restRedirect cId =
    ""
    { matchStatus = 301
    , matchHeaders = ["Location" <:> BC.pack expectedLocation]
    }
  where
    baseURIPrefix = show . Config.baseURI $ config
    expectedLocation = baseURIPrefix ++ "/v1/content/" ++ unContentId cId

-- Config
nullLogger :: Middleware
nullLogger = id

config :: Config
config = Config
    { baseURI = BaseURI (toURI "http://localhost:8000")
    , contentBaseURI =
        case mkContentBaseURI (toURI "http://localhost:9000") (toURI "_dzis_") of
          Just uri -> uri
          _ -> error $ "ZoomHub.APISpec: Failed to parse `Config.contentBaseURI`."
    , dbConnInfo = dbConnInfo'
    , dbConnPool = dbConnPool'
    , dbConnPoolIdleTime = dbConnPoolIdleTime'
    , dbConnPoolMaxResourcesPerStripe = dbConnPoolMaxResourcesPerStripe'
    , dbConnPoolNumStripes = dbConnPoolNumStripes'
    , dbPath = DatabasePath "./data/zoomhub-development.sqlite3"
    , encodeId = show
    , error404 = "404"
    , existingContentStatus = IgnoreExistingContent
    , logger = nullLogger
    , newContentStatus = NewContentDisallowed
    , openSeadragonScript = "osd"
    , port = 8000
    , publicPath = "./public"
    , rackspace = undefined
    , staticBaseURI = StaticBaseURI (toURI "http://static.zoomhub.net")
    , tempPath = TempPath "./data/temp"
    , version = "test"
    }
  where
    numSpindles = 1
    -- TODO: How can we avoid `unsafePerformIO`?
    numCapabilities = fromIntegral $ unsafePerformIO getNumCapabilities
    dbConnInfo' = defaultConnectInfo { connectDatabase = "zoomhub_test" }
    -- TODO: How can we avoid `unsafePerformIO`?
    dbConnPool' = unsafePerformIO $ createConnectionPool dbConnInfo'
      dbConnPoolNumStripes' dbConnPoolIdleTime' dbConnPoolMaxResourcesPerStripe'
    dbConnPoolIdleTime' = 10 :: Second
    dbConnPoolMaxResourcesPerStripe' = (numCapabilities * 2) + numSpindles
    dbConnPoolNumStripes' = 1

spec :: Spec
spec = with (return $ app config) $ do
  describe "RESTful" $ do
    describe "List (GET /v1/content)" $
        it "should be interpreted as a ‘get by URL’, with no URL given" $
          get "/v1/content" `shouldRespondWith` "Missing ID or URL.\
            \ Please provide ID, e.g. `/v1/content/<id>`,\
            \ or URL via `/v1/content?url=<url>` query parameter."
            { matchStatus = 400
            , matchHeaders = [plainTextUTF8]
            }

    describe "Get by URL (GET /v1/content?url=…)" $ do
      it "should reject empty URLs" $
        get "/v1/content?url=" `shouldRespondWith`
          invalidURL

      it "should reject malformed URLs" $
        get "/v1/content?url=lasjdoasj)(¨‚Ô‚ˆÔ∏ŒÂ;sd)" `shouldRespondWith`
          invalidURL

      it "should reject URLs without protocol" $
        get "/v1/content?url=example.com" `shouldRespondWith`
          invalidURL

      it "should reject URLs with non-HTTP protocol" $ do
        get "/v1/content?url=ftp://example.com" `shouldRespondWith`
          invalidURL

        get "/v1/content?url=mailto://example@example.com" `shouldRespondWith`
          invalidURL

      it "should reject new HTTP URLs (for now)" $
        get "/v1/content?url=http://example.com" `shouldRespondWith`
          noNewContent

      it "should redirect existing (converted) HTTP URLs to ID" $
        let (existingId, existingURL) = existingContent in
        get ("/v1/content?url=" <> BC.pack existingURL) `shouldRespondWith`
          restRedirect existingId

    describe "Get by ID (GET /v1/content/:id)" $ do
      it "should return correct data for existing content" $
        get "/v1/content/4rcn" `shouldRespondWith`
          "{\"dzi\":{\"height\":3750,\"url\":\
            \\"http://localhost:9000/_dzis_/4rcn.dzi\",\"width\":5058,\
            \\"tileOverlap\":1,\"tileFormat\":\"jpg\",\"tileSize\":254},\
            \\"progress\":1,\"url\":\"http://media.stenaline.com/media_SE/\
            \lalandia-map-zoomit/lalandia-map.jpg\",\"embedHtml\":\
            \\"<script src=\\\"http://localhost:8000/4rcn.js?width=auto&\
            \height=400px\\\"></script>\",\"shareUrl\":\"http://localhost:8000\
            \/4rcn\",\"id\":\"4rcn\",\"ready\":true,\"failed\":false}"
          { matchStatus = 200
          , matchHeaders = [applicationJSON]
          }

      it "should return 404 non-existent content" $
        get "/v1/content/nonExistentContent" `shouldRespondWith`
          "No content with ID: nonExistentContent"
          { matchStatus = 404
          , matchHeaders = [plainTextUTF8]
          }

    describe "POST /v1/content?url=…" $
      it "should be rejected" $
        post "/v1/content?url=http://example.com" "" `shouldRespondWith`
          invalidHTTPMethod

    describe "PUT /v1/content?url=…" $
      it "should be rejected" $
        put "/v1/content?url=http://example.com" "" `shouldRespondWith`
          invalidHTTPMethod

  describe "JSONP" $ do
    describe "GET /v1/content?url=…&callback=…" $
      it "should accept `callback` query parameter" $
        get "/v1/content?callback=handleContent" `shouldRespondWith`
          "/**/ typeof handleContent === 'function' &&\
          \ handleContent({\"status\":400,\"error\":\"Missing ID or URL.\
          \ Please provide ID, e.g. `/v1/content/<id>`, or URL via\
          \ `/v1/content?url=<url>` query parameter.\",\"statusText\":\
          \\"Bad Request\",\"redirectLocation\":null});"
          { matchStatus = 200
          , matchHeaders = [javaScriptUTF8]
          }

    describe "GET /v1/content/:id?callback=…" $
      it "should accept `callback` query parameter" $
        get "/v1/content/4rcn?callback=handleContent" `shouldRespondWith`
          "/**/ typeof handleContent === 'function' && \
          \handleContent({\"status\":200,\"statusText\":\"OK\",\"content\":\
          \{\"dzi\":{\"height\":3750,\"url\":\
          \\"http://localhost:9000/_dzis_/4rcn.dzi\",\"width\":5058,\
          \\"tileOverlap\":1,\"tileFormat\":\"jpg\",\"tileSize\":254},\
          \\"progress\":1,\"url\":\"http://media.stenaline.com/media_SE/\
          \lalandia-map-zoomit/lalandia-map.jpg\",\"embedHtml\":\"<script \
          \src=\\\"http://localhost:8000/4rcn.js?width=auto&height=400px\\\">\
          \</script>\",\"shareUrl\":\"http://localhost:8000/4rcn\",\"id\":\
          \\"4rcn\",\"ready\":true,\"failed\":false},\
          \\"redirectLocation\":null});"
          { matchStatus = 200
          , matchHeaders = [javaScriptUTF8]
          }

  describe "CORS" $
    it "should allow all origins" $
      let getWithHeader path headers = request methodGet path headers "" in
      getWithHeader "/v1/content/4rcn" [("Origin", "http://example.com")]
        `shouldRespondWith` 200 {
          matchHeaders =
            [ "Access-Control-Allow-Origin" <:> "*"
            , applicationJSON
            ]
        }

  describe "Meta" $ do
    describe "Health (/health)" $
      it "should respond with `up`" $
        get "/health" `shouldRespondWith` "up" {matchStatus = 200}

    describe "Version (/version)" $
      it "should respond with version" $
        get "/version" `shouldRespondWith` "test" {matchStatus = 200}
