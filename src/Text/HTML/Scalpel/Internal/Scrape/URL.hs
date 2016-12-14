{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK hide #-}
module Text.HTML.Scalpel.Internal.Scrape.URL (
    URL
,   Config (..)
,   Decoder

,   defaultDecoder
,   utf8Decoder
,   iso88591Decoder

,   scrapeURL
,   scrapeURLWithOpts
,   scrapeURLWithConfig
) where

import Text.HTML.Scalpel.Internal.Scrape

import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.Default (def)
import Data.List (isInfixOf)
import Data.Maybe (listToMaybe)
import Data.ByteString.Char8 (unpack)
import Control.Lens ((^.), (^?))

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Default as Default
import qualified Data.Text.Lazy.Encoding as Text
import qualified Network.Wreq as Wreq
import qualified Text.HTML.TagSoup as TagSoup
import qualified Text.StringLike as TagSoup



type URL = String

type WreqResponse = Wreq.Response BSL.ByteString

-- | A method that takes a HTTP response as raw bytes and returns the body as a
-- string type.
type Decoder str = Wreq.Response BSL.ByteString -> str

-- | A record type that determines how 'scrapeUrlWithConfig' interacts with the
-- HTTP server and interprets the results.
data Config str = Config {
    wreqOpts :: Wreq.Options
,   decoder  :: Decoder str
}

instance TagSoup.StringLike str => Default.Default (Config str) where
    def = Config {
            wreqOpts = Wreq.defaults
        ,   decoder  = defaultDecoder
        }

-- | The 'scrapeURL' function downloads the contents of the given URL and
-- executes a 'Scraper' on it.
scrapeURL :: (Ord str, TagSoup.StringLike str)
          => URL -> Scraper str a -> IO (Maybe a)
scrapeURL = scrapeURLWithOpts Wreq.defaults

-- | The 'scrapeURLWithOpts' function take a list of curl options and downloads
-- the contents of the given URL and executes a 'Scraper' on it.
scrapeURLWithOpts :: (Ord str, TagSoup.StringLike str)
                  => Wreq.Options -> URL -> Scraper str a -> IO (Maybe a)
scrapeURLWithOpts options = scrapeURLWithConfig (def {wreqOpts = options})

-- | The 'scrapeURLWithConfig' function takes a 'Config' record type and
-- downloads the contents of the given URL and executes a 'Scraper' on it.
scrapeURLWithConfig :: (Ord str, TagSoup.StringLike str)
                  => Config str -> URL -> Scraper str a -> IO (Maybe a)
scrapeURLWithConfig config url scraper = do
    maybeTags <- downloadAsTags (decoder config) url
    return (maybeTags >>= scrape scraper)
    where
        downloadAsTags decoder url = do
            maybeBytes <- openURIWithOpts url (wreqOpts config)
            return $ TagSoup.parseTags . decoder <$> maybeBytes

openURIWithOpts :: URL -> Wreq.Options -> IO (Maybe WreqResponse)
openURIWithOpts url opts = do
    resp <- wreqGetResponse_ url opts
    return $ if status resp /= 200
        then Nothing
        else Just resp
      where status r = r ^. Wreq.responseStatus . Wreq.statusCode

wreqGetResponse_ :: URL
                 -> Wreq.Options
                 -> IO WreqResponse
wreqGetResponse_ = flip Wreq.getWith

-- | The default response decoder. This decoder attempts to infer the character
-- set of the HTTP response body from the `Content-Type` header. If this header
-- is not present, then the character set is assumed to be `ISO-8859-1`.
defaultDecoder :: TagSoup.StringLike str => Decoder str
defaultDecoder response = TagSoup.castString
                        $ choosenDecoder body
    where
        body        = response ^. Wreq.responseBody
        contentType = response ^? Wreq.responseHeader "Content-Type"

        isType t | Just ct <- contentType = ("charset=" ++ t) `isInfixOf` (unpack ct)
                 | otherwise              = False

        choosenDecoder | isType "utf-8" = Text.decodeUtf8
                       | otherwise      = Text.decodeLatin1

-- | A decoder that will always decode using `UTF-8`.
utf8Decoder ::  TagSoup.StringLike str => Decoder str
utf8Decoder = TagSoup.castString . Text.decodeUtf8 .
              (^. Wreq.responseBody)

-- | A decoder that will always decode using `ISO-8859-1`.
iso88591Decoder ::  TagSoup.StringLike str => Decoder str
iso88591Decoder = TagSoup.castString . Text.decodeLatin1 .
                  (^. Wreq.responseBody)
