-- |
-- Module    : Network.Simplenote
-- Copyright : (c) 2009 marius a. eriksen
-- License   : BSD3
-- 
-- Maintainer:  marius a. eriksen <marius@monkey.org>
-- Stability :  provisional
-- 
-- Haskell API for Simplenote (http://www.simplenoteapp.com/)
-- 

module Network.Simplenote 
  (
  -- * Types for representing structured simplenote responses
    Index(..)
  , IndexEntry(..)
  , SearchResults(..)
  , SearchResult(..)

  -- * API methods
  , authenticate
  , index
  , note
  , search
  , delete
  , update
  ) where

import Control.Monad     (unless)
import Control.Exception (throw)
import Data.Maybe        (isJust, catMaybes)
import Text.JSON         ( JSON(..), JSValue(..), Result(..)
                         , fromJSObject, fromJSString, decode)
import Text.Printf       (printf)

import Network.Simplenote.Request

-- | Index responses
data Index      = Index [IndexEntry]
                  deriving (Show)
data IndexEntry = IndexEntry 
                    String      -- ^ key
                    String      -- ^ modify date
                    Bool        -- ^ deleted
                  deriving (Show)

-- | Search response
data SearchResults = SearchResults Int [SearchResult]
                     deriving (Show)
data SearchResult  = SearchResult 
                       String   -- ^ key
                       String   -- ^ content
                     deriving (Show)

-- Implement JSON parsers for the above for decoding responses:
instance JSON Index where
  showJSON _ = error "showJSON not implemented"
  readJSON (JSArray v) = mapM readJSON v >>= return . Index

instance JSON IndexEntry where
  showJSON _ = error "showJSON not implemented"
  readJSON (JSObject v) = do
    let kvs = fromJSObject v
        vs  = map (flip lookup kvs) ["key", "modify", "deleted"]
    unless (all isJust vs) $ fail "Missing keys."
    let vs' = catMaybes vs
    key     <- readJSON $ vs' !! 0
    modify  <- readJSON $ vs' !! 1
    deleted <- readJSON $ vs' !! 2
    return $ IndexEntry key modify deleted

instance JSON SearchResults where
  showJSON _ = error "showJSON not implemented"
  readJSON (JSObject v) = do
    -- Extract values in the Maybe monad:
    let values = do JSObject response' <- lookup "Response" $ fromJSObject v
                    let response = fromJSObject response'
                    totalRecords    <- lookup "totalRecords" response
                    JSArray results <- lookup "Results" response
                    return (totalRecords, results)

    case values of
      Just (totalRecords, results) -> do
        totalRecords' <- readJSON totalRecords
        results'      <- mapM readJSON results
        return $ SearchResults totalRecords' results'
      Nothing -> fail "JSON parse error"

instance JSON SearchResult where
  showJSON _ = error "showJSON not implemented"
  readJSON (JSObject v) = do
    let object = fromJSObject v
        values = do key     <- lookup "key" object
                    content <- lookup "content" object
                    return (key, content)
    case values of
      Just (key, content) -> do
        key'     <- readJSON key
        content' <- readJSON content
        return $ SearchResult key' content'
      Nothing -> fail "JSON parse error"

-- | Retrieve a note with the given key.
note :: Token                   -- ^ authentication token
     -> String                  -- ^ note key
     -> IO (Either String String)
note token key = get token "note" [("key", key)]

-- | List notes
index :: Token -> IO (Either String Index)
index token = decoded $ get token "index" []

-- | Search for text within all notes
search :: Token                 -- ^ authentication token
       -> String                -- ^ search string (query)
       -> Int                   -- ^ maximum number of results to return
       -> Int                   -- ^ offset into results
       -> IO (Either String SearchResults)
search token query maxResults offset =
  decoded $ get token "search" [ ("query", query)
                               , ("results", show maxResults)
                               , ("offset", show offset)
                               ]

-- | Delete the given note
delete :: Token                 -- ^ authentication token
       -> String                -- ^ note key
       -> IO (Either String ())
delete token key = do
  result <- get token "delete" [("key", key)]
  case result of 
    Left what -> return $ Left what
    Right _   -> return $ Right ()

-- | Update (or create) a note with new content. If 'Nothing' is
-- passed as a note ID, a new note is created, otherwise the named
-- note is updated with the new body. The note key is always returned.
-- 
-- Examples:
-- 
-- > Right token <- authenticate "foo@bob.com" "password"
-- > update token Nothing "a test note"
-- > search token "note" 1 0
update :: Token                 -- ^ authentication token
       -> Maybe String          -- ^ note key or 'Nothing'
       -> String                -- ^ new note body
       -> IO (Either String String)
update token key body = 
  let args = case key of 
               Just key' -> [("key", key')]
               Nothing   -> []
  in
    post token "note" args body

-- Internal:

decoded response = 
  response >>= return . either Left tryDecode
  where
    tryDecode body = 
      case decode body of
        Ok decoded -> Right decoded
        Error what -> Left $ printf "JSON decode error: %s" what

