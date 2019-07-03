{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Course.Anagrams where

import Course.Core
import Course.List
import Course.Functor

{-

Functions you will need
--
* fmap :: (a -> b) -> IO a -> IO b
* readFile :: FilePath -> IO Str
* lines :: Str -> [Str]
* permutations :: [a] -> [[a]]
* intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
* toLower :: Char -> Char

Functions that might help
-
* on :: (b -> b -> c) -> (a -> b) -> a -> a -> c

-}


-- Return all anagrams of the given string
-- that appear in the given dictionary file.
anagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
anagrams w path =
  -- let xx input = filter (\i -> length (filter (equalIgnoringCase w) (permutations i)) > 0) (words input)

  let nn = permutations w
      f i = length (filter (equalIgnoringCase i) nn) > 0
      xx input = filter (\i -> f i) (words input)
  in xx <$> (readFile path)
  -- error "todo: Course.Anagrams#anagrams"
-- anagrams name =
--   (<$>) (intersectBy equalIgnoringCase (permutations name) . words) . readFile

-- Compare two strings for equality, ignoring case
equalIgnoringCase ::
  Chars
  -> Chars
  -> Bool
equalIgnoringCase a b = (isPrefixOf (toLower <$> a) (toLower <$> b)) && (length a == length b)
  -- error "todo: Course.Anagrams#equalIgnoringCase"
