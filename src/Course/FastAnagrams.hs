{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams w path =
  -- error "todo: Course.FastAnagrams#fastAnagrams"
  let 
    s = foldRight (\e acc -> S.insert (toLower <$> e) acc) S.empty (permutations w) 
    xx input = filter (\aa -> S.member (toLower <$> aa) s) (words input)
  in xx <$> (readFile path)


newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
