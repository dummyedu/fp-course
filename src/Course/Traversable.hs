{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module Course.Traversable where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.List
import Course.ExactlyOne
import Course.Optional
import Course.Compose

-- | All instances of the `Traversable` type-class must satisfy three laws. These
-- laws are not checked by the compiler. These laws are given as:
--
-- * The law of naturality
--   `∀f g. f . traverse g ≅ traverse (f . g)`
--
-- * The law of identity
--   `∀x. traverse ExactlyOne x ≅ ExactlyOne x`
--
-- * The law of composition
--   `∀f g. traverse ((g <$>) . f) ≅ (traverse g <$>) . traverse f`
class Functor t => Traversable t where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> t a
    -> f (t b)

instance Traversable List where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> List a
    -> f (List b)
  traverse f =
    foldRight (\a b -> (:.) <$> f a <*> b) (pure Nil)

instance Traversable ExactlyOne where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> ExactlyOne a
    -> f (ExactlyOne b)
  traverse f (ExactlyOne a) =
    -- error "todo: Course.Traversable traverse#instance ExactlyOne"
    pure <$> f a

instance Traversable Optional where
  traverse ::
    Applicative f =>
    (a -> f b)
    -> Optional a
    -> f (Optional b)
  traverse _ Empty = pure Empty
  traverse f (Full a) = pure <$> f a
    -- error "todo: Course.Traversable traverse#instance Optional"


-- | Sequences a traversable value of structures to a structure of a traversable value.
--
-- >>> sequenceA (ExactlyOne 7 :. ExactlyOne 8 :. ExactlyOne 9 :. Nil)
-- ExactlyOne [7,8,9]
--
-- >>> sequenceA (Full (ExactlyOne 7))
-- ExactlyOne (Full 7)
--
-- >>> sequenceA (Full (*10)) 6
-- Full 60
sequenceA ::
  (Applicative f, Traversable t) =>
  t (f a)
  -> f (t a)
sequenceA f =
  -- error "todo: Course.Traversable#sequenceA"
  traverse id f

instance (Traversable f, Traversable g) =>
  Traversable (Compose f g) where
-- Implement the traverse function for a Traversable instance for Compose
  traverse f (Compose a) =
    -- error "todo: Course.Traversable traverse#instance (Compose f g)"
    Compose <$> (traverse (traverse f) a)

-- | The `Product` data type contains one value from each of the two type constructors.
data Product f g a =
  Product (f a) (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Product f g) where
-- Implement the (<$>) function for a Functor instance for Product
  (<$>) ff (Product f g) =
    -- error "todo: Course.Traversable (<$>)#instance (Product f g)"
    Product (ff <$> f) (ff <$> g)

instance (Traversable f, Traversable g) =>
  Traversable (Product f g) where
-- Implement the traverse function for a Traversable instance for Product
  traverse ff (Product f g) =
    -- error "todo: Course.Traversable traverse#instance (Product f g)"
    lift2 Product (traverse ff f) (traverse ff g)

-- | The `Coproduct` data type contains one value from either of the two type constructors.
data Coproduct f g a =
  InL (f a)
  | InR (g a) deriving (Show, Eq)

instance (Functor f, Functor g) =>
  Functor (Coproduct f g) where
-- Implement the (<$>) function for a Functor instance for Coproduct
  (<$>) ff (InL f) = InL (ff <$> f)
  (<$>) ff (InR f) = InR (ff <$> f)
    -- error "todo: Course.Traversable (<$>)#instance (Coproduct f g)"

instance (Traversable f, Traversable g) =>
  Traversable (Coproduct f g) where
-- Implement the traverse function for a Traversable instance for Coproduct
  traverse ff (InL f) = InL <$> (traverse ff f)
  traverse ff (InR f) = InR <$> (traverse ff f)
    -- error "todo: Course.Traversable traverse#instance (Coproduct f g)"
