module App.Utils.RouteValue where

import Prelude
import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Generic (class Generic)

data RouteValue a = NotInitialized | FromRoute a | FromComponent a

fromRouteValue :: forall a. a -> RouteValue a -> a
fromRouteValue a NotInitialized = a
fromRouteValue _ (FromRoute a) = a
fromRouteValue _ (FromComponent a) = a

derive instance genericRouteValue :: Generic a => Generic (RouteValue a)
instance showRouteValue :: ( Show a ) => Show (RouteValue a) where
  show NotInitialized = "NotInitialized"
  show (FromRoute a) = "FromRoute " <> show a
  show (FromComponent a) = "FromComponent " <> show a

instance functorRouteValue :: Functor RouteValue where
  map f NotInitialized = NotInitialized
  map f (FromRoute a) = FromRoute $ f a
  map f (FromComponent a) = FromComponent $ f a

instance applyRouteValue :: Apply RouteValue where
  apply NotInitialized _ = NotInitialized
  apply (FromRoute f) (FromRoute a) = FromRoute $ f a
  apply (FromRoute f) (FromComponent a) = FromComponent $ f a
  apply (FromRoute f) NotInitialized = NotInitialized
  apply (FromComponent f) (FromRoute a) = FromRoute $ f a
  apply (FromComponent f) (FromComponent a) = FromComponent $ f a
  apply (FromComponent f) NotInitialized = NotInitialized

instance applicativeRouteValue :: Applicative RouteValue where
  pure = const NotInitialized


instance altRouteValue :: Alt RouteValue where
  alt (FromComponent a) _ = FromComponent a
  alt _ (FromComponent a) = FromComponent a
  alt (FromRoute a) _ = FromRoute a
  alt _ (FromRoute a) = FromRoute a
  alt NotInitialized a = a

instance plusRouteValue :: Plus RouteValue where
  empty = NotInitialized

