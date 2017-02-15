{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pipend.Policy (
  Role(..),
  AccessLevel(..), AccessPolicy,
  ExecutionLevel(..), ExecutionPolicy(..),
  mkAccessPolicy,
  canAccess,
  mkExectionPolicy,
  canExecute,
  defaultQueryAccessPolicy,
  defaultQueryExecutionPolicy,
  defaultDataSourceAccessPolicy,
  defaultDataSourceExecutionPolicy
) where

import Data.Semigroup (Max(..))
import Data.List (intercalate)

data Role = Public | Contributor | Owner
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data AccessLevel = None | View | Edit
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
data ExecutionLevel = NoExcec | Execute
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- Utility
mkPolicy :: (Ord r, Bounded l) => ((r -> Max l) -> p) -> r -> l -> p
mkPolicy c r l = c (Max . pol) where
  pol r'
    | r' >= r   = l
    | otherwise = minBound

-- Utility
describePolicy :: (Show l, Enum l, Bounded l) => (Role -> l -> p -> Bool) -> p -> [String]
describePolicy can policy = map (\ r ->
    show r ++ " " ++ describe (span (\l -> can r l policy) (drop 1 $ enumFrom minBound))
  )
  (enumFrom Public)
  where
    describe ([], _) = "has no permissions"
    describe (_, []) = "has all permissions"
    describe (xs, _) = "has " ++ intercalate " and " (map show xs) ++ " permission" ++ (if length xs > 1 then "s" else "")


newtype AccessPolicy = AccessPolicy (Role -> Max AccessLevel) deriving (Monoid)

mkAccessPolicy :: Role -> AccessLevel -> AccessPolicy
mkAccessPolicy = mkPolicy AccessPolicy

canAccess :: Role -> AccessLevel -> AccessPolicy -> Bool
(r `canAccess` l) (AccessPolicy f) = getMax (f r) >= l

describeAccessPolicy :: AccessPolicy -> [String]
describeAccessPolicy = describePolicy canAccess


newtype ExecutionPolicy = ExecutionPolicy (Role -> Max ExecutionLevel) deriving (Monoid)

mkExectionPolicy :: Role -> ExecutionLevel -> ExecutionPolicy
mkExectionPolicy = mkPolicy ExecutionPolicy

canExecute :: Role -> ExecutionLevel -> ExecutionPolicy -> Bool
(r `canExecute` l) (ExecutionPolicy f) = getMax (f r) >= l

describeExecutionPolicy :: ExecutionPolicy -> [String]
describeExecutionPolicy = describePolicy canExecute


defaultQueryAccessPolicy :: AccessPolicy
defaultQueryAccessPolicy = mkAccessPolicy Contributor Edit

defaultQueryExecutionPolicy :: ExecutionPolicy
defaultQueryExecutionPolicy = mkExectionPolicy Contributor Execute

defaultDataSourceAccessPolicy :: AccessPolicy
defaultDataSourceAccessPolicy = mkAccessPolicy Contributor View `mappend` mkAccessPolicy Owner Edit

defaultDataSourceExecutionPolicy :: ExecutionPolicy
defaultDataSourceExecutionPolicy = mkExectionPolicy Contributor Execute
