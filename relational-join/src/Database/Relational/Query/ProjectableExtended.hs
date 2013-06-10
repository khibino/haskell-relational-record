{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Database.Relational.Query.ProjectableExtended
-- Copyright   : 2013 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines operators on various polymorphic projections
-- which needs extended GHC features.
module Database.Relational.Query.ProjectableExtended (
  -- * Projection for nested 'Maybe's
  ProjectableFlattenMaybe (flatten),

  flattenPiMaybe,
  flattenPiMaybe',

  -- * Get narrower projections
  (!), (?!), (?!?), (!??),

  (<!>), (<?!>), (<?!?>), (<!??>),

  -- * Zipping projection type trick
  ProjectableGeneralizedZip (generalizedZip), (>?<)
  )  where

import Database.Record (PersistableWidth)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Aggregation (Aggregation)
import qualified Database.Relational.Query.Aggregation as Aggregation
import Database.Relational.Query.Projectable
  (Projectable(project), AggregateProjectable(projectAggregation),
   ProjectableMaybe (flattenMaybe), ProjectableZip(projectZip))
import Database.Relational.Query.Pi (Pi)


-- | Interface to compose phantom 'Maybe' nested type.
class ProjectableFlattenMaybe a b where
  flatten :: ProjectableMaybe p => p a -> p b

-- | Compose 'Maybe' type in projection phantom type.
instance ProjectableFlattenMaybe (Maybe a) b
         => ProjectableFlattenMaybe (Maybe (Maybe a)) b where
  flatten = flatten . flattenMaybe

-- | Not 'Maybe' type is not processed.
instance ProjectableFlattenMaybe (Maybe a) (Maybe a) where
  flatten = id


-- | Get narrower projection along with projection path
--   and project into result projection type.
(!) :: (PersistableWidth b, Projectable p)
    => Projection a -- ^ Source projection
    -> Pi a b       -- ^ Projection path
    -> p b          -- ^ Narrower projected object
p ! pi' = project $ Projection.pi p pi'

-- | Get narrower projection along with projection path
--   and project into result projection type.
--   'Maybe' phantom type is propagated.
(?!) :: (PersistableWidth b, Projectable p)
     => Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' type
     -> Pi a b               -- ^ Projection path
     -> p (Maybe b)          -- ^ Narrower projected object. 'Maybe' type result
p ?! pi' = project $ Projection.piMaybe p pi'

-- | Get narrower projection along with projection path
--   and project into result projection type.
--   'Maybe' phantom type is propagated. Projection path leaf is 'Maybe' case.
(?!?) :: (PersistableWidth b, Projectable p)
      => Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' phantom type
      -> Pi a (Maybe b)       -- ^ Projection path. 'Maybe' type leaf
      -> p (Maybe b)          -- ^ Narrower projected object. 'Maybe' phantom type result
p ?!? pi' = project $ Projection.piMaybe' p pi'

-- | Get narrower projection with flatten leaf phantom type along with projection path.
flattenPiMaybe :: (PersistableWidth b, ProjectableFlattenMaybe (Maybe b) c)
               => Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' phantom type
               -> Pi a b               -- ^ Projection path
               -> Projection c         -- ^ Narrower 'Projection'. Flatten 'Maybe' phantom type
flattenPiMaybe p = flatten . Projection.piMaybe p

-- | Get narrower projection with flatten leaf phantom type along with projection path
--   and project into result projection type.
(!??) :: (PersistableWidth b, ProjectableFlattenMaybe (Maybe b) c,
          Projectable p, ProjectableMaybe p)
       => Projection (Maybe a) -- ^ Source 'Projection'. 'Maybe' phantom type
       -> Pi a b               -- ^ Projection path.
       -> p c                  -- ^ Narrower flatten and projected object.
p !?? pi' = project $ flattenPiMaybe p pi'


-- | Get narrower aggregated projection along with projection path
--   and project into result projection type.
(<!>) :: (PersistableWidth b, AggregateProjectable p)
      => Aggregation a -- ^ Source 'Aggregation'
      -> Pi a b        -- ^ Projection path
      -> p b           -- ^ Narrower projected object
(<!>) a = projectAggregation . Aggregation.pi a

-- | Get narrower aggregated projection along with projection path
--   and project into result projection type.
--   'Maybe' phantom type is propagated.
(<?!>) :: (PersistableWidth b, AggregateProjectable p)
       => Aggregation (Maybe a) -- ^ Source 'Aggregation'. 'Maybe' phantom type
       -> Pi a b                -- ^ Projection path
       -> p (Maybe b)           -- ^ Narrower projected object. 'Maybe' phantom type result
(<?!>) a = projectAggregation . Aggregation.piMaybe a

-- | Get narrower aggregated projection along with projection path
--   and project into result projection type.
--   'Maybe' phantom type is propagated. Projection path leaf is 'Maybe' case.
(<?!?>) :: (PersistableWidth b, AggregateProjectable p)
        => Aggregation (Maybe a) -- ^ Source 'Aggregation'. 'Maybe' phantom type
        -> Pi a (Maybe b)        -- ^ Projection path. 'Maybe' type leaf
        -> p (Maybe b)           -- ^ Narrower projected object. 'Maybe' phantom type result
(<?!?>) a = projectAggregation . Aggregation.piMaybe' a

-- | Get narrower aggregated projection with flatten leaf phantom type along with projection path.
flattenPiMaybe' :: (PersistableWidth b, ProjectableFlattenMaybe (Maybe b) c)
                => Aggregation (Maybe a) -- ^ Source 'Aggregation'. 'Maybe' phantom type
                -> Pi a b                -- ^ Projection path
                -> Aggregation c         -- ^ Narrower 'Aggregation'. Flatten 'Maybe' phantom type
flattenPiMaybe' a = flatten . Aggregation.piMaybe a

-- | Get narrower aggregated projection with flatten leaf phantom type along with projection path
--   and project into result projection type.
(<!??>) :: (PersistableWidth b, ProjectableFlattenMaybe (Maybe b) c,
            AggregateProjectable p, ProjectableMaybe p)
        => Aggregation (Maybe a) -- ^ Source 'Aggregation'. 'Maybe' phantom type
        -> Pi a b                -- ^ Projection path
        -> p c                   -- ^ Narrower flatten and projected object.
a <!??> pi' = projectAggregation $ flattenPiMaybe' a pi'


-- | Interface for Zipping type trick.
class ProjectableGeneralizedZip a b c where
  generalizedZip :: ProjectableZip p => p a -> p b -> p c

-- | Zip right unit as zero width.
instance ProjectableGeneralizedZip a () a where
  generalizedZip = const

-- | Zip left unit as zero width.
instance ProjectableGeneralizedZip () a a where
  generalizedZip = const id

-- | Ordinary Zipping into tuple.
instance ProjectableGeneralizedZip a b (a, b) where
  generalizedZip = projectZip

-- | Binary operator the same as 'generalizedZip'.
(>?<) :: (ProjectableGeneralizedZip a b c, ProjectableZip p)
      => p a -> p b -> p c
(>?<) =  generalizedZip

infixl 8 !, ?!, ?!?, !??, <!>, <?!>, <?!?>, <!??>
infixl 1 >?<
