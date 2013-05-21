{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.ProjectableExtended (
  ProjectableFlattenMaybe (flatten),

  (!), (!?), (!??)
  )  where

import Database.Record (PersistableWidth)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable (Projectable(project), ProjectableMaybe (flattenMaybe))
import Database.Relational.Query.Pi (Pi)

class ProjectableFlattenMaybe a b where
  flatten :: ProjectableMaybe p => p a -> p b

instance ProjectableFlattenMaybe (Maybe a) b
         => ProjectableFlattenMaybe (Maybe (Maybe a)) b where
  flatten = flatten . flattenMaybe

instance ProjectableFlattenMaybe (Maybe a) (Maybe a) where
  flatten = id


(!) :: (PersistableWidth b, Projectable p) => Projection a -> Pi a b -> p b
p ! pi' = project $ Projection.pi p pi'

(!?) :: (PersistableWidth b, Projectable p)
        => Projection (Maybe a) -> Pi a b -> p (Maybe b)
p !? pi' = project $ Projection.piMaybe p pi'

(!??) :: (PersistableWidth b, Projectable p, ProjectableMaybe p,
         ProjectableFlattenMaybe c (Maybe a))
        => Projection c -> Pi a b -> p (Maybe b)
p !?? pi' = project $ Projection.piMaybe (flatten p) pi'

infixl 8 !, !?, !??
