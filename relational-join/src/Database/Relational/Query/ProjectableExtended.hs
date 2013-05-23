{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Database.Relational.Query.ProjectableExtended (
  ProjectableFlattenMaybe (flatten),
  
  piMaybeFlatten,

  (!), (!?), (!??),

  ProjectableGeneralizedZip (generalizedZip), (>?<)
  )  where

import Database.Record (PersistableWidth)

import Database.Relational.Query.Projection (Projection)
import qualified Database.Relational.Query.Projection as Projection
import Database.Relational.Query.Projectable
  (Projectable(project), ProjectableMaybe (flattenMaybe), ProjectableZip(projectZip))
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

piMaybeFlatten :: (PersistableWidth b, ProjectableFlattenMaybe c (Maybe a))
               => Projection c -> Pi a b -> Projection (Maybe b)
piMaybeFlatten = Projection.piMaybe . flatten

(!??) :: (PersistableWidth b, ProjectableFlattenMaybe c (Maybe a),
          Projectable p, ProjectableMaybe p)
      => Projection c -> Pi a b -> p (Maybe b)
p !?? pi' = project $ piMaybeFlatten p pi'

class ProjectableGeneralizedZip a b c where
  generalizedZip :: ProjectableZip p => p a -> p b -> p c

instance ProjectableGeneralizedZip a () a where
  generalizedZip = const

instance ProjectableGeneralizedZip () a a where
  generalizedZip = const id

instance ProjectableGeneralizedZip a b (a, b) where
  generalizedZip = projectZip

(>?<) :: (ProjectableGeneralizedZip a b c, ProjectableZip p)
      => p a -> p b -> p c
(>?<) =  generalizedZip

infixl 8 !, !?, !??
infixl 1 >?<
