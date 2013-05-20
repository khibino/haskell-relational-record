
module Database.Relational.Query.Internal.Context (
  Context,

  primContext, currentAliasId, product, restriction, orderByRev,
  nextAliasContext,

  updateProduct',
  updateRestriction',
  updateOrderBy'
  ) where

import Prelude hiding (product)

import Database.Relational.Query.AliasId.Unsafe (primAlias)
import Database.Relational.Query.AliasId (AliasId, newAliasId)

import Database.Relational.Query.Expr (Expr, showExpr)

import Database.Relational.Query.Product (QueryProduct)

import qualified Database.Relational.Query.Projectable as Projectable

import Database.Relational.Query.Relation (Order)


data Context = Context
               { currentAliasId :: AliasId
               , product :: Maybe QueryProduct
               , restriction :: Maybe (Expr Bool)
               , orderByRev :: [(Order, String)]
               }

primContext :: Context
primContext =  Context primAlias Nothing Nothing []

nextAliasContext :: Context -> Context
nextAliasContext s = s { currentAliasId =  newAliasId (currentAliasId s) }

updateProduct' :: (Maybe QueryProduct -> QueryProduct) -> Context -> Context
updateProduct' uf ctx =
  ctx { product = Just . uf . product $ ctx }

updateRestriction' :: Expr Bool -> Context -> Context
updateRestriction' e1 ctx =
  ctx { restriction = Just . uf . restriction $ ctx }
  where uf  Nothing = e1
        uf (Just e0) = e0 `Projectable.and` e1

updateOrderBy' :: Order -> Expr t -> Context -> Context
updateOrderBy' order e ctx =
  ctx { orderByRev = ((order, showExpr e) :) . orderByRev $ ctx  }
