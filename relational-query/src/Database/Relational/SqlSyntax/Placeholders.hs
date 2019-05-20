module Database.Relational.SqlSyntax.Placeholders
  ( -- * Manipulate placeholders referred in the statement.
    withPlaceholderOffsets,
    placeholderOffsets,
    mapWithPlaceholderOffsets,
    detachPlaceholderOffsets,
    attachEmptyPlaceholderOffsets,
    emptyPlaceholderOffsets,
    sortByPlaceholderOffsets,

    -- ** Manipulate records with PlaceholderOffsets
    record,
    recordWidth,
    untypeRecord,
    placeholderOffsetsOfRecord,
    isPlaceholdersRecord,
    emptyPlaceholderOffsetsOfRecord,
    tupleFromPlaceholderOffsets,
    detachPlaceholderOffsetsOfRecord,
    untypeRecordWithPlaceholderOffsets,
    unsafeRecordFromTupleWithPlaceholderOffsets,
    typeFromRawColumns,

  ) where

import Control.Monad.Trans.Writer (writer, execWriter, runWriter, mapWriter)
import Data.Array (listArray, (!))
import Data.DList (toList)
import Data.Monoid (mempty)

import Database.Relational.Internal.String (StringSQL)
import Database.Relational.SqlSyntax.Types
  (Record (Record), PlaceholderOffsets, WithPlaceholderOffsets, WithPlaceholderOffsetsT (WithPlaceholderOffsetsT),
   Tuple, TypedTuple (TypedTuple), Column (RawColumn ),
   toTypedTuple, untypeTuple, withPlaceholderOffsets, tupleWidth,)

mapWithPlaceholderOffsets
  :: ((a, PlaceholderOffsets) -> (b, PlaceholderOffsets)) -> WithPlaceholderOffsets a -> WithPlaceholderOffsets b
mapWithPlaceholderOffsets f (WithPlaceholderOffsetsT w) = WithPlaceholderOffsetsT $ mapWriter f w

placeholderOffsets :: WithPlaceholderOffsets a -> PlaceholderOffsets
placeholderOffsets (WithPlaceholderOffsetsT w) = execWriter w

detachPlaceholderOffsets :: WithPlaceholderOffsets a -> a
detachPlaceholderOffsets (WithPlaceholderOffsetsT w) = fst $ runWriter w

tupleFromPlaceholderOffsets :: WithPlaceholderOffsets a -> (a, PlaceholderOffsets)
tupleFromPlaceholderOffsets (WithPlaceholderOffsetsT w) = runWriter w

attachEmptyPlaceholderOffsets :: a -> WithPlaceholderOffsets a
attachEmptyPlaceholderOffsets x = WithPlaceholderOffsetsT $ writer (x, mempty)

emptyPlaceholderOffsets :: WithPlaceholderOffsets a -> WithPlaceholderOffsets a
emptyPlaceholderOffsets = attachEmptyPlaceholderOffsets . detachPlaceholderOffsets

isPlaceholdersRecord :: Record c r -> Bool
isPlaceholdersRecord = (/= mempty) . placeholderOffsets . toTypedTuple

placeholderOffsetsOfRecord :: Record c r -> PlaceholderOffsets
placeholderOffsetsOfRecord = placeholderOffsets . toTypedTuple

emptyPlaceholderOffsetsOfRecord :: Record c r -> Record c r
emptyPlaceholderOffsetsOfRecord =  Record . emptyPlaceholderOffsets . toTypedTuple

detachPlaceholderOffsetsOfRecord :: Record c r -> TypedTuple c r
detachPlaceholderOffsetsOfRecord = detachPlaceholderOffsets . toTypedTuple

sortByPlaceholderOffsets :: PlaceholderOffsets -> [a] -> [a]
sortByPlaceholderOffsets phos xs = map (ary !) $ toList phos
  where ary = listArray (0, length xs) xs

-- | Unsafely type 'Tuple' value to 'Record' type.
record :: PlaceholderOffsets -> Tuple -> Record c t
record phs = Record . withPlaceholderOffsets phs . TypedTuple

recordWidth :: Record c r -> Int
recordWidth = tupleWidth . untypeRecord

untypeRecord :: Record c r -> Tuple
untypeRecord = untypeTuple . detachPlaceholderOffsets . toTypedTuple

-- | Unsafely generate 'Record' from SQL string list.
typeFromRawColumns :: PlaceholderOffsets
                   -> [StringSQL] -- ^ SQL string list specifies columns
                   -> Record c r  -- ^ Result 'Record'
typeFromRawColumns phs = record phs . map RawColumn

untypeRecordWithPlaceholderOffsets :: Record c t -> WithPlaceholderOffsets Tuple
untypeRecordWithPlaceholderOffsets = fmap untypeTuple . toTypedTuple

unsafeRecordFromTupleWithPlaceholderOffsets :: WithPlaceholderOffsets Tuple -> Record c t
unsafeRecordFromTupleWithPlaceholderOffsets = Record . fmap TypedTuple
