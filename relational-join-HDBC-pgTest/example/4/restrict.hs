
import Database.Relational.Query
import SetA
import Data.Int (Int32)

banana :: Restriction () SetA
banana =  restriction $
  \a -> do wheres $ a ! name' .=. value "Banana"

deleteBanana :: Delete ()
deleteBanana =
  typedDelete tableOfSetA banana

updateBanana :: Update (SetA, ())
updateBanana =
  typedUpdateAllColumn tableOfSetA banana

updateBanana2 :: Update String
updateBanana2 =
  targetUpdate tableOfSetA $
  \ta pa -> do (ph', ()) <- placeholder ( \ph -> ta !# name' <-# ph )
               wheres $ pa ! name' .=. value "Banana"
               return ph'

updateBanana3 :: Update SetA
updateBanana3 =
  targetUpdate tableOfSetA $
  \ta pa -> do (ph', ()) <- placeholder ( \ph -> ta !# id' <-# ph )
               wheres $ pa ! name' .=. value "Banana"
               return ph'

keyUpdateBanana :: KeyUpdate Int32 SetA
keyUpdateBanana =  typedKeyUpdate tableOfSetA seq'

main :: IO ()
main =  do
  print deleteBanana
  print updateBanana
  print updateBanana2
  print updateBanana3
