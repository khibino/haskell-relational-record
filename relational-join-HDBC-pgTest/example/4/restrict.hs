
import Database.Relational.Query
import SetA

-- Restriction

banana :: Restriction () SetA
banana =
  restriction $
  \a -> do wheres $ a ! name' .=. value "Banana"

updateBanana :: Update () SetA
updateBanana =  restrictedUpdate tableOfSetA banana

deleteBanana :: Delete () a
deleteBanana =  restrictedDelete tableOfSetA banana

main :: IO ()
main =  print updateBanana >> print deleteBanana
