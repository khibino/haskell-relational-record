module Text.Printer.List
       ( PrintM, Printer, execPrinter
       , token, list
       ) where

import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.DList (DList)
import qualified Data.DList as DList


type PrintM t = Writer (DList t)

type Printer t a = a -> PrintM t ()

token :: Printer t t
token = tell . return

list :: Printer t [t]
list = mapM_ token

execPrinter :: Printer t a -> a -> [t]
execPrinter p = DList.toList . execWriter . p
