### Composing relations (for LTS-9)

Copy the following to "helloworld.hs" for LTS-9:

{% highlight haskell %}
import Database.Relational.Query
import Data.Int (Int32)

hello :: Relation () (Int32, String)
hello = relation $ return (value 0 >< value "Hello")

world :: Relation () (Int32, String)
world = relation $ return (value 0 >< value "World!")

helloWorld :: Relation () (Int32, String, String)
helloWorld = relation $ do
    h <- query hello
    w <- query world
    on $ h ! fst' .=. w ! fst'
    return $ (,,) |$| h ! fst' |*| h ! snd' |*| w ! snd'

main :: IO ()
main = putStrLn $ show helloWorld ++ ";"
{% endhighlight %}
