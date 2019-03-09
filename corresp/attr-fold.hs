An elegant way to fold over the history involved in a histomorphism?
---

I just [learned about histomorphisms](https://blog.sumtypeofway.com/recursion-schemes-part-iv-time-is-of-the-essence/). They rely on a couple helper types:

```
newtype Term f = In { out :: f (Term f) }
data Attr f a = Attr { attribute :: a
                     , hole      :: f (Attr f a) }
type CVAlgebra f a = f (Attr f a) -> a
histo :: forall a f. Functor f => CVAlgebra f a -> Term f -> a
```

I tried them out using the simplest novel data structure I could think of:

```
data Abc = A | B Abc | C Abc Abc
  deriving (Eq, Show, Ord)

data AbcF a = AF | BF a | CF a a
  deriving (Show, Eq, Ord, Functor)
```

(I could have omitted C, but that would have been isomorphic to Maybe. Omitting B gives something isomorphic to a binary tree.)

To use `histo` I had to write a `CVAlgebra`, as defined above. Here is one, from `AbcF` to `Int`. It counts the number of `B`s and `C`s, with the wrinkle that when processing a `B`, if the history of `Attr`s leading there contains a 2, a 100 point bonus is awarded. (Without that complicating wrinkle, one could use a catamorphism (a.k.a. a fold) instead of a histomorphism.)

```
forHisto :: CVAlgebra AbcF Int
forHisto abc = case abc of
  AF     -> 0
  BF a   -> 1 + attribute a
            + (if history_has_a_2 a then 100 else 0)
  CF a b -> 1 + attribute a + attribute b
  where
    history_has_a_2 :: Attr AbcF Int -> Bool
    history_has_a_2 (Attr 2 _)        = True
    history_has_a_2 (Attr _ AF)       = False
    history_has_a_2 (Attr _ (BF i))   = history_has_a_2 i
    history_has_a_2 (Attr _ (CF i j)) = history_has_a_2 i || history_has_a_2 j
```

So, for instance,

```
histo forHisto $           In $ BF $ In $ BF $ In AF -- equals 2
histo forHisto $ In $ BF $ In $ BF $ In $ BF $ In AF -- equals 103
```

It feels like there might be a better way to write `history_has_a_2`. The last two clauses are just handling recursion; it feels like they ought to be factored out. I know how to factor recursion out of a simple type like Abc, by creating a `Functor` like AbcF. `Attr f a`, though, is already a `Functor` (assuming `f` is).
