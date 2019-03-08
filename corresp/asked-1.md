A few things that might be broken in the blog series

I just discovered your posts about recursion schemes. They are absolutely wonderful. Thank you so much.

I'm not sure whether the following are all in fact problems with the blog; it could be I just missed something.


# The `cata` laws

In [part 2](https://blog.sumtypeofway.com/recursion-schemes-part-2/) I'm confused by the laws. The first is stated as an equation: `cata In = id`. That's clear. But the second is stated as an implication:

```
-- given alg :: f a -> a
-- and func  :: f a -> f
cata (alg >>> fmap func) =>
   (cata alg) >>> func
```

Is that `=>` symbol supposed to be `=`?

The third law's statement is even stranger:

```
-- given alg  :: f a -> a
-- and func :: f a -> g a
cata (f >>> In) >>> cata g
   ==> cata (f >>> g)
```

Is that `==>` symbol again supposed to be `=`? And why does it stipulate types for `alg` and `func`, without ever using them?


# Generalizing `cata` using `para`

This happens in section 3 of the blog series.
The code provided is this:
```
cata' :: Functor f => Algebra f a -> Term f -> a
cata' = para'' (const f)
```
But f is not defined, except as a type constraint.


# Section II appears not to talk about ana

Section III claims Section II defined ana: "In the previous post, we defined ana, the anamorphism ...".  Section 1 makes a similar claim. I'm not finding it.
