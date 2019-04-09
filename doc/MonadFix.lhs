> import Data.Maybe

In dealing with =slang= comlier, a concept came to me about monadFix.
It's used in codeBuild process, when I need to implement the recursive fucntion by llvm-hs.
Although it seems work just right, but really confused me a lot.

So I speed some time digging into it.

First, when not in monad, it looks like this:

> fix :: (a -> a) -> a
> fix f = f (fix f)

it means fix-point of the specified function. (and when it's not fix, it will loop forever)

what about in monad context. we could first look in =maybe=.

> maybeFix :: (a -> Maybe a) -> Maybe a
> maybeFix f = ma where ma = f (fromJust ma)

TODO: 
