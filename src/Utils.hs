module Utils ( if' ) where

-- just to make it available everywhere
if' :: Bool -> t -> t -> t
if' True t _ = t
if' False _ f = f
