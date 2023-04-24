{-# LANGUAGE FlexibleInstances #-}

module ToString where

class ToString t where
    toString :: t -> String

instance ToString String where
    toString str = str
