module Item where
import ToString

import Data.Functor
import Data.Maybe

type Node t = Maybe (Item t)
type Acc t a r = Item t -> a -> r

data Item t = Item { value :: t, next :: Node t }

(!!) :: Item t -> Int -> Node t
item !! index = atIndex (Just item) index
    where
        atIndex Nothing _ = Nothing
        atIndex item' 0 = item'
        atIndex (Just item') index' = atIndex (next item') (index' - 1)

printGetNext :: ToString t => Item t -> IO (Node t)
printGetNext item = putStr (toString (value item) ++ if isNothing (next item) then "\n" else ", ") $> next item

itemIterator :: Item t -> [Item t]
itemIterator item = case next item of
    Nothing -> [item]
    Just next' -> item : itemIterator next'

itemFold :: (Item t -> Acc t a a) -> Acc t a r -> (a -> r) -> a -> Node t -> r
itemFold fSome fLast fEmpty accumulator item = case item of
    Nothing -> fEmpty accumulator
    Just item' -> case next' of
        Nothing -> fLast item' accumulator
        Just next'' -> itemFold fSome fLast fEmpty (fSome item' next'' accumulator) next'
        where
            next' = next item'

itemFold' :: (Item t -> Acc t a a) -> Acc t a r -> (Item t -> Item t -> Acc t a a) -> (Item t -> Acc t a r) -> (a -> r) -> a -> Node t -> Node t -> r
itemFold' fStart fOnly fMiddle fLast fEmpty accumulator previous current = case current of
    Nothing -> fEmpty accumulator
    Just current' -> case next' of
        Nothing -> case previous of
            Nothing -> fOnly current' accumulator
            Just previous' -> fLast previous' current' accumulator
        Just next'' -> itemFold' fStart fOnly fMiddle fLast fEmpty newAccumulator current next'
            where
                newAccumulator = case previous of
                    Nothing -> fStart current' next'' accumulator
                    Just previous' -> fMiddle previous' current' next'' accumulator
        where
            next' = next current'

itemFoldback :: (Item t -> Acc t a a) -> (Item t -> a) -> (() -> a) -> (a -> r) -> Node t -> r
itemFoldback fSome fLast fEmpty generator item = case item of
    Nothing -> generator (fEmpty ())
    Just item' -> case next' of
        Nothing -> generator (fLast item')
        Just next'' -> itemFoldback fSome fLast fEmpty (generator . fSome item' next'') next'
        where
            next' = next item'

itemFoldback' :: (Item t -> Acc t a a) -> (Item t -> a) -> (Item t -> Item t -> Acc t a a) -> (Item t -> Item t -> a) -> (() -> a) -> (a -> r) -> Node t -> Node t -> r
itemFoldback' fStart fOnly fMiddle fLast fEmpty generator previous current = case current of
    Nothing -> generator (fEmpty ())
    Just current' -> case next' of
        Nothing -> case previous of
            Nothing -> generator (fOnly current')
            Just previous' -> generator (fLast previous' current')
        Just next'' -> itemFoldback' fStart fOnly fMiddle fLast fEmpty (generator . newInner) current next'
            where
                newInner = case previous of
                    Nothing -> fStart current' next''
                    Just previous' -> fMiddle previous' current' next''
        where
            next' = next current'
