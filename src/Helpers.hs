module Helpers where
import Item
import ToString

import Data.Functor

insertItem :: ToString t => Node t -> t -> (t -> Item t -> Bool) -> IO (Node t)
insertItem start value' insertBefore = putStrLn ("Creating item: " ++ toString value') $> Just (itemFoldback' fStart fOnly fMiddle fLast fEmpty id Nothing start)
    where
        insert item before after = if insertBefore value' item then before else after

        fStart current _ innerVal = insert current Item { value = value', next = Just current } Item { value = value current, next = Just innerVal }
        fOnly current = insert current Item { value = value', next = Just current } Item { value = value current, next = Just Item { value = value', next = Nothing } }
        fMiddle previous current next' innerVal = insert previous current (fStart current next' innerVal)
        fLast previous current = insert previous current (fOnly current)
        fEmpty _ = Item { value = value', next = Nothing }

removeItem :: ToString t => Node t -> t -> (Item t -> t -> Bool) -> IO (Node t)
removeItem start value' valueEquals = result <$ putStrLn removeStr
    where
        remove item remove' retain = if valueEquals item value' then remove' else retain

        fStart current next' innerVal = remove current (True, Just next') (fst innerVal, Just Item { value = value current, next = snd innerVal })
        fOnly current = remove current (True, Nothing) (False, Just current)
        fMiddle previous current next' innerVal = remove previous (True, Just current) (fStart current next' innerVal)
        fLast previous current = remove previous (True, Just current) (fOnly current)
        fEmpty _ = (False, Nothing)

        (removed, result) = itemFoldback' fStart fOnly fMiddle fLast fEmpty id Nothing start
        removeStr = if removed then "Removed item: " ++ toString value' else "Item " ++ toString value' ++ " does not exist!"

removeAll :: Node t -> Node t
removeAll _ = Nothing

printIterator :: ToString t => Node t -> IO ()
printIterator start = case start of
    Nothing -> return ()
    Just start' -> printEach (itemIterator start')
        where
           printEach [] = return ()
           printEach (x : xs) = printGetNext x >> printEach xs

printArray :: ToString t => Node t -> IO ()
printArray start = case start of
    Nothing -> return ()
    Just start' -> indexPrintGetNext 0
        where
            indexPrintGetNext index = case start' Item.!! index of
                Nothing -> return ()
                Just item -> printGetNext item >> indexPrintGetNext (index + 1)

printRecursive :: ToString t => Node t -> IO ()
printRecursive start = case start of
    Nothing -> return ()
    Just start' -> printGetNext start' >>= printRecursive

printFold :: ToString t => Node t -> IO ()
printFold start = putStr (itemFold fSome fLast fEmpty "" start)
    where
        fSome current _ accumulator = accumulator ++ toString (value current) ++ ", "
        fLast current accumulator = accumulator ++ toString (value current) ++ "\n"
        fEmpty accumulator = accumulator

printFoldback :: ToString t => Node t -> IO ()
printFoldback start = putStr (itemFoldback fSome fLast fEmpty id start)
    where
        fSome current _ innerVal = toString (value current) ++ ", " ++ innerVal
        fLast current = toString (value current) ++ "\n"
        fEmpty _ = ""
