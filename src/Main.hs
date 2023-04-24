import Helpers
import Item

import Data.Functor
import Text.Read
import Text.Regex.TDFA

validRegex :: String
validRegex = "^(0|-?[1-9][0-9]*|[A-Za-z][0-9A-Z_a-z]*)$"

isValidString :: String -> Bool
isValidString str = str =~ validRegex

insertBefore :: String -> Item String -> Bool
insertBefore value' item = case (readMaybe value' :: Maybe Integer, readMaybe (value item) :: Maybe Integer) of
    (Just x, Just y) -> x <= y
    _ -> value' <= value item

valueEquals :: Item String -> String -> Bool
valueEquals item value' = value item == value'

update :: Bool -> Node String -> IO ()
update begin start = (if begin then return () else putStrLn "") >> putStrLn "Awaiting input..." >> getLine >>= handleStr
    where
        handleStr input = case length' of
            0 -> putStrLn "\nProgram terminated!" $> removeAll start >> return ()
            _ -> case input of
                '~' : substr -> case length' of
                    1 -> putStrLn "\nDeleting list..." >> update' (removeAll start)
                    _ -> if isValidString substr then putStrLn "\nRemoving item..." >> (update' =<< removeItem start substr valueEquals) else parseFail ()
                _ -> case input of
                    "l" -> putStrLn "\nLoop print not implemented!" >> update' start
                    "i" -> printList "\nIterator print..." printIterator
                    "a" -> printList "\nArray print..." printArray
                    "r" -> printList "\nRecursive print..." printRecursive
                    "f" -> printList "\nFold print..." printFold
                    "b" -> printList "\nFoldback print..." printFoldback
                    _ -> if isValidString input then putStrLn "\nInserting item..." >> (update' =<< insertItem start input insertBefore) else parseFail ()
                    where
                        printList str fPrint = putStrLn str >> fPrint start >> update' start
                where
                    update' = update False

                    parseFail _ = putStrLn "\nCould not parse input!" >> update' start
            where
                length' = length input

main :: IO ()
main = update True Nothing
