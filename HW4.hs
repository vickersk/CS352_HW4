module HW4(readItem, getDepartment) where

-- A record representing a grocery item
-- Contains item code, item name, unit price, and quantity
type GroceryItem = (Int,String,Double,Int)

-- Parsing error
data ParseError = 
    InvalidItemCode String -- Invalid course number, string provided
    | InvalidPrice String  -- Invalid price, string provided
    | WrongFieldCount Int  -- Wrong number of fields, count provided
    deriving Show

-- Convert an input string in the following format to a GroceryItem:
--   ItemCode ItemName UnitPrice
--
-- The fields are subject to the following constraints:
--   * ItemCode is an integer value
--   * ItemName is a single whitespace-delimited token
--   * UnitPrice is a floating-point value
--   * There are exactly three whitespace-separated tokens
--   * The GroceryItem's quantity will be 1
--
-- Returns Right item on a successful match, Left err on failure.
readItem :: String -> Either ParseError GroceryItem
readItem line =
    error "unimplemented"

-- Gets the department name for a given item code
-- 
-- = Code = Department =
--   1xxx   Grocery
--   2xxx   Produce
--   3xxx   Bakery
--   4xxx   Deli
--   5xxx   Dairy
--   6xxx   Meat
--   7xxx   Seafood
--   8xxx   Housewares
--   9xxx   Electronics
--
-- Returns Nothing for non-four-digit item codes
getDepartment :: Int -> Maybe String
getDepartment itemCode =
    error "unimplemented"