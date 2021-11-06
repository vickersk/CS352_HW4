use std::fmt;

/// A record representing a grocery item
pub struct GroceryItem {
    /// inventory code
    item_code: u16,
    /// human-readable name
    name: String,
    /// price per unit, in dollars
    unit_price: f64,
    /// quantity of item purchased
    quantity: u16
}

/// Print a GroceryItem
impl fmt::Display for GroceryItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} {} {} {}]", 
            self.item_code, self.name, self.unit_price, 
            self.quantity)
    }
}

/// Parsing error
pub enum ParseError {
    /// Invalid course number (value is unparsed string)
    InvalidItemCode(String),
    /// Invalid price (value is unparsed string)
    InvalidPrice(String),
    /// Wrong number of fields (value is number of fields provided)
    WrongFieldCount(usize)
}

/// Convert an input string in the following format to a GroceryItem:
///   ItemCode ItemName UnitPrice
///
/// The fields are subject to the following constraints:
///   * ItemCode is an integer value
///   * ItemName is a single whitespace-delimited token
///   * UnitPrice is a floating-point value
///   * There are exactly three whitespace-separated tokens
///   * The GroceryItem's quantity will be 1
///
/// Returns Ok(item) on a successful match, Err(err) on failure.
pub fn read_item(line: &str) -> Result<GroceryItem, ParseError> {
    unimplemented!();
}

/// Gets the department name for a given item code
/// 
/// = Code = Department =
///   1xxx   Grocery
///   2xxx   Produce
///   3xxx   Bakery
///   4xxx   Deli
///   5xxx   Dairy
///   6xxx   Meat
///   7xxx   Seafood
///   8xxx   Housewares
///   9xxx   Electronics
///
/// Returns None for non-four-digit item codes
pub fn get_department(item_code: u16) -> Option<&'static str> {
    unimplemented!();
}