use std::fmt;

/// A record representing a grocery item
pub struct GroceryItem {
    /// inventory code
    pub item_code: u16,
    /// human-readable name
    pub name: String,
    /// price per unit, in dollars
    pub unit_price: f64,
    /// quantity of item purchased
    pub quantity: u16
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

    // Splits the line on whitespace
    let mut tokens = line.split_whitespace();

    // Stores the item code for creating the GroceryItem
    let code_value : u16;

    // Extracts the first token as the item code
    if let Some(item_code) = tokens.next() {

        // Tries to convert the string to an integer
        code_value = match item_code.trim().parse() {
            Ok(val) => { val },
            Err(_) => {
                // Throws an InvalidItemCode error if the token
                // can't be converted to an integer
                return Err(ParseError::InvalidItemCode(item_code.to_string()))
            }
        };
    
    // Throws a WrongFieldCount exception if there is no token
    } else {
        return Err(ParseError::WrongFieldCount(0))
    }

    // Stores the name for creating the GroceryItem
    let item_name;

    // Extracts the second token as the item name
    if let Some(name) = tokens.next() {
        item_name = name.to_string();

    // Throws a WrongFieldCount exception if there is no token
    } else {
        return Err(ParseError::WrongFieldCount(1))
    }
    
    // Stores the price for creating the GroceryItem
    let price_value : f64;

    // Extracts the third token as the price
    if let Some(price) = tokens.next() {
    
        // Tries to convert the string to a floating point
        price_value = match price.trim().parse() {
            Ok(val) => { val },
            Err(_) => {
                // Throws an InvalidPrice error if the token
                // can't be converted to a floating point number
                return Err(ParseError::InvalidPrice(price.to_string()))
            }
        };
    
        // Throws an InvalidPrice exception if the price
        // is a negative floating point value
        if price_value < 0.0 {
            return Err(ParseError::InvalidPrice(price.to_string()))
        }
    
    // Throws a WrongFieldCount exception if there is no token
    } else {
        return Err(ParseError::WrongFieldCount(2))
    }

    // Throws a WrongFieldCount exception if there is more than three tokens
    if let Some(_) = tokens.next() {

        let mut extra_tokens = 4;

        // Checks if there are additional tokens
        while let Some(_) = tokens.next() {
            extra_tokens += 1;
        }

        return Err(ParseError::WrongFieldCount(extra_tokens))

    // If there are no additional tokens, it returns the GroceryItem
    // with the parsed tokens and 1 for the quantity
    } else {
        return Ok(GroceryItem {
            item_code: code_value,
            name: item_name,
            unit_price: price_value,
            quantity: 1
        })
    }
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

    // The position of the department number in the item code
    const DEPARTMENT_POS: &'static u16 = &1000;

    // Gets the department number
    let department_num = item_code / DEPARTMENT_POS;

    // Looks up the department number
    match department_num {
        1 => return Some("Grocery"),
        2 => return Some("Produce"),
        3 => return Some("Bakery"),
        4 => return Some("Deli"),
        5 => return Some("Dariy"),
        6 => return Some("Meat"),
        7 => return Some("Seafood"),
        8 => return Some("Housewares"),
        9 => return Some("Electronics"),

        // Returns None if the department number doesn't
        // exist or if the item code is not four digits
        _ => return None
    }
}