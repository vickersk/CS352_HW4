#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

/// A record representing a grocery item
struct GroceryItem {
    int item_code;      ///< inventory code
    std::string name;   ///< human-readable name
    double unit_price;  ///< price per unit, in dollars
    int quantity;       ///< quantity of item purchased
};

/// Print a GroceryItem
std::ostream& operator<< (std::ostream& out, const GroceryItem& g) {
    return out 
        << "[" << g.item_code << " " << g.name 
        << " " << g.unit_price << " " << g.quantity << "]";
}

/// Base class for parsing errors
class ParseError {
public:
    /// Ensure subclasses are properly destroyed
    virtual ~ParseError() = default;
};

/// An error representing an invalid item code
struct InvalidItemCode : public ParseError {
    std::string item_code;  ///< the item code that could not be parsed

    InvalidItemCode(const std::string& s) : item_code(s) {}
};

/// An error representing an invalid price
struct InvalidPrice : public ParseError {
    std::string unit_price;  ///< the price that could not be parsed

    InvalidPrice(const std::string& s) : unit_price(s) {}
};

/// An error representing the wrong number of fields
struct WrongFieldCount : public ParseError {
    int num_fields;  ///< the number of fields present

    WrongFieldCount(int n) : num_fields(n) {}
};

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
/// Returns the GroceryItem on a successful match, throws an appropriate
/// ParseError on failure.
GroceryItem read_item(const std::string& line) {
    assert(!"unimplemented");
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
/// Throws a NoSuchDepartment error for non-four-digit item codes
std::string get_department(int item_code) {
    assert(!"unimplemented");
}