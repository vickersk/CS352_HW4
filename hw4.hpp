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

// An error represetning a nonexistent department
struct NoSuchDepartment : public ParseError {
    int department; // the department that doesn't exist

    NoSuchDepartment(int n) : department(n) {}
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

    // Splits the line on whitespace
    std::istringstream tokens{line};

    // Extracts the first token as the item code
    std::string itemCode;
    int codeValue = -1;

    // Checks if there exists a token
    if (tokens >> itemCode) {

        // If so, it tries to convert the string to an integer
        try {
            codeValue = std::stoi(itemCode);

        // Throws an InvalidItemCode exception if the token
        // can't be converted to an integer
        } catch ( std::invalid_argument& e ) {
            throw InvalidItemCode{itemCode};
        }

    // Throws a WrongFieldCount exception if there is no token
    } else {
        throw WrongFieldCount{0};
    }
    
    // Extracts the second token as the item name
    std::string itemName;

    // Throws a WrongFieldCount exception if there is no token
    if (!(tokens >> itemName)) {
        throw WrongFieldCount{1};
    }

    // Extracts the third token as the price
    std::string price;
    double priceValue = -1;

    // Checks if there exists a token
    if (tokens >> price) {

        // If so, it tries to convert the string to a double
        try {
            priceValue = std::stod(price);

        // Throws an InvalidPrice exception if the token
        // can't be converted to a double
        } catch ( std::invalid_argument& e ) {
            throw InvalidPrice{price};
        }
        
        // Throws an InvalidPrice exception if the price
        // is a negative floating point value
        if (priceValue < 0) {
            throw InvalidPrice{price};
        }
    
    // Throws a WrongFieldCount Exception if there is no token
    } else {
        throw WrongFieldCount{2};
    }

    // Throws a WrongField Exception if there is more than three tokens
    std::string other;
    if ( tokens >> other ) {
        int extra_tokens = 3;
        
        while ( tokens >> other ) {
            extra_tokens++;
        }

        throw WrongFieldCount{extra_tokens};
    }

    // Returns a GroceryItem with the parsed tokens
    // and 1 for the quanitty
    return GroceryItem {
        codeValue,
        itemName,
        priceValue,
        1
    };
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

    // The position of the department number in the item code
    const static int DEPARTMENT_POS = 1000;

    // Gets the department number
    int department_num = item_code / DEPARTMENT_POS;

    // Looks up the department number
    switch(department_num) {
        case 1:
            return "Grocery";
        case 2:
            return "Produce";
        case 3:
            return "Bakery";
        case 4: 
            return "Deli";
        case 5:
            return "Dairy";
        case 6:
            return "Meat";
        case 7:
            return "Seafood";
        case 8:
            return "Housewares";
        case 9:
            return "Electronices";

        // Throws a NoSuchDepartment exception if the department number
        // doesn't exist or if the item code is not four digits
        default:
            throw NoSuchDepartment(department_num);
    }
}