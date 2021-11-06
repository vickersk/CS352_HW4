#include "hw4.hpp"

#include <fstream>
#include <iostream>
#include <string>

/// Runs read_item on each lne of the file, printing the results
void read_all_items(std::istream& in, std::ostream& out) {
    std::string line;
    while ( std::getline(in, line) ) {
        try {
            GroceryItem item = read_item(line);
            out << item << std::endl;
        } catch (const WrongFieldCount& e) {
            out << "Wrong number of fields: " << e.num_fields << std::endl;
        } catch (const InvalidItemCode& e) {
            out << "Invalid item code: " << e.item_code << std::endl;
        } catch (const InvalidPrice& e) {
            out << "Invalid price: " << e.unit_price << std::endl;
        }
    }
}

int main(int argc, char** argv) {
    switch(argc) {
        case 1: {
            read_all_items(std::cin, std::cout);
            break;
        }
        case 2: {
            std::ifstream in{argv[1]};
            read_all_items(in, std::cout);
            break;
        }
        case 3: {
            std::ifstream in{argv[1]};
            std::ofstream out{argv[2]};
            read_all_items(in, out);
            break;
        }
        default: {
            std::cout << "usage: " << argv[0] << " [infile [outfile]]" << std::endl;
            break;
        }
    }
}