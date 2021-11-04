#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

/// A record representing a student's course performance
struct ClassRecord {
    std::string subject;       ///< Subject code
    int course_num;            ///< Course number
    std::string section;       ///< Section letter
    int num_credits;           ///< Number of course credits
    std::string student_name;  ///< Student's name
    double grade;              ///< Grade in course
};

/// Print a ClassRecord
std::ostream& operator<< (std::ostream& out, const ClassRecord& r) {
    return 
        out << "[" << r.subject << " " << r.course_num << " " << r.section << " " 
            << r.num_credits << " " << r.student_name << " " << r.grade << "]";
}

/// Base class for parsing errors
class ParseError {
public:
    /// Ensure subclasses are properly destroyed
    virtual ~ParseError() = default;
};

/// An error representing the wrong number of fields
struct WrongFieldCount : public ParseError {
    int num_fields;  // the number of fields present

    WrongFieldCount(int n) : num_fields(n) {}
};

/// An error representing an invalid course number
struct InvalidCourseNum : public ParseError {
    std::string course_num;  // the course number that could not be parsed

    InvalidCourseNum(const std::string& n) : course_num(n) {}
};

/// An error representing an invalid number of credits
struct InvalidNumCredits : public ParseError {
    std::string num_credits;  // the number of credits that could not be parsed

    InvalidNumCredits(const std::string& n) : num_credits(n) {}
};

/// An error representing an invalid grade
struct InvalidGrade : public ParseError {
    std::string grade;  // the grade that could not be parsed

    InvalidGrade(const std::string& g) : grade(g) {}
};

/// Converts a letter grade to the appropriate GPA
/// A = 4.0, B = 3.0, C = 2.0, D = 1.0, F = 0.0
/// All except F can be followed with + or - for +/- 0.3
/// Return the grade on success, throws InvalidGrade on failure
double letterToGpa(const std::string& sgrade) {
    assert(!"unimplemented");
}

/// Convert an input string in the following format to a ClassRecord:
///   Subject CourseNum Section NumCredits FirstName LastName GradeLetter
/// 
/// The fields are subject to the following constraints:
///   * CourseNum and NumCredits are integer values
///   * GradeLetter is a valid grade according to the following grammar:
///       [A-D]('+'|'-')? | 'F'
///   * There are exactly seven whitespace-separated fields
///
/// Returns the ClassRecord on successful match, throws an appropriate ParseError 
/// on failure.
ClassRecord parseRecord(const std::string& line) {
    assert(!"unimplemented");
}