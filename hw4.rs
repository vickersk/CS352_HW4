use std::fmt;

/// A record representing a student's course performance
pub struct ClassRecord {
    /// Subject code
    subject: String,
    /// Course number
    course_num: u16,
    /// Section letter
    section: String,
    /// Number of course credits
    num_credits: u8,     
    /// Student's name  
    student_name: String,
    /// Grade in course
    grade: f32
}

// Print a ClassRecord
impl fmt::Display for ClassRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{} {} {} {} {} {}]", 
            self.subject, self.course_num, self.section, self.num_credits, 
            self.student_name, self.grade)
    }
}

/// Parsing error
pub enum ParseError {
    /// Wrong number of fields (value is number of fields provided)
    WrongFieldCount(i32),
    /// Invalid course number (value is unparsed string)
    InvalidCourseNum(String),
    /// Invalid number of credits (value is unparsed string)
    InvalidNumCredits(String),
    /// Invalid grade (value is unparsed string)
    InvalidGrade(String)
}

/// Converts a letter grade to the appropriate GPA
/// A = 4.0, B = 3.0, C = 2.0, D = 1.0, F = 0.0
/// All except F can be followed with + or - for +/- 0.3
/// Return Some(grade) on success, None on failure
fn letter_to_gpa(sgrade: &str) -> Option<f32> {
    unimplemented!();
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
/// Returns Ok(record) on successful match, Err(err) on failure.
pub fn parse_record(line: &str) -> Result<ClassRecord,ParseError> {
    unimplemented!();
}
