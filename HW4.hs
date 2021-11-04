module HW4(parseRecord) where

-- A record representing a student's course performance
-- Contains Subject, CourseNum, Section, NumCredits, "LastName, FirstName", Gpa
type ClassRecord = (String,Int,String,Int,String,Double)

-- Parse error
data ParseError = 
    WrongFieldCount Int         -- wrong count of fields, count provided
    | InvalidCourseNum String   -- invalid course number, string provided
    | InvalidNumCredits String  -- invalid number of credits, string provided
    | InvalidGrade String       -- invalid grade, string provided
    deriving Show

-- Converts a letter grade to the appropriate GPA
-- A = 4.0, B = 3.0, C = 2.0, D = 1.0, F = 0.0
-- All except F can be followed with + or - for +/- 0.3
-- Return Just grade on success, Nothing on error
letterToGpa :: String -> Maybe Double
letterToGpa letter = 
    -- Temporary code, should be replaced
    Nothing

-- Convert an input string in the following format to a ClassRecord tuple:
--   Subject CourseNum Section NumCredits FirstName LastName GradeLetter
-- 
-- The fields are subject to the following constraints:
--   * CourseNum and NumCredits are integer values
--   * GradeLetter is a valid grade according to the following grammar:
--       [A-D]('+'|'-')? | 'F'
--   * There are exactly seven whitespace-separated fields
-- 
-- Returns the ClassRecord wrapped in Right on successful match, 
-- an appropriate ParseError wrapped in Left otherwise.
parseRecord :: String -> Either ParseError ClassRecord
parseRecord line =
    -- Temporary code, should be replaced
    Left (InvalidGrade "unimplemented")