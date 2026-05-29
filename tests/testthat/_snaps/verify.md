# unit_tests() snapshot for defined tests

    Code
      print(unit_tests(sfm))
    Message
      
      -- Stock-and-Flow Unit Tests ---------------------------------------------------
      2 tests • 1/2 active • 0/2 include conditions
      * 1. S is non-negative
        `all(S >= 0)`
      ( ) 2. inactive test
        `FALSE`

# print.verify_sdbuildR() snapshot for passing tests

    Code
      print(result)
    Message
      
      -- Stock-and-Flow Unit Test Results --------------------------------------------
      2/2 tests passed.
      v 1. S is non-negative
      v 2. S starts at 100

# print.verify_sdbuildR() snapshot for failing FALSE tests

    Code
      print(result)
    Message
      
      -- Stock-and-Flow Unit Test Results --------------------------------------------
      0/1 test passed.
      x 1. S is zero
        Expected: TRUE Actual: FALSE

