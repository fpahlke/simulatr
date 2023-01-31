


test_that("For invalid input arguments 'createSeed' throws meaningful exceptions", {
    expect_error(createSeed(numberOfValues = 0), 
        "Assertion on 'numberOfValues' failed: Element 1 is not >= 1.")
    expect_error(createSeed(numberOfValues = 1, minValue = -1), 
        "Assertion on 'minValue' failed: Element 1 is not >= 1.")
    expect_error(createSeed(numberOfValues = 1, maxValue = 0), 
        "Assertion on 'maxValue' failed: Element 1 is not >= 1.")
    expect_error(createSeed(numberOfValues = 1, minValue = 101, maxValue = 100), 
        "Assertion on 'minValue < maxValue' failed: Must be TRUE.")
})

test_that("The results of 'createSeed' depend on the input arguments as expected", {
    expect_length(createSeed(), 1)
    expect_length(createSeed(numberOfValues = 9), 9)
    
    seed <- createSeed(numberOfValues = 100, minValue = 1000, maxValue = 5000)
    expect_true(all(seed >= 1000))
    expect_true(all(seed <= 5000))
})

test_that("'createSeed' returns valid seed although network connection is missing", {
    expect_warning(seed <- createSeed(
        minValue = 100000000, maxValue = 999999999,
        test_exception = "network error"
    ), "Failed to receive new seed from www.random.org: network error")
    
    expect_type(seed, "integer")
})