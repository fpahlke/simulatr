

test_that("'assertPackageIsInstalled' works as expected", {
    expect_no_error(assertPackageIsInstalled("stats"))
    expect_error(assertPackageIsInstalled("notExistingPackage"),
        "Package 'notExistingPackage' is needed for this function to work")
})
