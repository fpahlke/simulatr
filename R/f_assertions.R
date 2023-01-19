

assertPackageIsInstalled <- function(packageName) {
    if (!requireNamespace(packageName, quietly = TRUE)) {
        stop("Package ", sQuote(packageName), " is needed for this function to work")
    }
}

