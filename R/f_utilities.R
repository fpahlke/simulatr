
#'
#' @title
#' Create Seed
#'
#' @description
#' Returns one or more true random numbers which can be used as seed.
#'
#' @param numberOfValues a single integer value. Number of seeds to create, default is \code{1}.
#' @param minValue a single integer value. The minimum value that a seed can have, default is \code{1000000}.
#' @param maxValue a single integer value. The maximum value that a seed can have, default is \code{9999999}.
#'
#' @details
#' RANDOM.ORG offers true random numbers to anyone on the Internet.
#' The randomness comes from atmospheric noise, which for many purposes
#' is better than the pseudo-random number algorithms typically used
#' in computer programs. For more information see \url{https://www.random.org}.
#'
#' @seealso \link{getSimluatedTwoArmMeans}
#'
#' @return an integer value or vector containing one or more seeds.
#'
#' @export
#'
createSeed <- function(numberOfValues = 1, minValue = 1000000, maxValue = 9999999) {
    assertPackageIsInstalled("httr")
    assertPackageIsInstalled("glue")
    checkmate::assertInt(numberOfValues, lower = 1, upper = 1000)
    checkmate::assertInt(minValue, lower = 1, upper = 1e12)
    checkmate::assertInt(maxValue, lower = 1, upper = 1e12)
    checkmate::assertTRUE(minValue < maxValue)
    tryCatch(
        {
            minValue <- as.integer(minValue)
            maxValue <- as.integer(maxValue)
            url <- glue::glue(paste0(
                "https://www.random.org/integers/",
                "?num={numberOfValues}",
                "&min={minValue}",
                "&max={maxValue}",
                "&col=1",
                "&base=10",
                "&format=plain",
                "&rnd=new"
            ))

            response <- httr::GET(url)
            response <- httr::content(response)
            response <- trimws(response)
            response <- strsplit(response, "\n")
            response <- unlist(response)
            return(as.integer(response))
        },
        error = function(e) {
            warning("Failed to receive new seed from www.random.org: ", e$message)
            return(NA_integer_)
        }
    )
}
