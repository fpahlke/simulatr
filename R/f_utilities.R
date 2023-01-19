
#' 
#' @title 
#' Create Seed
#' 
#' @description
#' TODO
#' 
#' @param numberOfValues
#' @param minValue
#' @param maxValue
#' 
#' @details 
#' TODO
#' 
#' @return 
#' 
#' @export 
#' 
createSeed <- function(numberOfValues = 1, minValue = 1000000, maxValue = 9999999) {
    assertPackageIsInstalled("httr")
    assertPackageIsInstalled("glue")
    checkmate::assertInt(numberOfValues, lower = 1, upper = 1000)
    checkmate::assertInt(minValue, lower = 1, upper = 1e12)
    checkmate::assertInt(maxValue, lower = 1, upper = 1e12)
    checkmate::assert(minValue < maxValue, 
        .var.name = sprintf("minValue (%s) has to be smaller than maxValue (%s)", minValue, maxValue))
    tryCatch(
        {
            minValue <- as.integer(minValue)
            maxValue <- as.integer(maxValue)
            url <- glue::glue(paste0("https://www.random.org/integers/",
                "?num={numberOfValues}",
                "&min={minValue}",
                "&max={maxValue}",
                "&col=1",
                "&base=10",
                "&format=plain",
                "&rnd=new"))
            
            if (requireNamespace(magrittr, quietly = TRUE)) {
                response <- httr::GET(url) %>%
                    httr::content() %>%
                    trimws() %>%
                    strsplit(split = "\n") %>%
                    unlist() %>%
                    as.integer()
                return(response)
            }

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
