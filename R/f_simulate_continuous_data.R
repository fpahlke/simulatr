
#'
#' @title
#' Get Simluated Two Arm Means
#'
#' @description
#' Simluates and returns a normally distributed continuous dataset for two groups.
#'
#' @param n1 a single integer value. The sample size of group 1.
#' @param n2 a single integer value. The sample size of group 2.
#' @param mean1 a single numeric value. The assumed mean of group 1.
#' @param mean2 a single numeric value. The assumed mean of group 2.
#' @param sd1 a single numeric value. The assumed standard deviation of group 1.
#' @param sd2 a single numeric value. The assumed standard deviation of group 2.
#' @param seed a single integer value. The start value for generating pseudo-random numbers.
#' @param alternative a character string specifying the alternative hypothesis,
#'        must be one of "two.sided" (default), "greater" or "less".
#' @param ... ensures that all arguments (starting from the "...") are to be named.
#'
#' @details
#' If the \code{seed} is not specified (NA) the function \link{createSeed} will be used to create
#' a true random number as seed.
#'
#' See \url{https://cran.r-project.org/package=simstudy} for a comprehensive R package for study data simulation.
#'
#' @seealso \link{createSeed}
#' 
#' @return a list with all arguments and results; the ouput is defined as a class with name 'SimulationResult'.
#'
#' @examples
#' getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4, seed = 123)
#'
#' @export
#'
getSimulatedTwoArmMeans <- function(
        n1, 
        n2, 
        mean1, 
        mean2, 
        sd1, 
        sd2, 
        ...,
        seed = NA_integer_,
        alternative = c("two.sided", "less", "greater")) {
    
    # use assertions to check all input arguments
    checkmate::assertInt(n1, lower = 1)
    checkmate::assertInt(n2, lower = 1)
    checkmate::assertNumber(mean1)
    checkmate::assertNumber(mean2)
    checkmate::assertNumber(sd1)
    checkmate::assertNumber(sd2)
    checkmate::assertInt(seed, na.ok = TRUE)

    # create a new seed if it is NA, i.e., it is undefined
    if (is.na(seed)) {
        seed <- createSeed(...)
    }

    # specify seed
    if (!is.na(seed)) {
        set.seed(seed)
    }

    # create normal distributed random data for the two groups
    values1 <- rnorm(n = n1, mean = mean1, sd = sd1)
    values2 <- rnorm(n = n2, mean = mean2, sd = sd2)

    # save the fake data to a data frame in long format
    data <- data.frame(
        group = as.factor(c(rep(1, n1), rep(2, n2))),
        values = c(values1, values2)
    )

    # calcualate t-test result
    testResult <- t.test(values ~ group, data = data, alternative = match.arg(alternative))

    # put all arguments and results to a list
    result <- list(
        n1 = n1,
        n2 = n2,
        allocationRatio = n1 / n2,
        mean1 = mean1,
        mean2 = mean2,
        sd1 = sd1,
        sd2 = sd2,
        meanSim1 = mean(values1),
        meanSim2 = mean(values2),
        sdSim1 = sd(values1),
        sdSim2 = sd(values2),
        seed = seed,
        statistic = testResult$statistic[[1]],
        df = testResult$parameter[[1]],
        pValue = testResult$p.value,
        confInt = as.vector(testResult$conf.int),
        confLevel = attr(testResult$conf.int, "conf.level"),
        alternative = testResult$alternative,
        method = testResult$method,
        data = data
    )

    # define that the result list is a class with name 'SimulationResult'
    result <- structure(result, class = "SimulationResult")

    return(result)
}

#'
#' @title
#' Print Simulation Result
#'
#' @description
#' Generic function to print a \code{SimulationResult} object.
#'
#' @param x a \code{SimulationResult} object to print.
#' @param ... further arguments passed to or from other methods.
#' 
#' @examples
#' x <- getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4, seed = 123)
#' print(x)
#'
#' @export
#'
print.SimulationResult <- function(x, ...) {
    assertPackageIsInstalled("dplyr")
    checkmate::checkClass(x, "SimulationResult")

    # hide the output attr(,"class")
    attributes(x)[["class"]] <- NULL
    
    # format the confidence interval output
    x$confInt <- paste0("[", paste0(x$confInt, collapse = ", "), "]")
    
    # show the fake data as tibble
    x$data <- dplyr::tibble(x$data)
    
    print(x, ...)
}

#'
#' @title
#' Show Simulation Result
#'
#' @description
#' Generic function to show a \code{SimulationResult} object.
#'
#' @param x a \code{SimulationResult} object to show.
#' @param ... further arguments passed to or from other methods.
#'
#' @examples
#' getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4, seed = 123)
#' 
#' @export
#'
showDefault.SimulationResult <- function(x, ...) {
    print(x = x, ...)
}

#'
#' @title
#' Plot Simulation Result
#'
#' @description
#' Generic function to plot a \code{SimulationResult} object.
#'
#' @param x a \code{SimulationResult} object to plot.
#' @param main an overall title for the plot.
#' @param xlab a title for the x axis.
#' @param ylab a title for the y axis.
#' @param ... ensures that all arguments (starting from the "...") are to be named.
#'
#' @details
#' Uses ggplot2 to create the plot.
#'
#' @return
#' A ggplot2 object.
#'
#' @examples
#' x <- getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4, seed = 123)
#' if (require(ggplot2)) plot(x)
#'
#' @importFrom rlang .data
#' 
#' @export
#'
plot.SimulationResult <- function(
        x, 
        ..., 
        main = "Continuous Fake Data", 
        xlab = "Group", 
        ylab = "Simulated Values") {
        
    assertPackageIsInstalled("ggplot2")
    checkmate::checkClass(x, "SimulationResult")

    # see vignette("ggplot2-in-packages")
    data <- x$data
    p <- ggplot2::ggplot(
        data = data,
        mapping = ggplot2::aes(x = .data$group, y = .data$values)
    )
    p <- p + ggplot2::geom_boxplot(data = data, mapping = ggplot2::aes(fill = .data$group))
    p <- p + ggplot2::geom_point(
        colour = "#0e414e", shape = 20,
        position = ggplot2::position_jitter(width = .1),
        size = 2
    )
    p <- p + ggplot2::stat_summary(
        fun = "mean", geom = "point",
        shape = 21, position = ggplot2::position_dodge(.75), size = 4, fill = "white",
        colour = "black", show.legend = FALSE
    )
    p <- p + ggplot2::theme_bw()
    p <- p + ggplot2::theme(
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(colour = "black")
    )
    p <- p + ggplot2::ggtitle(main)
    p <- p + ggplot2::xlab(xlab)
    p <- p + ggplot2::ylab(ylab)
    p <- p + ggplot2::theme(legend.position = "none")
    return(p)
}
