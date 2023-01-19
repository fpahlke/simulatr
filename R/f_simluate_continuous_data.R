
#' 
#' @seealso https://cran.r-project.org/package=simstudy
#' 
#' @export 
#'
getSimluatedTwoArmMeans <- function(n1, n2, mean1, mean2, sd1, sd2, ..., 
    seed = NA_integer_, 
    alternative = c("two.sided", "less", "greater")) {
    
    assertPackageIsInstalled("checkmate")
    
    checkmate::assertInt(n1, lower = 1)
    checkmate::assertInt(n2, lower = 1)
    checkmate::assertNumber(mean1)
    checkmate::assertNumber(mean2)
    checkmate::assertNumber(sd1)
    checkmate::assertNumber(sd2)
    checkmate::assertInt(seed, na.ok = TRUE)
    
    if (is.na(seed)) {
        seed <- createSeed()
    }
    
    if (!is.na(seed)) {
        set.seed(seed)
    }
    
    values1 <- rnorm(n = n1, mean = mean1, sd = sd1)
    values2 <- rnorm(n = n2, mean = mean2, sd = sd2)
    
    data <- data.frame(
        group = as.factor(c(rep(1, n1), rep(2, n2))),
        values = c(values1, values2)
    )
    
    testResult <- t.test(values ~ group, data = data, alternative = match.arg(alternative))
    
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
        confLevel = attr(testResult$conf.int,"conf.level"),
        alternative = testResult$alternative,
        method = testResult$method,
        data = data
    )
    
    result <- structure(result, class = "SimulationResult")
    
    return(result)
}

print.SimulationResult <- function(x) {
    assertPackageIsInstalled("dplyr")
    attributes(x)[["class"]] <- NULL
    x$confInt <- paste0("[", paste0(x$confInt, collapse = ", "), "]")
    x$data <- dplyr::tibble(x$data)
    print(x)
}

#' x <- getSimluatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4)
plot.SimulationResult <- function(x, ..., mainTitle = "Continuous Fake Data") {
    assertPackageIsInstalled("ggplot2")
    data <- x$data
    p <- ggplot2::ggplot(
        data = x$data,
        ggplot2::aes(y = values, x = group)
    )
    p <- p + ggplot2::geom_boxplot(ggplot2::aes(fill = group))
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
    p <- p + ggplot2::ggtitle(mainTitle)
    p <- p + ggplot2::xlab("Group")
    p <- p + ggplot2::ylab("Simulated Values")
	p <- p + ggplot2::theme(legend.position = "none")
    
    p
}

