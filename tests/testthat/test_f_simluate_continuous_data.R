


test_that("For invalid input arguments 'getSimulatedTwoArmMeans' throws meaningful exceptions", {
    expect_error(
        getSimulatedTwoArmMeans(),
        'argument "n1" is missing, with no default'
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50),
        'argument "n2" is missing, with no default'
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50, n2 = 50),
        'argument "mean1" is missing, with no default'
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5),
        'argument "mean2" is missing, with no default'
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7),
        'argument "sd1" is missing, with no default'
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3),
        'argument "sd2" is missing, with no default'
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 0, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4),
        "Assertion on 'n1' failed: Element 1 is not >= 1."
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50, n2 = -5, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4),
        "Assertion on 'n2' failed: Element 1 is not >= 1."
    )
    expect_error(
        getSimulatedTwoArmMeans(n1 = 50, n2 = 50, mean1 = 5, mean2 = 7, sd1 = 3, sd2 = 4, seed = 1.5),
        "Assertion on 'seed' failed: Must be of type 'single integerish value', not 'double'."
    )
    expect_error(
        getSimulatedTwoArmMeans(
            n1 = 50, n2 = 50, mean1 = 5, mean2 = 7,
            sd1 = 3, sd2 = 4, seed = 123, alternative = "one"
        ),
        '\'arg\' should be one of "two.sided", "less", "greater"'
    )
})

test_that("Default display of 'getSimulatedTwoArmMeans' returns an output", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4, seed = 232323
    )
    
    expect_output(showDefault.SimulationResult(simResult))
})

test_that("With defined seed 'getSimulatedTwoArmMeans' returns reproducible results", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4, seed = 232323
    )

    expect_equal(simResult$n1, 10)
    expect_equal(simResult$n2, 10)
    expect_equal(simResult$allocationRatio, 1)
    expect_equal(simResult$mean1, 5)
    expect_equal(simResult$mean2, 7)
    expect_equal(simResult$sd1, 3)
    expect_equal(simResult$sd2, 4)
    expect_equal(simResult$meanSim1, 5.8835095, tolerance = 1e-07)
    expect_equal(simResult$meanSim2, 5.7358547, tolerance = 1e-07)
    expect_equal(simResult$sdSim1, 2.9701436, tolerance = 1e-07)
    expect_equal(simResult$sdSim2, 3.4836918, tolerance = 1e-07)
    expect_equal(simResult$seed, 232323)
    expect_equal(simResult$statistic, 0.10199383, tolerance = 1e-07)
    expect_equal(simResult$df, 17.560814, tolerance = 1e-07)
    expect_equal(simResult$pValue, 0.91991717, tolerance = 1e-07)
    expect_equal(simResult$confInt, c(-2.899276, 3.1945855), tolerance = 1e-07)
    expect_equal(simResult$confLevel, 0.95, tolerance = 1e-07)
    expect_equal(simResult$alternative, "two.sided")
    expect_equal(simResult$method, "Welch Two Sample t-test")
    expect_equal(simResult$data$group, factor(c(
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2
    )))
    expect_equal(simResult$data$values, c(
        5.6444129, 7.0815453, 8.5169894, 9.5635767, 5.4989867,
        8.5665747, 3.1804153, 4.3349261, 6.845017, -0.39734943, 8.8410798, 2.3826052,
        4.3751252, 8.2548479, 2.8344348, 12.083721, 7.337037, 0.48579194, 4.6572878,
        6.1066166
    ), tolerance = 1e-07)
})

test_that("With undefined seed 'getSimulatedTwoArmMeans' returns valid results", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4
    )

    expect_s3_class(simResult, "SimulationResult")
    expect_type(simResult$data, "list")
    expect_equal(names(simResult$data), c("group", "values"))
    expect_type(simResult$seed, "integer")
})

test_that("'getSimulatedTwoArmMeans' result can be printed", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4, seed = 232323
    )

    expect_output(print(simResult))
})

test_that("'getSimulatedTwoArmMeans' result plot layers match expectations", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4, seed = 232323
    )

    p <- plot(simResult)
    expect_s3_class(p$layers[[1]], "ggproto")
    expect_s3_class(p$layers[[1]]$geom, "GeomBoxplot")
    expect_s3_class(p$layers[[1]]$stat, "StatBoxplot")
})

test_that("'getSimulatedTwoArmMeans' result x axis is labelled 'Simulated Values'", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4, seed = 232323
    )
    
    p <- plot(simResult)
    expect_identical(p$labels$x, "Group")
})

test_that("'getSimulatedTwoArmMeans' result y axis is labelled 'Simulated Values'", {
    simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4, seed = 232323
    )

    p <- plot(simResult)
    expect_identical(p$labels$y, "Simulated Values")
})

test_that("With undefined seed 'getSimulatedTwoArmMeans' returns valid results although network connection is missing", {
    expect_warning(simResult <- getSimulatedTwoArmMeans(
        n1 = 10, n2 = 10, mean1 = 5, mean2 = 7,
        sd1 = 3, sd2 = 4,  
        test_exception = "network error"
    ), "Failed to receive new seed from www.random.org: network error")

    expect_type(simResult$seed, "integer")
})


