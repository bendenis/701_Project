context("plot_regsim output")


test_that("ggplot class uniform", {

        plots = plot_regsim(reps = 1000, n = c(10, 50, 100), beta = c(0.1,0.2,0.3,0.4,0.5), eps_dist = "uniform")
        expect_that(class(plots), equals("list"))
        expect_that(length(plots), equals(3))
        expect_that(class(plots[[1]])[1], equals("gg"))
        expect_that(class(plots[[1]])[2], equals("ggplot"))
})

test_that("ggplot class t", {

        plots = plot_regsim(reps = 1000, n = c(10, 50, 100), beta = c(0.1,0.2,0.3,0.4,0.5), eps_dist = "t")
        expect_that(class(plots), equals("list"))
        expect_that(length(plots), equals(3))
        expect_that(class(plots[[1]])[1], equals("gg"))
        expect_that(class(plots[[1]])[2], equals("ggplot"))
})

test_that("ggplot class laplace", {

        plots = plot_regsim(reps = 1000, n = c(10, 50, 100), beta = c(0.1,0.2,0.3,0.4,0.5), eps_dist = "laplace")
        expect_that(class(plots), equals("list"))
        expect_that(length(plots), equals(3))
        expect_that(class(plots[[1]])[1], equals("gg"))
        expect_that(class(plots[[1]])[2], equals("ggplot"))
})





