context("regsim output")

test_that("regsim t dist", {

        expect_that(length(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")), equals(3))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")), equals("list"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$power), equals("numeric"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$beta_hat), equals("numeric"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$r_sqrt), equals("numeric"))

})

test_that("regsim uniform dist", {

        expect_that(length(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")), equals(3))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")), equals("list"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$power), equals("numeric"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$beta_hat), equals("numeric"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$r_sqrt), equals("numeric"))

})

test_that("regsim laplace dist", {

        expect_that(length(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")), equals(3))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")), equals("list"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$power), equals("numeric"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$beta_hat), equals("numeric"))
        expect_that(class(regsim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$r_sqrt), equals("numeric"))

})

