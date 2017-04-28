context("reg_sim output")

test_that("reg_sim t dist", {

        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")), equals(6))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")), equals("list"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$power), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$beta_hat), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$r_sqrt), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$power_norm), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$beta_hat_norm), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$r_sqrt_norm), equals("numeric"))

        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$beta_hat), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$r_sqrt), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$beta_hat_norm), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "t")$r_sqrt_norm), equals(100))
})

test_that("reg_sim uniform dist", {

        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")), equals(6))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")), equals("list"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$power), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$beta_hat), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$r_sqrt), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$power_norm), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$beta_hat_norm), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$r_sqrt_norm), equals("numeric"))

        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$beta_hat), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$r_sqrt), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$beta_hat_norm), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "uniform")$r_sqrt_norm), equals(100))

})

test_that("regsim laplace dist", {

        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")), equals(6))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")), equals("list"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$power), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$beta_hat), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$r_sqrt), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$power_norm), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$beta_hat_norm), equals("numeric"))
        expect_that(class(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$r_sqrt_norm), equals("numeric"))

        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$beta_hat), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$r_sqrt), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$beta_hat_norm), equals(100))
        expect_that(length(reg_sim(reps = 100, n = 24, beta = 0.5, eps_dist = "laplace")$r_sqrt_norm), equals(100))

})
