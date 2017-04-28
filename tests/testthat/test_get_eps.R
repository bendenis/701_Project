context("get_eps output")

test_that("epsilon vector length", {

        expect_that(length(get_eps("uniform",100)),equals(100))
        expect_that(length(get_eps("laplace",50)),equals(50))
        expect_that(length(get_eps("t",70)),equals(70))

})

test_that("epsilon vector expectation", {

        expect_true(mean(get_eps("uniform",100)) < 0.75)
        expect_true(mean(get_eps("uniform",100)) > 0.25)
        expect_true(mean(get_eps("laplace",100)) < 0.25)
        expect_true(mean(get_eps("laplace",100)) > -0.25)
        expect_true(mean(get_eps("t",100)) < 0.25)
        expect_true(mean(get_eps("t",100)) > -0.25)

})
