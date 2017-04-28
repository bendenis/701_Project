context("make_url output")
library(stringr)

test_that("if url correct", {

        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "onion"))
        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "cheese"))
        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "beef"))
        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "burger"))
        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "^http://www.recipepuppy.com/api/?"))
        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "i="))
        expect_true(str_detect(make_url(c("onion", "cheese", "beef"), "burger"), "q="))

})
