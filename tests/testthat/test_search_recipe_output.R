context("reg_sim output")

test_that("a test", {
        expect_that(class(search_recipe(ingredients = "tomato")), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = "tomato", search = "pasta")), equals("data.frame"))
        expect_that(class(search_recipe(search = "pasta", ingredients = "tomato")), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"))), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), search = "pasta")), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), minNumIngredients = 1)), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), search = "pasta", minNumIngredients = 1)), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), minNumIngredients = 1, search = "pasta")), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), minNumIngredients = 2)), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), search = "pasta", minNumIngredients = 2)), equals("data.frame"))
        expect_that(class(search_recipe(ingredients = c("tomato","oil"), minNumIngredients = 2, search = "pasta")), equals("data.frame"))
        expect_that(class(search_recipe(minNumIngredients = 2,ingredients = c("tomato","oil"))), equals("data.frame"))
        expect_that(class(search_recipe(search = "pasta",minNumIngredients = 2, ingredients = c("tomato","oil"))), equals("data.frame"))
        expect_that(class(search_recipe(search = "pasta", ingredients = c("tomato","oil"), minNumIngredients = 2)), equals("data.frame"))

})




# search_recipe(search = "pasta")
# search_recipe(ingredients = c("tomato","oil"), minNumIngredients = 3)
