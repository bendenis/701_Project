#' Searching Recipes
#'
#' \code{search_recipe} returns a data.frame with search results. Make sure to spell the ingredients correctly,
#' or the search won't work. You must specify at leat one ingredient. You can also add a search for a particular type
#' of dish via the `search` parameter. If you pass on more than 2 ingredients, you may set a minimum number of
#' supplied ingredients to be included in the recipe. Ingredients more important for the dish must be passed in first.
#' The function returns a data.frame object with a list of links to recipes, required ingredients, and name of the dish.
#' Bon Appetit!
#'
#' @param ingredients vector of character strings.
#' @param search a character string.
#' @param minNumIngredients an integer.
#'
#' @return data.frame object with search results
#'
#' @examples
#' search_recipe(c("onion", "cheese", "beef"), "burger", 3)
#'
#' @export

search_recipe = function(ingredients = NULL, search = NULL, minNumIngredients = 1){

        if(is.null(ingredients) == TRUE) { stop("Must specify at least one ingredient!") }
        if(is.null(ingredients) == FALSE & length(ingredients) < minNumIngredients){
                stop("minNumIngredients can't be larger than the number of ingredients!")
        }

        url = make_url(ingredients, search)

        RES = data.frame()
        for(i in 1:10){
                url_request = stringr::str_c(url,"&p=",i)
                res = jsonlite::fromJSON(url_request)$results
                if(class(dim(res)) == "NULL") { break() }
                CHE = matrix(nrow = length(ingredients), ncol = 10)
                for(j in 1:length(ingredients)){
                        l = length((stringr::str_detect(res[,3], ingredients[j])))
                        CHE[j,1:l] = stringr::str_detect(res[,3], ingredients[j])
                }
                #print(CHE)
                include = which(colSums(CHE) >= minNumIngredients)
                #print(include)
                RES = rbind(RES,res[include,])
        }

        return(RES)

}








