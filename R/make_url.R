#' Form url for API call to recipe puppy
#'
#' \code{make_url} returns a url as a character string based in user inputs. Made to handle NULL.
#'
#' @param user_ingredients_input vector of character strings.
#' @param user_search_query a character string.
#'
#' @return ulr as a character string.
#'
#' @examples
#' make_url(c("onion", "cheese", "beef"), "burger")



make_url = function(user_ingredients_input,user_search_query) {

        if(is.null(user_ingredients_input) == FALSE & is.null(user_search_query) == FALSE){

                ingredients = user_ingredients_input[1]

                if(length(user_ingredients_input) > 1) {
                        for(i in 2:length(user_ingredients_input)){
                                ingredients = stringr::str_c(ingredients,
                                                             user_ingredients_input[i], sep = ",")
                        }
                }

                url_request = stringr::str_c("http://www.recipepuppy.com/api/?i=",ingredients,"&q=",user_search_query)

        }

        if(is.null(user_ingredients_input) == TRUE & is.null(user_search_query) == FALSE){

                url_request = stringr::str_c("http://www.recipepuppy.com/api/?q=",user_search_query)

        }

        if(is.null(user_ingredients_input) == FALSE & is.null(user_search_query) == TRUE){

                ingredients = user_ingredients_input[1]

                if(length(user_ingredients_input) > 1) {
                        for(i in 2:length(user_ingredients_input)){
                                ingredients = stringr::str_c(ingredients,
                                                             user_ingredients_input[i], sep = ",")
                        }
                }

                url_request = stringr::str_c("http://www.recipepuppy.com/api/?i=",ingredients)
        }

        return(url_request)

}
