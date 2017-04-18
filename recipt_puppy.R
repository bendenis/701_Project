library(jsonlite)
library(stringr)
user_ingredients_input = c("onion", "cheese", "beef")
ingredients = user_ingredients_input[1]
for(i in 2:length(user_ingredients_input)){
        ingredients = str_c(ingredients,
                            user_ingredients_input[i], sep = ",")
}
user_search_query = "burger"
minNumIngredients = 2
RES = data.frame()
for(i in 1:10){
        url_request = str_c("http://www.recipepuppy.com/api/?i=",ingredients,"&q=",user_search_query,"&p=",i)
        res = fromJSON(url_request)$results
        if(class(dim(res)) == "NULL") { break() }
        CHE = matrix(nrow = length(user_ingredients_input), ncol = 10)
        for(j in 1:length(user_ingredients_input)){
                l = length((str_detect(res[,3], user_ingredients_input[j])))
                CHE[j,1:l] = str_detect(res[,3], user_ingredients_input[j])
        }
        print(CHE)
        include = which(colSums(CHE) >= minNumIngredients)
        print(include)
        RES = rbind(RES,res[include,])
}