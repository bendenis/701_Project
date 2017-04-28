#' Get the errors with specified distribution.
#'
#' \code{get_eps} returns a randomly generated numeric vector.
#'
#' @param eps_dist specified type of distibution.Supports ("uniform","t","laplace").
#' @param n sample size
#'
#' @return numeric vector.
#'
#' @examples
#' get_eps(eps_dist = "uniform", n = 100)

get_eps = function(eps_dist, n){
        EPS = data.frame(stats::runif(n), stats::rt(n,n), smoothmest::rdoublex(n))

        if(eps_dist == "uniform"){
                eps = EPS[,1]
        }

        if(eps_dist == "t"){
                eps = EPS[,2]
        }

        if(eps_dist == "laplace"){
                eps = EPS[,3]
        }

        return(eps)

}
