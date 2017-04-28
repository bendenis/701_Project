#' Plots the Simulated Regression Results
#'
#' \code{plot_regsim} returns a list with simulated results.
#'
#' @param reps nuber of simulated regressions. Default value of 1000.
#' @param n vector of sample sizes. Default values set to (10, 20, 30, 40, 50).
#' @param beta vector of coeficients. Default sequence from -1 to 1  by = 0.1.
#' @param eps_dist destribution of error term. Supports ("uniform","t","laplace"). Default set to "uniform".
#'
#' @return plots.
#'
#' @examples
#' plot_regsim(reps = 1000, n = c(10, 20, 30, 40, 50), beta = seq(-1, 1, by = 0.1), eps_dist = "uniform")
#'
#' @export


plot_regsim = function(reps = 1000, beta = seq(-1, 1, by = 0.1), n = c(10, 20, 30, 40, 50), eps_dist = "uniform") {

        pll = list(length = length(beta))
        for(i in 1:length(beta)){
                pll[[i]] = parallel::mclapply(n, function(x) regsim(reps, x, beta[i], eps_dist))
        }

        beta_hat = vector(length = length(beta)*length(n))
        for(j in 1:length(n)){
                v = vector(length = length(beta))
                for(i in 1:length(beta)){
                        v[i] = pll[[i]][[j]][[1]]
                }
                a = (length(beta)*(j-1) + 1)
                b = length(beta)*j
                beta_hat[a:b] = v
        }

        r_sqrt_hat = vector(length = length(beta)*length(n))
        for(j in 1:length(n)){
                v = vector(length = length(beta))
                for(i in 1:length(beta)){
                        v[i] = pll[[i]][[j]][[2]]
                }
                a = (length(beta)*(j-1) + 1)
                b = length(beta)*j
                r_sqrt_hat[a:b] = v
        }

        power_hat = vector(length = length(beta)*length(n))
        for(j in 1:length(n)){
                v = vector(length = length(beta))
                for(i in 1:length(beta)){
                        v[i] = pll[[i]][[j]][[3]]
                }
                a = (length(beta)*(j-1) + 1)
                b = length(beta)*j
                power_hat[a:b] = v
        }

        num = sapply(n, function(x) rep(x, length(beta)))
        dim(num) = c(dim(num)[1]*dim(num)[2],1)


        dt = data.frame(beta_hat = beta_hat,
                        r_sqrt_hat = r_sqrt_hat,
                        power_hat = power_hat,
                        n = as.factor(num),
                        beta = rep(beta,length(n)))

        g1 = ggplot2::ggplot(dt, ggplot2::aes(y = power_hat, x = beta, color = n)) + ggplot2::geom_line() +
                ggplot2::labs(title = "Estimated Power") + ggplot2::xlab("Effect Size") +
                ggplot2::ylab("Power")
        g2 = ggplot2::ggplot(dt, ggplot2::aes(y = beta_hat, x = beta, color = n)) + ggplot2::geom_line() +
                ggplot2::labs(title = "Estimated Beta") + ggplot2::xlab("Actual Beta") +
                ggplot2::ylab("Beta Hat")
        g3 = ggplot2::ggplot(dt, ggplot2::aes(y = r_sqrt_hat, x = beta, color = n)) + ggplot2::geom_line() +
                ggplot2::labs(title = "Estimated R^2") + ggplot2::xlab("Effect Size") +
                ggplot2::ylab("R^2")

        return(list(g1,g2,g3))

}
