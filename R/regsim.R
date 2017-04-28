#' Simulates Regression with non-Normal error terms.
#'
#' \code{regsim} returns a list with simulated results.
#'
#' @param reps nuber of simulated regressions. Default value of 1000.
#' @param n sample size. Default value of 100.
#' @param beta the slope coeficient. Default value of 0.5.
#' @param eps_dist distribution of error term. Supports ("uniform","t","laplace"). Default set to "uniform".
#'
#' @return list of mean estimated beta coefficient, mean estimated R^2, and estimated power for each repetition.
#' Returns results for the specified distribution only.
#'
#' @examples
#' regsim(reps = 1000, n = 100, beta = 0.1, eps_dist = "uniform")


regsim = function(reps = 1000, n = 100, beta = 0.5, eps_dist = "uniform") {
        beta_hat = vector(length = reps)
        r_srt = vector(length = reps)
        power = vector(length = reps)

        EPS = data.frame(stats::runif(n), stats::rt(n,n), smoothmest::rdoublex(n))

        for(i in 1:reps){
                eps = get_eps(eps_dist, n)
                sd = sd(eps)

                x = stats::rnorm(n)
                y1 = beta*x + eps
                y2 = beta*x + stats::rnorm(n,sd = sd)

                s1 = summary(stats::lm(y1 ~ x))
                beta_hat[i] = s1$coefficients[2,1]
                r_srt[i] = s1$r.squared
                power[i] = s1$coefficients[2,4]

                # s2 = summary(stats::lm(y2 ~ x))
                # beta_hat[i] = s2$coefficients[2,1]
                # r_srt[i] = s2$r.squared

        }

        power = sum(power < 0.05)/reps

        return(list(beta_hat = mean(beta_hat), r_sqrt = mean(r_srt), power = power))
}

