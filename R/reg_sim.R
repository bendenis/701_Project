#' Simulates Regression with non-Normal and Normal error terms.
#'
#' \code{reg_sim} returns a list with simulated results.
#'
#' @param reps number of simulated regressions. Default value of 1000.
#' @param n sample size. Default value of 100.
#' @param beta the slope coeficient. Default value of 0.5.
#' @param eps_dist distribution of error term. Supports ("uniform","t","laplace"). Default set to "uniform".
#'
#' @return list of estimated beta coefficients, R^2, and power estimation for each repetition, for both
#' errors of a specified distribution and normal errors.
#'
#' @examples
#' reg_sim(reps = 1000, n = 100, beta = 0.1, eps_dist = "uniform")
#'
#' @export


reg_sim = function(reps = 1000, n = 100, beta = 0.5, eps_dist = "uniform") {
        beta_hat = vector(length = reps)
        r_srt = vector(length = reps)
        power = vector(length = reps)

        beta_hat2 = vector(length = reps)
        r_srt2 = vector(length = reps)
        power2 = vector(length = reps)

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

                s2 = summary(stats::lm(y2 ~ x))
                beta_hat2[i] = s2$coefficients[2,1]
                r_srt2[i] = s2$r.squared

        }

        power = sum(power < 0.05)/reps
        power2 = sum(power2 < 0.05)/reps

        return(list(beta_hat = beta_hat, r_sqrt = r_srt, power = power,
                    beta_hat_norm = beta_hat2, r_sqrt_norm = r_srt2, power_norm = power2))
}
