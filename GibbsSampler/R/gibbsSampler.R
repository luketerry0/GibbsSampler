#' Rbvnorm
#'
#' @param iter the number of iterations to make
#' @param mu the mean vector of the BVN distribution
#' @param sigma the covariance matrix of the BVN distribution
#'
#' @return a named list of a dataframe of x1 and x2 iterations and original parameters
#' @export
#'
#' @examples
#' rbvnorm(10, c(1, 1, 1), matrix(c(1, 1, 1, 1, 1, 1, 1, 1, 1),nrow=3),seed=3)
#' rbvnorm(iter = 10000, mu = c(3,6), sigma = matrix(c(8,-2,-2,4), nrow =2, byrow = TRUE))
rbvnorm <- function(iter,mu,sigma,seed=3){
  # seed a value for x_2
  x_2 <- seed

  # keep track of the x1 and x2 values
  x1_values <- c()
  x2_values <- c()

  # iterate "iter" times
  for (i in c(1:iter)){
    # x1 given x2
    x_1 <- rnorm(1, mu[1] + sigma[1,2]*(1/sigma[2,2])*(x_2 - mu[2]), sigma[1,1] - sigma[1,2]*(1/sigma[2,2])*sigma[2,1])
    # x2 given x1
    x_2 <- rnorm(1, mu[2] + sigma[2,1]*(1/sigma[1,1])*(x_1 - mu[1]), sigma[2,2] - sigma[2,1]*(1/sigma[1,1])*sigma[1,2])

    # keep track for data frames
    x1_values = c(x1_values, x_1)
    x2_values = c(x2_values, x_2)
  }

  df <- data.frame(x1_values, x2_values)
  invisible(list(gibbs=df, iter = iter, mu = mu, sigma = sigma))
}
