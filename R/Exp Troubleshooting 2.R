xmin <- xmins[321]
lambda <- mean(xi)
xi <- x[x>xmin]
n <- length(xi)

create_nll <- function(x){
  xi <- x[x>xmin]
  n <- length(xi)
  function(lambda){
    nll <- -(sum(dexp(xi, rate=lambda, log = TRUE)) - n*pexp(xmin, rate=lambda, log.p = TRUE, lower.tail = FALSE))
    if (!is.finite(nll)){
      nll <- 1e+12
    }
    nll
  }
}


  # ll <- colSums(matrix(sapply(theta_0, function(i) dexp(xi, i, log = TRUE)), nrow = length(xi))) - n * sapply(theta_0, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
  # -ll
  # joint_prob = colSums(matrix(sapply(theta_0, function(i) dexp(xi, i, log = TRUE)), nrow = length(xi)))
  # prob_over = sapply(theta_0, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
  # ll=joint_prob - n * prob_over
  # -ll



# negloglike = function(par) {
#   r = -conexp_tail_ll(xi, par, xmin)
#
# }
# conexp_tail_ll = function (xi, par, xmin){
#   n = length(xi)
#   joint_prob = colSums(matrix(sapply(par, function(i) dexp(xi, i, log = TRUE)), nrow = length(xi)))
#   prob_over = sapply(par, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
#   return(joint_prob - n * prob_over)
# }

xi <- x[x>xmin] # truncate dataset at xmin
n <- length(xi) #size of truncated data set
my_nll <- create_nll(xi)
pars <- mean(xi)
mle = stats4::mle(minuslogl = my_nll, start=list(lambda=pars), method = "L-BFGS-B", lower = 0)
pars.list[i] = as.numeric(mle@coef[1])

pars.list[i]
#  0.3157643

# create_nll_LOGNORM <- function(x){
#   xi <- x[x>xmin]
#   n <- length(xi)
#   function(mu, sigma) {
#     ll <- sum(dnorm(log(xi), mean=mu, sd=sigma, log=TRUE)) - n*pnorm(log(xmin), mean=mu, sd=sigma, log=TRUE, lower.tail=FALSE)
#     -ll
#   }
# }

i=321
xmin <- xmins[i]
xi <- x[x>xmin]
n <- length(xi)
theta_0 <- mean(xi)

negloglike = function(par) {
  r = -conexp_tail_ll(xi, par, xmin)
  if (!is.finite(r))
    r = 1e+12
  r
}
conexp_tail_ll = function (xi, rate, xmin){
  n = length(xi)
  joint_prob = colSums(matrix(sapply(rate, function(i) dexp(xi, i, log = TRUE)), nrow = length(xi)))
  prob_over = sapply(rate, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
  return(joint_prob - n * prob_over)
}

create_nll <- function(x){
  xi <- x[x>xmin]
  n <- length(xi)
  theta_0 <- mean(xi)
  joint_prob = colSums(matrix(sapply(par, function(i) dexp(xi, i, log = TRUE)), nrow = length(xi)))
  prob_over = sapply(par, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
  nll=joint_prob - n * prob_over
  -ll
  # if (!is.finite(nll))
  #   nll = 1e+12
  # nll
}

mle = stats4::mle(minuslogl = create_nll, start=theta_0, method = "L-BFGS-B", lower = 0)
pars.list[i] = as.numeric(mle@coef[1])
pars.list[i]
