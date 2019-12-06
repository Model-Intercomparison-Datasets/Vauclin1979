# install.packages('adagio')
# install.packages('cmaes')

library(adagio)
library(cmaes)
library(hydroGOF)
# NOT RUN {
##  Polynomial minimax approximation of data points
##  (see the Remez algorithm)
n <- 10; m <- 101           # polynomial of degree 10; no. of data points
xi <- seq(-1, 1, length = m)
obs <- 1 / (1 + (5*xi)^2)    # Runge's function

n=5
col=1
fun.model <- function(p, x) {     # Horner scheme
  r = outer(x, (length(p) - 1):0, "^") %*% p
  # points(1:length(p),p, col=col)
  r = as.numeric(r)
}

fun.obs <- function(x, y, n){   # polynomial fitting of degree n
  qr.solve(outer(x, seq(n, 0), "^"), y)
}

objFun <- function(p){           # objective function
  rmse(as.numeric(fun.model(p,xi)), obs)
  # max(abs(fun.model(p, xi) - obs))
}
lim= c(-500,500)

pf <- fun.obs(xi, obs, n)      # start with a least-squares fitting
pf=rep(1,n+1)
plot(0,type='n',ylim=lim, xlim=c(0,n+1))
sol1 <- pureCMAES(par=pf,
                  fun = objFun,
                  lower = rep(lim[1], n+1),
                  upper = rep(lim[2], n+1))
sol1
y=fun.model(sol1$xmin, xi)
points(1:(n+1), sol1$xmin, col=2)
# plot(y,obs, asp=1);grid(); abline(0,1)
# rmse(y,obs)
matplot(cbind(y,obs), col=1:2, type=c('l','p'), pch=20);grid()

zapsmall(sol1$xmin)

stop()
