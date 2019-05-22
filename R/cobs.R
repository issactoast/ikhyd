# library(cobs)
# x <- seq(-1,1, length.out = 50)
# y <- (f.true <- pnorm(2*x)) + rnorm(50)/10
# ## specify pointwise constraints (boundary conditions)
# con <- rbind(c( 1,min(x),0), # f(min(x)) >= 0
#              c(-1,max(x),1), # f(max(x)) <= 1
#              c(0,  0,   0.5))
# plot(x, y)
#
#
# ## specify pointwise constraints (boundary conditions)
# con <- rbind(c( 1, min(x), 0), # f(min(x)) >= 0
#              c(0,    -1.0, 0),
#              c(0,  1,    1.2)) # f(0)      = 0.5
#
#
# ## obtain the median regression B-spline using automatically selected knots
# Rbs <- cobs(x,y, pointwise=con)
# Rbs
#
# plot(x,y)
# lines(predict(Rbs), col = 2, lwd = 1.5)
#
# lines(spline(x,f.true), col = "gray40")
#
# Rbsub <- cobs(x,y,constraint="increase",pointwise=con, n.sub = 45)
#
# summary(Rbsub)
# lines(predict(Rbsub), col = 4, lwd = 1)
#
