# Sandbox for calculating first derivatives with loess

library(ggplot2)

d <- data.frame(x=seq(0,100,1),y=runif(101))
ggplot(d,aes(x=x,y=y)) +
  geom_point() +
  geom_smooth()

my_loess <- loess(y ~ x,data=d)
new <- data.frame(x=seq(0,100,1))
new$y <- predict(my_loess,new)
ggplot(new,aes(x=x,y=y)) +
  geom_point() 
# maximum appears to be at x=50
new$slope <- c(NA,diff(new$y) / diff(new$x)) # pad with an initial NA

ggplot(new,aes(x=x,y=slope)) +
  geom_point() 
# crosses zero very close to x=50

# incidentally, this is likely to be a much better model for filling in missing values than what I've been doing

