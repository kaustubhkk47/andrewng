set.seed(1234)
y_length <- 100
deviation <- 0.2
num_iter <- 1000
learning_rate <- 0.1

y <- seq(0, 1, 1/y_length)
x <- y + deviation*(runif(length(y)) - 0.5)
z <- y + deviation*(runif(length(y)) - 0.5)

X <- matrix(c(rep(1, 101), x, z), byrow = F, nrow = 101)
theta <- matrix(rep(0, ncol(X)))
df <- data.frame(x = x, y =y , z=z)

computeCost <- function(X, y, theta){
m <- length(y)
J = sum((t(t(theta)%*%t(X)) - y)^2)/2/m
return(J)
}

gradientDescent <- function(X, y, theta, alpha, num_iter){
J_history = rep(0,num_iter)
m <- length(y)
for (i in 1:num_iter){
theta <- theta - alpha/m*t(X)%*%(t(t(theta)%*%t(X)) - y)
J_history[i] <- computeCost(X, y, theta)
}
return(list(theta,J_history))
}

print("custom gradient descent implementation")
gd_result <- gradientDescent(X,y,theta, learning_rate, num_iter)
gd_result[[1]]
qplot(1:length(gd_result[[2]]), gd_result[[2]])


print("R gradient descent implementation")

lm(y~., data=df)

