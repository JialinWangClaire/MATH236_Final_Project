# Simulate the linear model (n < p)(n = 80, p = 100): 
#y = beta_0 + \sum_{i=1}{50}beta_i*x_i + epsilon
#where x_i ~ N(0,1) and epsilon ~ N(0,2.5^2)
#beta_0 = 0.5 beta_i = 0 if i is a multiply of 5

library(glmnet)
library(selectiveInference)
library("ggplot2")

#initialize
n = 30
p = 50
set.seed(20) 
e <- rnorm(n, 0, 2.5)
beta_0 = 0.5
my_range <- 1:p

#generate_beta
beta<- vector(mode = "list", length = 0)
for (i in my_range){
  num = sample(2:10,1)
  beta_i = num
  beta <- c(beta,beta_i)
}
zeros = sample(1:p,10)
for (i in zeros){
  beta[[i]] = 0
}

#generate x
x = list()
for (i in 1:n){
  x[[i]] <- rnorm(p)
}
xx <-matrix(unlist(x),nrow = n)
data1 = as.data.frame(xx)

#generate y
y = 0
for (j in my_range){
  y = y + unlist(beta[[j]]) * xx[,j]
}
y = y + beta_0
y = y + e
yy<-as.matrix(y)

#generate more data
num = sample(c(1:n),size=2*p,replace=T)
data_x = NULL
data_y = NULL
for (j in 1:n){
  data_x <- c(data_x,xx[as.vector(num)[j],])
  data_y <- c(data_y,yy[as.vector(num)[j]])
}
xx <- t(matrix(data_x,nrow = p))
yy <- matrix(data_y)

# fit model
lasso_cv <- cv.glmnet(xx, yy, alpha = 1, type.measure="mse",
                      standardize = TRUE, nfolds =5,family="gaussian")
#windows(width = 12, height = 10)
plot(lasso_cv)
lambda_cv <- lasso_cv$lambda.min
gfit = glmnet(xx,yy,thresh=1E-10,standardize=FALSE)
em_beta = coef(gfit, x=xx, y=yy, s=lambda_cv/n, exact=TRUE)[-1]
sigma = estimateSigma(xx, yy, intercept=TRUE, standardize=TRUE)
sigma1 = sigma$sigmahat
result = fixedLassoInf(xx,yy,em_beta,lambda_cv,
                       family = c("gaussian", "binomial", "cox"),
                       intercept=TRUE,
                       add.targets=NULL,
                       status=NULL,
                       sigma=sigma1,
                       alpha=0.1,
                       type=c("partial","full"),
                       tol.beta=1e-5,
                       tol.kkt=0.1,
                       gridrange=c(-100,100),
                       bits=NULL,
                       verbose=FALSE,
                       linesearch.try=10)
Var <- result$vars
Coef <- result$coef0

LowConfPt<- result$ci[,1]
UpConfPt <- result$ci[,2]

data <- round(data.frame(x = Var,
                         y = Coef,
                         lower = LowConfPt,
                         upper = UpConfPt), 2)


colors <- c("LASSO interval" = 'darkred')
# p1 <- ggplot(data, aes(Var, Coef)) 
Points = data.frame(x=c(1:p), y = unlist(beta))
Points_coef = data.frame(x=Var, y = Coef)
# p1 <- p1 + geom_point(data=Points, size=1, aes(x,y),color='lightblue')
# p1 <- p1 + geom_point(data=Points_coef, size=1, aes(x,y),color='brown')
# p1 <- p1 + theme(legend.title = element_blank())
# p1

p2 <- ggplot(data, aes(Var, Coef)) +  
  geom_errorbar(aes(ymin = LowConfPt, ymax = UpConfPt, color="LASSO interval"),show.legend = TRUE)
p2 <- p2 + geom_point(data=Points, size=1, aes(x,y),color='lightblue')
p2 <- p2 + geom_point(data=Points_coef, size=1, aes(x,y),color='brown')
p2 <- p2 + theme(legend.title = element_blank())
p2
total = 0
l = lengths(list(Var))
for (i in 1:l){
  total = total + (beta[[Var[i]]] - Coef[i]) ** 2
}
empirical_loss = total %/% l
empirical_loss

