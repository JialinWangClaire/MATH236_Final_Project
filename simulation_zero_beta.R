library(glmnet)
library(selectiveInference)
library("ggplot2")

#initialize
p = 100
n = 80
set.seed(0) 
beta_0 = 0.5
e <- rnorm(n, 0, 2.5)
my_range <- 1:p


#generate x
x = list()
for (i in 1:n){
  x[[i]] <- rnorm(p)
}
xx <-matrix(unlist(x),nrow = n)
data1 = as.data.frame(xx)

empirical_loss_list = list()
my_seq = seq(1,50)
real_seq = list()

for (z in my_seq){
  #generate_beta
  beta<- vector(mode = "list", length = 0)
  for (i in my_range){
    num = sample(2:10,1)
    beta_i = num
    beta <- c(beta,beta_i)
  }
  zeros = sample(1:p,z)
  for (i in zeros){
    beta[[i]] = 0
  }
  
  
  #generate y
  y = 0
  for (j in my_range){
    y = y + unlist(beta[[j]]) * xx[,j]
  }
  y = y + beta_0
  y = y + e
  yy<-as.matrix(y)
  
  # compute coefficients
  lasso_cv <- cv.glmnet(xx, yy, alpha = 1, type.measure="mse",
                        standardize = TRUE, nfolds =5,family="gaussian")
  lambda_cv <- lasso_cv$lambda.min
  gfit = glmnet(xx,yy,thresh=1E-10,standardize=FALSE)
  em_beta = coef(gfit, x=xx, y=yy, s=lambda_cv/n, exact=TRUE)[-1]
  sigma = estimateSigma(xx, yy, intercept=TRUE, standardize=TRUE)
  sigma1 = sigma$sigmahat
  
  skip_to_next <- FALSE
  trytry <- tryCatch({fixedLassoInf(xx,yy,em_beta,lambda_cv,
                                    family = c("gaussian"),
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
                                    linesearch.try=10)}, 
                     error = function(e){skip_to_next <<- TRUE})
  if (skip_to_next){next}
  real_seq <- c(real_seq, z)
  result = fixedLassoInf(xx,yy,em_beta,lambda_cv,
                         family = c("gaussian"),
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
  
  
  #compute empirical loss
  l = lengths(list(Var))
  total = 0
  for (i in 1:l){
    total = total + (beta[[Var[i]]] - Coef[i]) ** 2
  }
  empirical_loss = total %/% l
  
  empirical_loss_list <- c(empirical_loss_list,list(empirical_loss))
}


#plot
plot(unlist(real_seq),unlist(empirical_loss_list), type="l", col="brown", lwd=3, xlab="number of zero beta_i", ylab="empirical_loss", main="Influence of number of zero beta_i")


