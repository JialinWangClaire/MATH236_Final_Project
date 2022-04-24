library(glmnet)
library(selectiveInference)
library("ggplot2")
if(!require('plyr')) {
  install.packages('plyr')
  library('plyr')
}

#initialize
p = 100
n = 60
set.seed(20) 
beta_0 = 0.5
e <- rnorm(n, 0, 2.5)
my_range <- 1:p
times = 20



#generate x
x = list()
for (i in 1:n){
  x[[i]] <- rnorm(p)
}
xx <-matrix(unlist(x),nrow = n)
data1 = as.data.frame(xx)

#generate_beta
beta<- vector(mode = "list", length = 0)
for (i in my_range){
  num = sample(2:10,1)
  beta_i = num
  beta <- c(beta,beta_i)
}
zeros = sample(1:p,5)
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


prob_list = list()
my_seq = seq(n,2*p,by=5)
real_seq = list()



res <- NULL
res_coef<- list()
for (i in 1:p){
  res_coef[[i]] = list(NULL)
}


for (size in my_seq){
  for (i in 1:times) {
    #generate data
    num = sample(c(1:n),size=size,replace=T)
    data_x = NULL
    data_y = NULL
    for (j in 1:n){
      data_x <- c(data_x,xx[as.vector(num)[j],])
      data_y <- c(data_y,yy[as.vector(num)[j]])
    }
    data_xx <- t(matrix(data_x,nrow = p))
    data_yy <- matrix(data_y)
    
    
    #compute coefficients
    lasso_cv <- cv.glmnet(data_xx, data_yy, alpha = 1, type.measure="mse",
                          standardize = TRUE, nfolds =5,family="gaussian",lambda = seq(0.1,5,by = 0.1))
    lambda_cv <- lasso_cv$lambda.min
    gfit_normal = glmnet(data_xx,data_yy,thresh=1E-20,standardize=TRUE)
    d = dim(data_xx)[1]
    beta_normal = coef(gfit_normal, x=data_xx, y=data_yy, s=lambda_cv/d, exact=TRUE)[-1]
    sigma = estimateSigma(data_xx, data_yy, intercept=TRUE, standardize=TRUE)
    sigma1 = sigma$sigmahat
    skip_to_next <- FALSE
    trytry <- tryCatch({fixedLassoInf(data_xx,data_yy,beta_normal,lambda_cv,
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
    result = fixedLassoInf(data_xx,data_yy,beta_normal,lambda_cv,
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
    
    s<-c(result$vars)
    res<-c(res,s)
    s_coef<-c(result$coef0)
    for (i in 1:p){
      if (i %in% s){
        res_coef[[i]] <- c(res_coef[[i]],s_coef[[1]])
        s_coef<-s_coef[-1]
      }else{res_coef[[i]]<-c(res_coef[[i]],0)}
    }
    
  }
  Var <- result$vars
  Coef <- result$coef0
  
  #compute ci
  qts<- list()
  for (i in 1:p){
    qts[[i]] = list(NULL)
  }
  
  for (i in 1:p){
    qts[[i]] <- quantile(unlist(res_coef[[i]]),c(0.025,0.975))
  }
  
  #compute cover probability
  cover = 0
  for (i in 1:p){
    if(beta[[i]] %in% (qts[[i]][1]:qts[[i]][2]))
    {cover = cover + 1}}
  prob_list <- c(prob_list,list(cover/p))
  real_seq <- c(real_seq, size)
  print(c("run complete",size))
  
}

#plot
plot(unlist(real_seq),unlist(prob_list), type="l", col="brown", lwd=3, xlab="bootstrap size", ylab="probability of covering true beta", main="Influence of bootstrap size")

