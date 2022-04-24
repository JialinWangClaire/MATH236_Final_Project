library(glmnet)
library(selectiveInference)
library("ggplot2")
if(!require('plyr')) {
  install.packages('plyr')
  library('plyr')
}

#initialize
n = 30
p = 50
times = 50
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


#fit model
real_times = 0 
res <- NULL
res_coef<- list()
for (i in 1:p){
  res_coef[[i]] = list(NULL)
}


for (i in 1:times) {
  num = sample(c(1:n),size=p,replace=T)
  data_x = NULL
  data_y = NULL
  for (j in 1:n){
    data_x <- c(data_x,xx[as.vector(num)[j],])
    data_y <- c(data_y, yy[as.vector(num)[j]])
  }
  data_xx <- t(matrix(data_x,nrow = p))
  data_yy <- matrix(data_y)

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
  real_times = real_times + 1
  result_normal = fixedLassoInf(data_xx,data_yy,beta_normal,lambda_cv,
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
  
  s<-c(result_normal$vars)
  res<-c(res,s)
  s_coef<-c(result_normal$coef0)
  for (i in 1:p){
    if (i %in% s){
      res_coef[[i]] <- c(res_coef[[i]],s_coef[[1]])
      s_coef<-s_coef[-1]
    }else{res_coef[[i]]<-c(res_coef[[i]],0)}
  }
  
}

s_final<-count(res)

x<-s_final[,c('x')]

y<-s_final[,c('freq')]
s_final
plot(x,1-y/real_times,main='Prob of coef being zero',ylab='Probability',xlab='Var',col = "dark red")

qts<- list()
for (i in 1:p){
  qts[[i]] = list(NULL)
}


for (i in 1:p){
  qts[[i]] <- quantile(unlist(res_coef[[i]]),c(0.025,0.975))
}
len <- 0
LowConfPt_bs <- vector(mode = "list", length = len)
UpConfPt_bs <- vector(mode = "list", length = len)

for (i in 1:p){
  LowConfPt_bs <- c(LowConfPt_bs,qts[[i]][1])
  UpConfPt_bs<- c(UpConfPt_bs,qts[[i]][2])
}

data_bs <- round(data.frame(x = c(1:p),
                            y = c(1:p),
                            lower = as.numeric(unlist(LowConfPt_bs)),
                            upper = as.numeric(unlist(UpConfPt_bs))), 2)


library("ggplot2")
colors <- c("Bootstrap interval" = "orange")
p1 <- ggplot(data_bs, aes(x,y)) +  
  geom_errorbar(aes(ymin = as.numeric(unlist(LowConfPt_bs)), ymax = as.numeric(unlist(UpConfPt_bs)),color="Bootstrap interval"),show.legend = TRUE)
Points = data.frame(x=c(1:p), 
                    y = unlist(beta))
p1 <- p1 + geom_point(data=Points, size=1, aes(x,y),color='lightblue')
p1 <- p1 + theme(legend.title = element_blank())
p1



