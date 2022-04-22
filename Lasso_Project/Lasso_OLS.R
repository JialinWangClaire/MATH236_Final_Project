#install.packages("selectiveInference")
data1 = read.csv("datadata.csv",as.is = TRUE)
data_x=data1[,c('AGE','SEX','BMI','BP','S1','S2','S3','S4','S5','S6')]
data_y=data1[,c('Y')]
data_x_normal <- as.data.frame(scale(data_x)) 
data_y_normal <- as.data.frame(scale(data_y)) 
xx_normal<-as.matrix(data_x_normal)
yy_normal<-as.matrix(data_y_normal)
library(glmnet)
cnt <- 0
total_lambda <- 0
repeat {
  cv_model <- cv.glmnet(xx, yy, alpha = 1)
  best_lambda <- cv_model$lambda.1se
  total_lambda <- total_lambda + best_lambda
  cnt <- cnt+1
  if(cnt > 49) {
    break
  }
}
average_best_lambda = total_lambda/50
gfit_normal = glmnet(xx_normal,yy_normal,thresh=1E-10,standardize=FALSE)
n=dim(data_x)[1]
beta_normal = coef(gfit, x=xx_normal, y=yy_normal, s=average_best_lambda/n, exact=TRUE)[-1]
library(selectiveInference)
result_normal = fixedLassoInf(xx_normal,yy_normal,beta_normal,average_best_lambda,
                              family = c("gaussian", "binomial", "cox"),
                              intercept=TRUE,
                              add.targets=NULL,
                              status=NULL,
                              sigma=NULL,
                              alpha=0.1,
                              type=c("partial","full"),
                              tol.beta=1e-5,
                              tol.kkt=0.1,
                              gridrange=c(-100,100),
                              bits=NULL,
                              verbose=FALSE,
                              linesearch.try=10)


Var <- result_normal$vars
Var
Coef_full <- result$coef0
Coef <- Coef_full[-c(1,6,8)]
LowConfPt_full <- result$ci[,1]
UpConfPt_full <- result$ci[,2]
LowConfPt <- LowConfPt_full[-c(1,6,8)]
UpConfPt <- UpConfPt_full[-c(1,6,8)]
data <- round(data.frame(x = Var,
                         y = Coef,
                         lower = LowConfPt,
                         upper = UpConfPt), 2)

#install.packages("ggplot2")    
library("ggplot2")
#install.packages("olsrr")
library("olsrr")

AGE = data1[,c('AGE')]
SEX = data1[,c('SEX')]
BMI = data1[,c('BMI')]
BP = data1[,c('BP')]
S1 = data1[,c('S1')]
S2 = data1[,c('S2')]
S3 = data1[,c('S3')]
S4 = data1[,c('S4')]
S5 = data1[,c('S5')]
S6 = data1[,c('S6')]

result_ols <- ols_regress(yy_normal~xx_normal ,data = data1)
result_ols$conf_lm
LowConfPt_ols_try <- result_ols$conf_lm[,1]
UpConfPt_ols_try <- result_ols$conf_lm[,2]
LowConfPt_ols_full <- tail(LowConfPt_ols_try,-1)
LowConfPt_ols <- LowConfPt_ols_full[-c(1,6,8)]
UpConfPt_ols_full<- tail(UpConfPt_ols_try,-1)
UpConfPt_ols <- UpConfPt_ols_full[-c(1,6,8)]
data_ols <- round(data.frame(x = Var,
                             y = Coef,
                             lower = LowConfPt_ols,
                             upper = UpConfPt_ols), 2)
colors <- c("OLS interval" = "blue", "LASSO interval" = "darkred")

p <- ggplot(data_ols, aes(Var, Coef)) +  
  geom_errorbar(aes(ymin = LowConfPt_ols, ymax = UpConfPt_ols,color="OLS interval"),show.legend = TRUE)+
  geom_errorbar(aes(ymin = LowConfPt, ymax = UpConfPt, color="LASSO interval"),show.legend = TRUE)
p <- p + scale_x_discrete(limits=c('AGE','SEX','BMI','BP','S1','S2','S3','S4','S5','S6'))
p <- p + theme(legend.title = element_blank())

p

