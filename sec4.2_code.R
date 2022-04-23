#install.packages("selectiveInference")
data1 = read.csv("datadata.csv",as.is = TRUE)
data_x=data1[,c('AGE','SEX','BMI','BP','S1','S2','S3','S4','S5','S6')]
data_y=data1[,c('Y')]
xx<-as.matrix(data_x)
yy<-as.matrix(data_y)
library(glmnet)
lasso.model <- glmnet(xx_normal, yy_normal, alpha = 1, standardize = TRUE)
windows(width = 15, height = 20)
plot(lasso.model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:ncol(xx_normal), legend = colnames(xx), cex = 0.7)

lasso_cv <- cv.glmnet(xx, yy, alpha = 1, type.measure="mse",
                      standardize = TRUE, nfolds =10,family="gaussian")
windows(width = 12, height = 10)
plot(lasso_cv)
lasso_cv$lambda.min
lambda_cv <- lasso_cv$lambda.min
#cnt <- 0
# total_lambda <- 0
# repeat {
#   cv_model <- cv.glmnet(xx, yy, alpha = 1)
#   best_lambda <- cv_model$lambda.1se
#   total_lambda <- total_lambda + best_lambda
#   cnt <- cnt+1
#   if(cnt > 49) {
#     break
#   }
# }
# average_best_lambda = total_lambda/50
# gfit_normal = glmnet(xx_normal,yy_normal,thresh=1E-10,standardize=FALSE)
# n=dim(data_x)[1]
# beta_normal = coef(gfit, x=xx_normal, y=yy_normal, s=average_best_lambda/n, exact=TRUE)[-1]
# library(selectiveInference)
# result_normal = fixedLassoInf(xx_normal,yy_normal,beta_normal,average_best_lambda,
#                        family = c("gaussian", "binomial", "cox"),
#                        intercept=TRUE,
#                        add.targets=NULL,
#                        status=NULL,
#                        sigma=NULL,
#                        alpha=0.1,
#                        type=c("partial","full"),
#                        tol.beta=1e-5,
#                        tol.kkt=0.1,
#                        gridrange=c(-100,100),
#                        bits=NULL,
#                        verbose=FALSE,
#                        linesearch.try=10)
# 
# 
# Var <- result_normal$vars
# Var
gfit = glmnet(xx,yy,thresh=1E-10,standardize=FALSE)
beta = coef(gfit, x=xx, y=yy, s=lambda_cv/n, exact=TRUE)[-1]
result = fixedLassoInf(xx,yy,beta,lambda_cv,
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
Var <- result$vars
Var
Coef <- result$coef0
Coef
#Coef <- Coef_full[-c(1,6,8)]
LowConfPt <- result$ci[,1]
UpConfPt <- result$ci[,2]
#LowConfPt <- LowConfPt_full[-c(1,6,8)]
#UpConfPt <- UpConfPt_full[-c(1,6,8)]
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

result_ols <- ols_regress(yy~xx ,data = data1)
result_ols$conf_lm
Coef_ols = tail(result_ols$betas,-1)
Coef_ols
LowConfPt_ols_try <- result_ols$conf_lm[,1]
UpConfPt_ols_try <- result_ols$conf_lm[,2]
LowConfPt_ols <- tail(LowConfPt_ols_try,-1)
UpConfPt_ols<- tail(UpConfPt_ols_try,-1)
#LowConfPt_ols <- LowConfPt_ols_full[-c(1,6,8)]
#UpConfPt_ols <- UpConfPt_ols_full[-c(1,6,8)]
data_ols <- round(data.frame(x = Var,
                         y = Coef,
                         lower = LowConfPt_ols,
                         upper = UpConfPt_ols), 2)






colors <- c("OLS interval" = "blue", "LASSO interval" = "darkred")

p <- ggplot(data_ols, aes(Var, Coef)) +  
  geom_errorbar(aes(ymin = LowConfPt_ols, ymax = UpConfPt_ols,color="OLS interval"),show.legend = TRUE)+
  geom_errorbar(aes(ymin = LowConfPt, ymax = UpConfPt, color="LASSO interval"),show.legend = TRUE)
Points = data.frame(x=Var, y = Coef)
Points_ols = data.frame(x=Var, y = Coef_ols)
p <- p + geom_point(data=Points, size=1, aes(Var, Coef),color='brown')
p <- p + geom_point(data=Points_ols, size=1, aes(Var, Coef_ols),color='yellow')
p <- p + scale_x_discrete(limits=c('AGE','SEX','BMI','BP','S1','S2','S3','S4','S5','S6'))
p <- p + theme(legend.title = element_blank())

p


