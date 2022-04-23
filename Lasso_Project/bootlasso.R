#install.packages("selectiveInference")
data1 = read.csv("datadata.csv",as.is = TRUE)
library(glmnet)
library(selectiveInference)
if(!require('plyr')) {
  install.packages('plyr')
  library('plyr')
}
res<-c(NULL)
res_coef1<-c(NULL)
res_coef2<-c(NULL)
res_coef3<-c(NULL)
res_coef4<-c(NULL)
res_coef5<-c(NULL)
res_coef6<-c(NULL)
res_coef7<-c(NULL)
res_coef8<-c(NULL)
res_coef9<-c(NULL)
res_coef10<-c(NULL)
times=10
for (i in 1:times) {
  sample_d = data1[sample(1:nrow(data1), 442, replace = TRUE), ]
  data_x=sample_d[,c('AGE','SEX','BMI','BP','S1','S2','S3','S4','S5','S6')]
  data_y=sample_d[,c('Y')]
  data_x_normal <- as.data.frame(scale(data_x)) 
  data_y_normal <- as.data.frame(scale(data_y)) 
  xx_normal<-as.matrix(data_x_normal)
  yy_normal<-as.matrix(data_y_normal)
  xx<-as.matrix(data_x)
  yy<-as.matrix(data_y)
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

  gfit_normal = glmnet(xx_normal,yy_normal,thresh=1E-10,standardize=TRUE)
  n=dim(data_x)[1]
  beta_normal = coef(gfit_normal, x=xx_normal, y=yy_normal, s=average_best_lambda/n, exact=TRUE)[-1]

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
  
  s<-c(result_normal$vars)
  res<-c(res,s)
  s_coef<-c(result_normal$coef[,c(1)])
  

  if(1 %in% s){
    res_coef1<-c(res_coef1,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef1<-c(res_coef1,0)
  }
  
  if(2 %in% s){
    res_coef2<-c(res_coef2,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef2<-c(res_coef2,0)
  }
  
  if(3 %in% s){
    res_coef3<-c(res_coef3,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef3<-c(res_coef3,0)
  }
  
  if(4 %in% s){
    res_coef4<-c(res_coef4,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef4<-c(res_coef4,0)
  }
  
  if(5 %in% s){
    res_coef5<-c(res_coef5,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef5<-c(res_coef5,0)
  }
  
  if(6 %in% s){
    res_coef6<-c(res_coef6,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef6<-c(res_coef6,0)
  }
  
  if(7 %in% s){
    res_coef7<-c(res_coef7,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef7<-c(res_coef7,0)
  }
  
  if(8 %in% s){
    res_coef8<-c(res_coef8,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef8<-c(res_coef8,0)
  }
 
  if(9 %in% s){
    res_coef9<-c(res_coef9,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef9<-c(res_coef9,0)
  }
  
  if(10 %in% s){
    res_coef10<-c(res_coef10,s_coef[1])
    s_coef<-s_coef[-1]
  }else{
    res_coef10<-c(res_coef10,0)
  }

  

  
  
  
}
s<-count(res)

x<-s[,c('x')]

y<-s[,c('freq')]

plot(x,1-y/times,main='Prob of coef being zero',ylab='Probability',xlab='Var',col = "dark red")

#qts<-quantile(res_coef1,c(0.025,0.975))
#hist(res_coef1)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef2,c(0.025,0.975))
#hist(res_coef2)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef3,c(0.025,0.975))
#hist(res_coef3)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef4,c(0.025,0.975))
#hist(res_coef4)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef5,c(0.025,0.975))
#hist(res_coef5)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef6,c(0.025,0.975))
#hist(res_coef6)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef7,c(0.025,0.975))
#hist(res_coef7)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef8,c(0.025,0.975))
#hist(res_coef8)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef9,c(0.025,0.975))
#hist(res_coef9)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

#qts<-quantile(res_coef10,c(0.025,0.975))
#hist(res_coef10)
#abline(v=qts[1],col="red")
#abline(v=qts[2],col="red")

y

qts_1<-quantile(res_coef1,c(0.025,0.975))
qts_2<-quantile(res_coef2,c(0.025,0.975))
qts_3<-quantile(res_coef3,c(0.025,0.975))
qts_4<-quantile(res_coef4,c(0.025,0.975))
qts_5<-quantile(res_coef5,c(0.025,0.975))
qts_6<-quantile(res_coef6,c(0.025,0.975))
qts_7<-quantile(res_coef7,c(0.025,0.975))
qts_8<-quantile(res_coef8,c(0.025,0.975))
qts_9<-quantile(res_coef9,c(0.025,0.975))
qts_10<-quantile(res_coef10,c(0.025,0.975))

len <- 0
LowConfPt_bs <- vector(mode = "list", length = len)
UpConfPt_bs <- vector(mode = "list", length = len)
LowConfPt_bs <- c(qts_1[1],qts_2[1],qts_3[1],qts_4[1],qts_5[1],qts_6[1],qts_7[1],qts_8[1],qts_9[1],qts_10[1])


UpConfPt_bs<- c(qts_1[2],qts_2[2],qts_3[2],qts_4[2],qts_5[2],qts_6[2],qts_7[2],qts_8[2],qts_9[2],qts_10[2])
data_bs <- round(data.frame(x = c(1:10),
                            y = c(1:10),
                            lower = as.numeric(unlist(LowConfPt_bs)),
                            upper = as.numeric(unlist(UpConfPt_bs))), 2)


library("ggplot2")
colors <- c("Bootstrap interval" = "orange")
p <- ggplot(data_bs,aes(x,y)) +  
  geom_errorbar(aes(ymin = as.numeric(unlist(LowConfPt_bs)), ymax = as.numeric(unlist(UpConfPt_bs)),color="Bootstrap interval"),show.legend = TRUE)
p <- p + ylim(-1,1)
p <- p + scale_x_discrete(limits=c('AGE','SEX','BMI','BP','S1','S2','S3','S4','S5','S6'))
p <- p + theme(legend.title = element_blank())

p
