library(ggplot2)
library(ggpubr)
library(survival)
library(survminer)
library(timeROC)
getwd()
setwd()
rawdata<-read.csv() 
dev = rawdata 
mydatakm2 <- dev
fitkm2 <- survfit(Surv(OS,S)~ Group,
                  data = dev)
ggsurvplot(fitkm2,
           conf.int=F,
           pval = T,
           risk.table.col = "strata",
           palette=c("red", "blue","pink","BLACK"),
           ylab = "Cumulation OS analysis",
           xlab="Time(Years)",
           break.time.by = 1,
           surv.median.line = "hv",
           risk.table = T,
           legend.labs=
             c("Group 1","Group 2","Group 3","Group 4"))

median_RFS <- surv_median(fitkm2)
print(median_RFS)

time_points <- c(1,3, 5, 8)

RFS_rates <- summary(fitkm2, times = time_points)$surv
print(RFS_rates)
####################################################################
library(foreign)
library(survival) 
library(glmnet) 
library(randomForestSRC)
library(survival)
library (survminer)
library(randomForest)
setwd()
rawdata<-read.csv()        

set.seed(1000)
rfsrc<- rfsrc(Surv(OS, S) ~ ., data = rawdata, ntree =1400, nsplit =1,na.action = "na.impute", tree.err = TRUE, importance = TRUE)
tiff(file="Train.tiff",width=44,height=25,units="cm",compression="lzw",bg="white",res=1000)
plot(rfsrc)
dev.off()

##################################################################
library(foreign)
library(survival) 
library(glmnet)  
library(randomForestSRC)
setwd()
rawdata<-read.csv()          
dev = rawdata
x<-as.matrix(dev[,c(4:45)])

y<-data.matrix(Surv(dev$OS,dev$S))

fit<-glmnet(x,y,family = "cox")
cairo_pdf("output_cairo.pdf", width=12, height=8)
plot(fit, xvar="lambda", label=F, xlab=expression(Log(λ)))
coef_names <- colnames(x)
legend("topright", inset=c(0.02, 0.02),  
       legend=colnames(x),               
       col=1:length(coef_names),         
       lty=1,                            
       cex=0.6,                          
       ncol=2,                           
       bty="n") 
dev.off()


cv.fit<-cv.glmnet(x,y,family="cox")
plot(cv.fit)
abline(v=log(c(cv.fit$lambda.min,cv.fit$lambda.1se)),lty=2,lwd=1.5)
cv.fit$lambda.min
Coefficients <- coef(fit, s = cv.fit$lambda.min)
Active.Index <- which(Coefficients != 0)
Active.Coefficients <- Coefficients[Active.Index]
Active.Index
Active.Coefficients
row.names(Coefficients)[Active.Index]
###################################################
library(nomogramFormula)
library(foreign)    
library(rms)         
library(survival)    
setwd()
rawdata<-read.csv()      

rawdata<-as.data.frame(rawdata)
dev = rawdata[rawdata$group==1,]
dd<-datadist(dev)
options(datadist = "dd")

fcox<-cph(Surv(OS,S)~ Mon + Palb + Glob,x=T,y=T,surv = T,data = dev)
surv<-Survival(fcox)
surv1<-function(x)surv(1*3,lp=x)   
surv2<-function(x)surv(1*5,lp=x)   
surv3<-function(x)surv(1*8,lp=x)  

nomocox<-nomogram(fcox,
                  fun=list(surv1,surv2,surv3),
                  lp=FALSE,
                  funlabel = c("3-year OS","5-year OS","8-year OS"),
                  maxscale = 100,   
                  fun.at = c("0.9","0.85","0.80","0.70","0.60","0.50","0.40","0.30","0.20","0.10"))


plot(nomocox)
###################################################
library(ggplot2)
library(ggpubr)
library(survival)
library(survminer)
setwd()     
rawdata<-read.csv() 
rawdata<-na.omit(rawdata)   
dev = rawdata[rawdata$group==1,]     
vad = rawdata[rawdata$group==2,]     

mydatakm2 <- dev
fitkm2 <- survfit(Surv(OS,S)~ stage,
                  data = dev)
ggsurvplot(fitkm2,
           conf.int = F,
           pval = T,
           risk.table.col = "strata",
           palette=c("red", "black"),
           ylab = "Cumulation OS analysis",
           xlab="Time(Years)",
           break.time.by = 1,
           surv.median.line = "hv",
           risk.table = T,
           legend.labs=
             c("Low Risk","High Risk"))

median_RFS <- surv_median(fitkm2)
print(median_RFS)

time_points <- c(3, 5, 8)

RFS_rates <- summary(fitkm2, times = time_points)$surv
print(RFS_rates)


fitkm2 <- survfit(Surv(OS,S)~ stage,
                  data = vad)
ggsurvplot(fitkm2,
           conf.int = F,
           pval = T,
           risk.table.col = "strata",
           palette=c("red", "black"),
           ylab = "Cumulation OS analysis",
           xlab="Time(Years)",
           break.time.by = 1,
           surv.median.line = "hv",
           risk.table = T,
           legend.labs=
             c("Low Risk","High Risk"))

median_RFS <- surv_median(fitkm2)
print(median_RFS)

time_points <- c(3, 5, 8)

RFS_rates <- summary(fitkm2, times = time_points)$surv
print(RFS_rates)

#################################################################
library(survival)
library(survminer)
library(timeROC)
library(foreign)  
setwd()
rt<-read.csv()        
rt<-na.omit(rt)
dev = rt[rt$group==1,]
vad = rt[rt$group==2,]

var="stage"

ROC_rt=timeROC(T=dev$OS, delta=dev$S,
               marker=dev[,var], cause=1,
               weighting='aalen',
               times=c(3,5,8), ROC=TRUE)
plot(ROC_rt,time=3,col='red',title=FALSE,lty=1,lwd=2)
plot(ROC_rt,time=5,col='blue',add=TRUE,title=FALSE,lwd=2)
plot(ROC_rt,time=8,col='BLACK',add=TRUE,title=FALSE,lwd=2)

legend('bottomright',
       c(paste0('3-year AUC: ',sprintf("%.03f",ROC_rt$AUC[1])),
         paste0('5-year AUC: ',sprintf("%.03f",ROC_rt$AUC[2])),
         paste0('8-year AUC: ',sprintf("%.03f",ROC_rt$AUC[3]))),
       col=c('red',"blue",'BLACK'),lty=1,lwd=2)

ROC_rt=timeROC(T=vad$OS, delta=vad$S,
               marker=vad[,var], cause=1,
               weighting='aalen',
               times=c(3,5,8), ROC=TRUE)

plot(ROC_rt,time=3,col='red',title=FALSE,lty=1,lwd=2)
plot(ROC_rt,time=5,col='blue',add=TRUE,title=FALSE,lwd=2)
plot(ROC_rt,time=8,col='BLACK',add=TRUE,title=FALSE,lwd=2)

legend('bottomright',
       c(paste0('3-year AUC: ',sprintf("%.03f",ROC_rt$AUC[1])),
         paste0('5-year AUC: ',sprintf("%.03f",ROC_rt$AUC[2])),
         paste0('8-year AUC: ',sprintf("%.03f",ROC_rt$AUC[3]))),
       col=c('red',"blue",'BLACK'),lty=1,lwd=2)

model1<-coxph(Surv(OS,S==1)~stage ,data=dev)
summary(model1)

L<-0.712-1.96*0.025
L
U<-0.712+1.96*0.025
U


model2<-coxph(Surv(OS,S==1)~stage ,data=vad)
summary(model2)

L<-0.719 -1.96*0.036
L
U<-0.719 +1.96*0.036
U
#######################################################################################    
library(rms)
library(survival)
library(foreign) 
setwd()
rawdata<-read.csv()       
rawdata<-na.omit(rawdata)   
dev = rawdata[rawdata$group==1,]     
vad = rawdata[rawdata$group==2,] 
units(dev$OS)<-"Years"
dd<- datadist(dev)
options(datadist='dd')

coxm_1 <- cph(Surv(OS,S)~ Glob + Mon+Palb,surv=T,x=T,y=T,
              time.inc = 3,     
              data = dev)
cal_1<-calibrate(coxm_1,u=3,cmethod='KM',m=50,B=1000)
par(mar=c(7,4,4,3),cex=1.0)
plot(cal_1,lwd=2,lty=1, 
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), 
     xlab='Nomogram-Predicted Probability of 3-year OS',
     ylab='Actual 3-year OS(proportion)',
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1)) 

abline(0,1,lty=3,lwd=2,col="black")


coxm_2 <- cph(Surv(OS,S)~ Glob + Mon+Palb,surv=T,x=T,y=T,
              time.inc = 5,      
              data = dev)
cal_2<-calibrate(coxm_2,u=5,cmethod='KM',m=50,B=1000)
plot(cal_2,lwd=2,lty=1,  
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), 
     xlab='Nomogram-Predicted Probability of 5-year 0S',
     ylab='Actual 5-year OS(proportion)',
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1)) 
abline(0,1,lty=3,lwd=2,col="black")



coxm_3 <- cph(Surv(OS,S)~ Glob + Mon+Palb,surv=T,x=T,y=T,
              time.inc = 8,     
              data = dev)
cal_3<-calibrate(coxm_3,u=8,cmethod='KM',m=50,B=1000)
plot(cal_3,lwd=2,lty=1,  
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), 
     xlab='Nomogram-Predicted Probability of 8-year OS',
     ylab='Actual 8-year OS(proportion)',
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1)) 
abline(0,1,lty=3,lwd=2,col="black")


##########################
units(vad$OS)<-"Year"
dd<- datadist(vad)
options(datadist='dd')
coxm_1 <- cph(Surv(OS,S)~ Glob + Mon+Palb,surv=T,x=T,y=T,
              time.inc = 3,      
              data = vad)
cal_1<-calibrate(coxm_1,u=3,cmethod='KM',m=20,B=1000)

par(mar=c(7,4,4,3),cex=1.0)
plot(cal_1,lwd=2,lty=1, 
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), 
     xlab='Nomogram-Predicted Probability of 3-year OS',
     ylab='Actual 3-year OS(proportion)',
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1)) 
abline(0,1,lty=3,lwd=2,col="black")


coxm_2 <- cph(Surv(OS,S)~ Glob + Mon+Palb,surv=T,x=T,y=T,
              time.inc = 5,      
              data = vad)
cal_2<-calibrate(coxm_2,u=5,cmethod='KM',m=20,B=1000)
plot(cal_2,lwd=2,lty=1,  
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), 
     xlab='Nomogram-Predicted Probability of 5-year OS',
     ylab='Actual 5-year OS(proportion)',
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1)) 
abline(0,1,lty=3,lwd=2,col="black")

coxm_3 <- cph(Surv(OS,S)~ Glob + Mon+Palb,surv=T,x=T,y=T,
              time.inc = 8,      
              data = vad)
cal_3<-calibrate(coxm_3,u=8,cmethod='KM',m=30,B=1000)
plot(cal_3,lwd=2,lty=1,  
     errbar.col=c(rgb(0,118,192,maxColorValue = 255)), 
     xlab='Nomogram-Predicted Probability of 8-year OS',
     ylab='Actual 8-year OS(proportion)',
     col=c(rgb(192,98,83,maxColorValue = 255)),
     xlim = c(0,1),ylim = c(0,1)) 
abline(0,1,lty=3,lwd=2,col="black")

######################################################################################    
setwd()
library(foreign)      
library(rms)
library(survival)
library(rmda)
#install.packages("DecisionCurve")
source("stdca.R")
rawdata<-read.csv()       
rawdata<-na.omit(rawdata)   

dev = rawdata[rawdata$group==1,]     
vad = rawdata[rawdata$group==2,] 

fcox<-coxph(Surv(OS,S)~ Glob + Mon+Palb,
            data = dev)
fcox2<-coxph(Surv(OS,S)~ Glob + Mon+Palb,
             data = vad)

dev$Nomogram<-c(1-summary(survfit(fcox,newdata=dev),times = 3)$surv)
dca1<-stdca(data = dev,
            outcome = "S",
            ttoutcome = "OS",
            timepoint = 3,
            predictors = "Nomogram",
            xstop = 0.5,      
            smooth = TRUE)

dev$Nomogram<-c(1-summary(survfit(fcox,newdata=dev),times = 5)$surv)
dca2<-stdca(data = dev,
            outcome = "S",
            ttoutcome = "OS",
            timepoint = 5,
            predictors = "Nomogram",
            xstop = 0.8,      
            smooth = TRUE)
dev$Nomogram<-c(1-summary(survfit(fcox,newdata=dev),times = 8)$surv)
dca3<-stdca(data = dev,
            outcome = "S",
            ttoutcome = "OS",
            timepoint = 8,
            predictors = "Nomogram",
            xstop = 0.84,      
            smooth = TRUE)


vad$Nomogram<-c(1-summary(survfit(fcox2,newdata=vad),times = 3)$surv)
dca4<-stdca(data = vad,
            outcome = "S",
            ttoutcome = "OS",
            timepoint = 3,
            predictors = "Nomogram",
            xstop = 0.4,            
            smooth = TRUE)
vad$Nomogram<-c(1-summary(survfit(fcox2,newdata=vad),times = 5)$surv)
dca5<-stdca(data = vad,
            outcome = "S",
            ttoutcome = "OS",
            timepoint = 5,
            predictors = "Nomogram",
            xstop = 0.5,            
            smooth = TRUE)
vad$Nomogram<-c(1-summary(survfit(fcox2,newdata=vad),times = 8)$surv)
dca6<-stdca(data = vad,
            outcome = "S",
            ttoutcome = "OS",
            timepoint = 8,
            predictors = "Nomogram",
            xstop = 0.55,            
            smooth = TRUE)

