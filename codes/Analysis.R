#################In this file I perform the main analysis, generate all plots and tables######
rm(list=ls())
library(dplyr)
library(striprtf)
library(tikzDevice)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(xtable)
library(texreg)
library(here)
source(here("codes","functions.R"))
setwd(here("Results"))
#########Set Makro Parameters############
S=50###scalar,number of disciplines########################
N=1737###overall number of units###########################

data<-data.frame(data) ###read in your data##### 
######################The way the codes are written below, the data needs to be supplied in the following
####form: One row corresponds to one department############
####The columns correspond to: 
##"FE": number of female department members
## "M": number of male department members
## "Size": Total department size (FE+M)
## "Share": Total female share (FE/Size) on the department level
## "Code": discipline code/identifier 
names(data)<-c("FE","M","Size","Share","Code")
means<-data %>% 
  group_by(Code)%>% 
  dplyr::summarize(average = mean(Share))

data<-data.frame(data)
names(data)<-c("FE","M","Size","Share","Code")
code=data$Code

means<-data %>% 
  group_by(Code)%>% 
  dplyr::summarize(average = mean(Share))

C<-rbind(c("10", "Humanities"),c("20","Theology (protestant)"),c("30", "Theology (catholic)"),c("40" ,"Philosophy"), c("50" ,"History"),c("80" ,"Literature and comparative language studies"),c("100","German Studies"),c("110","English and American Studies"),c("120","Roman Language Studies"),c("140","Other Language/Cultural Studies"),c("160", "Cultural Studies"),c("200" ,"Sports"),c("220" ,"Law, Economics and Social Sciences, general"),c("230" ,"Political Sciences"),c("235" ,"Sociology"),c("240" , "Social Work"))
B<-rbind(c("250" ,"Law"),c("290","Economics"),c("315","Psychology"), c("320","Paedagogy"),c("330","STEM, general"),c("340", "Mathematics"),c("360","Physics and Astronomy"),c( "370" ,"Chemistry"),c( "390","Pharmacy"),c( "400" ,"Biology"), c("410","Geology" ),c("420" ,"Geography"), c( "445","Health Sciences, general"), c( "450","Preclinical Medicine"),c( "470","Clinical-theoretical Medicine"), c("490","Clinical-Practical Medicine" ),c( "560","Clinical-theoretical Veterinary Science"),c( "580","Clinical-practical Veterinary Science"))
A<- rbind(c("620","Agricultural Science"),c( "650","Dietary Studies"),c( "690", "Mechanical Engineering"),c( "710","Electrical Engineering"),c("730","Architecture"),c("740","Spatial Planning" ),c( "750","Civil Engineering"),c( "765","Computer Science" ),c( "770","Material Science"),c( "780" ,"Art, general"),c( "790","Visual Arts" ),c( "800","Design" ),c( "820" ,"Performing Arts"),c("830" ,"Music"),c( "920" ,"Central scientific Departments"),c(" 960","Institutes associated with Universities"))
subjects<-data.frame(rbind(C,B,A))


#########################------------------------------##################################################
########Read in Results from the joint test and make the deviation plot of the $B(z)$############
#########################------------------------------##################################################

######read in the table from the combined test########
res<-read.table("table_tests_all_new")
res
#############Make the shaded deviation plots###########################
df<-data.frame(round(res$V1,4),as.factor(res$V7),res$V4)
names(df)=c("dev","Number","val")
#######
g=ggplot(df, aes(x=Number,y=dev,fill=val)) +geom_col()
g+scale_fill_gradient(low = "black", high = "white", na.value = NA)+theme_bw()


tikz(file = "plot_res_main_test.tex", width = 7.2, height = 5.5,bg="white")
g1=ggplot(df, aes(x=Number,y=dev,fill=val)) +geom_col()+labs(x="Number of Women per Department",y="Deviation from expected Number",title="Deviation from expected Number of Departments with zero to 10 women",subtitle = "Mean over all 50 disciplines, shaded by p-value, $p_{min}=0.011,$ $p_{max}=0.27$")+ theme(legend.position='bottom')+theme(plot.margin=unit(c(0.1,0.85,0.1,0.85),"cm"))
#print(g1+scale_color_grey()+theme_bw()+labs(fill="P-Value"))
g1=g1+scale_fill_gradient(low = "black",high = "grey90", na.value = NA)+theme_bw()+labs(fill="P-Value")+theme(legend.position="none", legend.box = "horizontal")+ theme(plot.title = element_text(size=9,face="bold"), plot.subtitle=element_text(size=7,face="bold"),axis.title=element_text(size=6,face="bold"))
g1
dev.off()
#######Make the p-value tables###############################
tr <- createTexreg(coef.names = as.character(df$Number),
                   coef = round(df$dev,2),
                   #pvalues = as.numeric(df$val),
                   #se = as.numeric(df$val),
)
tr1<-createTexreg(coef.names = as.character(df$Number),
                  coef = df$val,
)
R=texreg(list(tr,tr1),center=T,  digits = 3, siunitx=T,dcolumn = F,file="table_p_val.tex", booktabs = T,use.packages = FALSE, return.string=T,caption = "Summed Deviations and p-values from a weighted- and unweighted joint Z-test" ,   float.pos = "h", label = "table_p_val",custom.model.names = c("Deviations","p-value"))
######################------------------------####################################
#####Generating the plot for the bootstrapped confidence bands####################
######################------------------------####################################
f_z=0:10
rows<-seq(1,1001,by=100)
rows2<-seq(100,1100,by=100)
B=100##number of Bootstrap repetitions
df.dev<-matrix(NA,11*B,2)
df.uc<-matrix(NA,11*B,3)
i=1
z=f_z[i]
dev<-read.table(paste0("dev",z))
dev<-cbind(dev,rep(z,length(dev)))
names(dev)=c("dev","freq")
uc<-read.table(paste0("uc",z))
uc <- uc[order(uc),] 
uc<-cbind(uc,rep(0,length(uc)),rep(z,length(uc)))
uc[5,3]<-3
uc[95,3]<-3
df_test=uc
for(i in 2:11)
{
  z=f_z[i]
  print(z)
  dev<-read.table(paste0("dev",z))
  dev<-cbind(dev,rep(z,length(dev)))
  names(dev)=c("dev","freq")
  uc<-read.table(paste0("uc",z))
  uc <- uc[order(uc),] 
  print(uc)
  uc<-cbind(uc,rep(z,length(uc)),rep(0,length(uc)))
  uc[5,3]<-3
  uc[95,3]<-3
  df_test=rbind(df_test,uc)
  print(df_test)
}
names(df_test)=c("dev","freq","real")
df.uc=df_test
write.table(df.uc, file="df_uc")
####below needs to be moved to analysis file####
df_uc<-read.table("df_uc")
df.uc<-data.frame(df_uc)
names(df.uc)=c("dev","freq","real")
##############
result<-read.table("result")
share<-c(sum(result$X1<0)/length(code),sum(result$X2<0)/length(code),sum(result$X3<0)/length(code),sum(result$X4<0)/length(code),sum(result$X5<0)/length(code),sum(result$X6<0)/length(code),sum(result$X7<0)/length(code),sum(result$X8<0)/length(code),sum(result$X9<0)/length(code),sum(result$X10<0)/length(code),sum(result$X1<0)/length(code))
share<-cbind(share,share>0.5)
share.diff= data.frame(trt=0:10,outcome=share[,1],dose=share[,2])
outcome=c(sum(result$X1),sum(result$X2),sum(result$X3),sum(result$X4),sum(result$X5),sum(result$X6),sum(result$X7),sum(result$X8),sum(result$X9),sum(result$X10),sum(result$X11))

negative=outcome<0
mean.fe<-data.frame(Frequency =c(0:10),outcome=c(sum(result$X1),sum(result$X2),sum(result$X3),sum(result$X4),sum(result$X5),sum(result$X6),sum(result$X7),sum(result$X8),sum(result$X9),sum(result$X10),sum(result$X11)),negative)

outcome1<-data.frame(outcome,as.factor(c(0:10)),rep(1,11))
names(outcome1)=c("dev","freq","real")

df.full<-rbind(outcome1,df.uc)
df.full$real[df.full$real==1]=4

tikz(file = "uc.tex", width = 7.2, height = 5.5,bg="white")
p <- ggplot(df.full, aes(freq, dev))+theme_bw()
p=p + geom_point(aes(colour = factor(real),shape=factor(real) ,size = 2,alpha = real))+scale_color_manual(name="Type",values=c("grey90", "grey40","grey1"), labels = c( "Simulated Dev.", "90 perc. Interval","Observation"))+ scale_shape_manual(name="Type",labels = c( "Simulated Dev.", "90 perc. Interval","Observation"), values = c(18, 17, 19))+labs(x="Number of women per Department",y="Deviation from expected value",title="Deviation from expected number of departments",subtitle="Including bootstrapped 90 percent bands")+guides(size="none",alpha="none",factor="none")+theme(legend.position="bottom", legend.box = "horizontal")+ theme(plot.title = element_text(size=9,face="bold"), plot.subtitle=element_text(size=7,face="bold"),axis.title=element_text(size=6,face="bold"))
p=p+ scale_x_discrete(breaks=c("0","1","2","3","4","5","6","7","8","9","10"),labels=c("0","1","2","3","4","5","6","7","8","9","10"))
p
dev.off()



#########################------------------------------##################################################
######Calculate the leave one out shares of women (where own department is excluded, to yield vector p_s^{loo})
#########################------------------------------##################################################

alt_means<-data  %>% group_by(Code) %>%
  mutate(special_mean = (sum(Share) - Share)/(n()-1))
###standard deviation of the leave-one-probabilities#######################
sd_means<-alt_means %>% group_by(Code) %>%
  mutate(special_sd = sd(special_mean),mean = mean(Share))
sd_means2<-sd_means %>%group_by(Code) %>%
  mutate(special_conf_high = (special_mean-mean+special_sd*2),special_conf_low = (special_mean-mean-special_sd*2))
dim(sd_means2)[1]-sum(0>sd_means2$special_conf_low&0<sd_means2$special_conf_high)
sd_means3<-sd_means2 %>%group_by(Code) %>%
  mutate(width=special_conf_high-special_conf_low,sig=1-0>special_conf_low&0<special_conf_high)
sd_means4<-sd_means3 %>% arrange(desc(width))
sd_means5<-data.frame(sd_means4,id = 1:N)
sd_means5<-data.frame(c(1:dim(sd_means2)[1],1:dim(sd_means2)[1]),c(sd_means4$special_conf_high,sd_means4$special_conf_low),c(rep("high",dim(sd_means2)[1]),rep("low",dim(sd_means2)[1])),c(sd_means4$sig,sd_means4$sig))
names(sd_means5)=c("Dep","Interval","Type","Sig")
#############Make the table with the standard deviations of the leave-one-out means#############
tab1<-data.frame(cbind(unique(sd_means$Code),unique(sd_means$special_sd),unique(sd_means$mean)))
names(tab1)=c("code", "sd","mean")
tr <- createTexreg(coef.names = subjects$X2,#as.character(tab1$code),
                   coef = tab1$sd)
tr1 <- createTexreg(coef.names = subjects$X2,
                    coef = tab1$mean)
model.names<-as.character(c("Standard Deviation, Leave-one-out means","Means"))
R1=texreg(list(tr,tr1),center=T,siunitx=F,dcolumn = F,digits=4, file="table_sd.tex", booktabs = T,use.packages = FALSE, return.string=T,caption = "Standard deviation leave-one-out means by discipline", float.pos = "h", label = " table_sd_llo",single.row=T,custom.model.names=model.names)


