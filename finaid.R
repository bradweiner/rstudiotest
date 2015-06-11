

setwd("Z:/BW/OUE Projects/Projects/rstudiotest/")
options(scipen=999)
options(digits=2)
load(file = "Z:/BW/OUE Projects/Projects/Financial Aid Package Analysis/FileforAnalysis.rda")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)
data.backup<-data
data<-data.backup

descr<-data %>%
        filter(INTL_FLAG==0) %>%
        group_by(FTFTFALL13,ACAD_PROG_SDESC) %>%
        summarise(n=n(),
                  Mean=mean(G,na.rm=TRUE),
                  Median=median(G,na.rm=TRUE),
                  IQR=IQR(G,na.rm=TRUE),
                  SD=sd(G,na.rm=TRUE))

descr.tot<-data %>%
        filter(INTL_FLAG==0) %>%
        group_by(FTFTFALL13) %>%
        summarise(n=n(),
                  Mean=mean(G,na.rm=TRUE),
                  Median=median(G,na.rm=TRUE),
                  IQR=IQR(G,na.rm=TRUE),
                  SD=sd(G,na.rm=TRUE)) %>%
        mutate(ACAD_PROG_SDESC="TOTAL")

g<-rbind(descr,descr.tot)
g<-arrange(g,FTFTFALL13,ACAD_PROG_SDESC)


descr<-data %>%
        filter(INTL_FLAG==0) %>%
        group_by(FTFTFALL13,ACAD_PROG_SDESC) %>%
        summarise(n=n(),
                  Mean=mean(S,na.rm=TRUE),
                  Median=median(S,na.rm=TRUE),
                  IQR=IQR(S,na.rm=TRUE),
                  SD=sd(S,na.rm=TRUE))

descr.tot<-data %>%
        filter(INTL_FLAG==0) %>%
        group_by(FTFTFALL13) %>%
        summarise(n=n(),
                  Mean=mean(S,na.rm=TRUE),
                  Median=median(S,na.rm=TRUE),
                  IQR=IQR(S,na.rm=TRUE),
                  SD=sd(S,na.rm=TRUE)) %>%
        mutate(ACAD_PROG_SDESC="TOTAL")

s<-rbind(descr,descr.tot)
s<-arrange(g,FTFTFALL13,ACAD_PROG_SDESC)

descr<-data %>%
        filter(INTL_FLAG==0) %>%
        group_by(FTFTFALL13,ACAD_PROG_SDESC) %>%
        summarise(n=n(),
                  Mean=mean(L,na.rm=TRUE),
                  Median=median(L,na.rm=TRUE),
                  IQR=IQR(L,na.rm=TRUE),
                  SD=sd(L,na.rm=TRUE))

descr.tot<-data %>%
        filter(INTL_FLAG==0) %>%
        group_by(FTFTFALL13) %>%
        summarise(n=n(),
                  Mean=mean(L,na.rm=TRUE),
                  Median=median(L,na.rm=TRUE),
                  IQR=IQR(L,na.rm=TRUE),
                  SD=sd(L,na.rm=TRUE)) %>%
        mutate(ACAD_PROG_SDESC="TOTAL")

l<-rbind(descr,descr.tot)
l<-arrange(g,FTFTFALL13,ACAD_PROG_SDESC)

write.table(rbind(g,s,l),"clipboard",sep="\t",row.names = FALSE)
##PASTE FIRST


##LOGISTIC REGRESSION##
data<-filter(data,INTL_FLAG==0)
data[is.na(data)]<-0
data$COMP_ACT_SCORE<-ifelse(data$COMP_ACT_SCORE==0,NA,data$COMP_ACT_SCORE)
data$ACAD_PROG_SDESC<-relevel(data$ACAD_PROG_SDESC,ref="CLA")
data$HM_LOC_BAND<-relevel(data$HM_LOC_BAND,ref="TC Metro")
logit<-glm(FTFTFALL13~G+S+L+ACAD_PROG_SDESC+COMP_ACT_SCORE+HM_LOC_BAND,data=data,family=binomial(link="logit"))
logodds<-exp(cbind(OR = coef(logit), confint(logit)))
data$PREDICT.1<-predict(logit,data,type="response")
data$GROUP<-as.factor(ifelse(data$PREDICT.1>=.30,1,0))
write.table(logodds,"clipboard",sep="\t",row.names = TRUE)

##REALLY TOUGH TO SEE WHAT EFFECT MIGHT BE GOING ON HERE##

##PROBS for AVERGES##
newdata<-as.data.frame(data) %>%
        group_by(ACAD_PROG_SDESC,HM_LOC_BAND) %>%
        summarise(G=mean(G,na.rm=TRUE),
                  L=mean(L,na.rm=TRUE),
                  S=mean(S,na.rm=TRUE),
                  PRORATED_EFC_I=mean(PRORATED_EFC_I,na.rm=TRUE),
                  COMP_ACT_SCORE=mean(COMP_ACT_SCORE,na.rm=TRUE))
newdata$PREDICT<-predict(logit,newdata = newdata,type="response")
newdata



#DATA SHOWS THAT COLLEGE IS A MUCH MORE IMPORTANT FACTOR. SOMETHING IS WONKY HERE.

data.g.nozero<-filter(data,G>0)
logit.g.no.zero<-glm(FTFTFALL13~G+L+S+PRORATED_EFC_I+COMP_ACT_SCORE+ACAD_PROG_SDESC,data=data.g.nozero,family=binomial(link="logit"))
exp(cbind(OR = coef(logit.g.no.zero), confint(logit.g.no.zero)))

xtab<-table(data$FTFTFALL13,data$GROUP,dnn = c("Enrolled","Predicted"))
confusionMatrix(xtab,positive = "1")



#SCATTERPLOTS
levels(data$FTFTFALL13)<-c("Did Not Enroll","Enrolled")
f<-ggplot(data,aes(PRORATED_EFC_I,G))
a<-f+geom_point(aes(shape=FTFTFALL13,color=FTFTFALL13))+labs(color=NULL,shape=NULL,size=NULL)+scale_x_continuous(labels = dollar)+scale_y_continuous(labels = dollar)+labs(y="Grant",x="Imputed EFC")+ggtitle("Fall 2013 Enrollments by Grants and EFC\n")
ggsave(filename = "a.png",plot = a,width = 10, height = 7.5, units = "in")

f<-ggplot(data,aes(PRORATED_EFC_I,S))
b<-f+geom_point(aes(shape=FTFTFALL13,color=FTFTFALL13))+labs(color=NULL,shape=NULL,size=NULL)+scale_y_continuous(labels = dollar)+scale_x_continuous(labels = dollar)+labs(y="Scholarship",x="Imputed EFC")+ggtitle("Fall 2013 Enrollments by Scholarsips and EFC\n")
ggsave(filename = "b.png",plot = b,width = 10, height = 7.5, units = "in")

f<-ggplot(data,aes(PRORATED_EFC_I,L))
c<-f+geom_point(aes(shape=FTFTFALL13,color=FTFTFALL13))+labs(color=NULL,shape=NULL,size=NULL)+scale_y_continuous(labels = dollar)+scale_x_continuous(labels = dollar)+labs(y="Loans",x="Imputed EFC")+ggtitle("Fall 2013 Enrollments by Loans and EFC\n")
ggsave(filename = "c.png",plot = c,width = 10, height = 7.5, units = "in")

c.1<-grid.arrange(a,b,c,ncol=3)

##PREDICT SCATTERPLOTS (NOT USEFUL)
d<-ggplot(data,aes(G,PREDICT.1))+geom_point(aes(shape=factor(FTFTFALL13),color=factor(FTFTFALL13)))
monkey<-ggplot(data,aes(S,PREDICT.1))+geom_point(aes(shape=factor(FTFTFALL13),color=factor(FTFTFALL13)))
f<-ggplot(data,aes(L,PREDICT.1))+geom_point(aes(shape=factor(FTFTFALL13),color=factor(FTFTFALL13)))

##BOXPLOTS##
g<-ggplot(data,aes(factor(FTFTFALL13),G))+geom_boxplot(aes(fill=FTFTFALL13))+labs(y="Grant",x="Enrolled Fall 2013")+scale_y_continuous(labels=dollar,limits=c(0,50000),breaks=seq(0,50000,2000))
h<-ggplot(data,aes(factor(FTFTFALL13),S))+geom_boxplot(aes(fill=FTFTFALL13))+labs(y="Scholarships",x="Enrolled Fall 2013")+scale_y_continuous(labels=dollar,limits=c(0,50000),breaks=seq(0,50000,2000))
i<-ggplot(data,aes(factor(FTFTFALL13),L))+geom_boxplot(aes(fill=FTFTFALL13))+labs(y="Loans",x="Enrolled Fall 2013")+scale_y_continuous(labels=dollar,limits=c(0,50000),breaks=seq(0,50000,2000))
x<-grid.arrange(g,h,i,ncol=3,main = "Distribution of Aid Type")
y<-arrangeGrob(g,h,i,ncol=3)
ggsave(filename = "y.png",y,width = 10, height = 7.5, units = "in")
j<-g+facet_grid(.~ACAD_PROG_SDESC)
k<-h+facet_grid(.~ACAD_PROG_SDESC)
l<-i+facet_grid(.~ACAD_PROG_SDESC)


##DECISION TREE
library(rpart)
data$FTFTFALL13<-relevel(data$FTFTFALL13,ref = "Enrolled")
fit<-rpart(FTFTFALL13~G+L+S+PRORATED_EFC_I+COMP_ACT_SCORE+ACAD_PROG_SDESC,data=data,method="class")
fancyRpartPlot(fit,main = "Fall 2013 Financial Aid Decision Tree",dev)


