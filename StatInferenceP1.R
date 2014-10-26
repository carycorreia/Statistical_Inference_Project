############################################################################################
#
#     Coursera Course:  Data Science   
#     Specialty Course: Statistical Inference
#
#     Student:          Cary Correia
#     Date:             October 11, 2014
#
#############################################################################################
#
# Part 1
# 1.0 simulate rexp(0.2) a total of 40,000...this will be the raw datasource
set.seed(1984)
datasource<-0
for (i in 1:40000){
  datasource[i]<-rexp(1,0.2)
}
# 2.0 divide the raw datasource into subgroups.  We will have 1000 groups of 40 exps.
splitter<-rep(1:1000,40)
group<-split(datasource, splitter)
# 3.0 take the average of each group so that we will have a database of 40 averages
normal<-lapply(group, FUN=mean)
normal<-data.frame(unlist(normal))
# 4.0 plot the raw data
hist(datasource)

# 5.0 plot the raw distribution of 40,000 points.
library(ggplot2)
h<-hist(normal$unlist.normal)
xfit<-seq(5.0-4.5*sd(normal$unlist.normal), max(normal$unlist.normal), length=100)
yfit<-dnorm(xfit, mean=mean(normal$unlist.normal), sd(normal$unlist.normal))
yfit<-yfit*diff(h$mids[1:2])*length(normal$unlist.normal)
normal2<-data.frame(cbind(xfit, yfit))

ggplot(normal, aes(x=normal$unlist.normal)) +
        geom_histogram(binwidth=0.5, color='dark grey', fill='light pink') +
        geom_vline(xintercept=mean(normal$unlist.normal), color="dark red", lwd=1.5)+
        geom_line(aes(x=normal2$xfit, y=normal2$yfit), col="light blue", lwd=1.5)+
        theme_bw()+
        ggtitle(expression(atop("Distribution of Averages for 40 Exponential(0.2)", 
                                atop("with Mean(Red) and Normal Curve(Light Blue)"),""))) +
        theme(plot.title=element_text(size=25))+
        labs(x="Average Values", y="Frequency")+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title.x=element_text(size=15))+
        theme(axis.title.y=element_text(size=15))
        
# 6.0 mean of the distributon
meanval<-mean(normal$unlist.normal)
meanval

# 7.0 sd of the distribution
s.dev<-sd(normal$unlist.normal)
s.dev

# 8.0 actual std dev is 5...but with this subset....we should have 5/sqrt(subgroups)
s.dev.calc<-5/sqrt(40)
s.dev.calc

# 9.0 calculate the CI interval for the mean
CI<- meanval +c(-1,1)*qnorm(0.975)*s.dev/sqrt(length(normal$unlist.normal))

length(normal$unlist.normal)
normal$unlist.normal
norm.1CI<-ifelse(normal$unlist.normal>=4.93 & normal$unlist.normal<=5.03, normal$unlist.normal, NA)
norm.1CI.woNA<-NROW(na.omit(norm.1CI))

n<-100000
meanvalues<-seq(3.5, 6.00, by=0.05)
nosim<-1000
coverage<-sapply(meanvalues, function(meanval){
        mean.calc<-rnorm(nosim, mean=meanval, sd=s.dev)
        Il<-mean.calc-qnorm(0.975)*s.dev/sqrt(length(n))
        ul<-mean.calc+qnorm(0.975)*s.dev/sqrt(length(n))
        mean(Il < meanval & ul > meanval)
})
plot(coverage~meanvalues, type="l", ylim=c(.5,1.00), main="Coverage vs MeanValues") 
abline(a=NULL, b=0, h=0.95, v=NULL)
#######################################################################################################################
# Part 2:
# Purpose of project 2 is to analyze the ToothGrowth data in the R datasets Package.

# 1.0 Load the ToothGrowth data and perform some basic exploratory data analyses
data(ToothGrowth)
ggplot(ToothGrowth, aes(y=len, x=supp))+
        geom_boxplot(aes(fill=supp)) +
        facet_wrap(~dose)+
        theme_bw()+
        ggtitle(expression(atop("Exploratory Tooth Growth Boxplot Analysis",
                                atop("by Factors OJ, VC and Dose (0.5, 1.0 and 2.0)"),"")))+
        theme(plot.title=element_text(size=25))+
        labs(x="supp", y="Tooth Length")+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title.x=element_text(size=15))+
        theme(axis.title.y=element_text(size=15))
         
#2.0 Provide a basic summary of the data
data.summary<-data.frame(aggregate(len~supp + dose, 
            data=ToothGrowth, FUN=function(x) c(Average=mean(x), Median=median(x), Sdev=sd(x),  
                                                Min=min(x), Max=max(x), N=length(x))))
data.summary
    
#3.0 Use confidence intervals and hypothesis tests to compare tooth growth by supp and dose
#3.1 Test by Supplement (supp: OJ vs VC)
supptt<-t.test(len~supp, paired=FALSE, var.equal=F, data=ToothGrowth)

#Test by Dose (dose 0.5 vs 1, dose 0.5 vs 2.0 and dose 1.0 vs 2.0)
#3.2 a) Test by Dose (dose 0.5 vs 1)
Dose_dot5_to_1<-subset(ToothGrowth, dose %in% c(0.5,1.0))
dosepart1     <-t.test(len~dose, paired=FALSE, var.equal=F, data=Dose_dot5_to_1)


#3.2 b) Test by Dose (dose 0.5 vs 2.0)
Dose_dot5_to_2<-subset(ToothGrowth, dose %in% c(0.5, 2.0))
dosepart2     <-t.test(len~dose, paired=FALSE, var.equal=F, data=Dose_dot5_to_2)



#3.2 c) Test by Dose (dose 1.0 vs 2.0)
Dose_1_to_2<-subset(ToothGrowth, dose %in% c(1.0, 2.0))
dosepart3  <-t.test(len~dose, paired=FALSE, var.equal=F, data=Dose_1_to_2)

#3.3 a) Test by Supp across Dose; Test OJ vs VS for Dose 0.5
Dose_dot5  <-subset(ToothGrowth, dose==0.5)
suppbydose5<-t.test(len~supp, paired=FALSE, var.equal=F, data=Dose_dot5)


#3.3 b) Test by Supp across Dose; Test OJ vs VS for Dose 0.5
Dose_1_0   <-subset(ToothGrowth, dose==1.0)
suppbydose1<-t.test(len~supp, paired=FALSE, var.equal=F, data=Dose_1_0 )


#3.3 a) Test by Supp across Dose; Test OJ vs VS for Dose 0.5
Dose_2_0   <-subset(ToothGrowth, dose==2.0)
suppbydose2<-t.test(len~supp, paired=FALSE, var.equal=F, data=Dose_2_0)

#Section 3 Summary
tests<-cbind(c('OJ vs VC', 
               'Dose 0.5 vs 1.0', 'Dose 0.5 vs 2.0', 'Dose 1.0 vs 2.0',
               'OJ vs VC @ Dose 0.5', 'OJ vs VC @ Dose 1.0', 'OJ vs VC @ Dose 2.0'))
tests_name<-"Test Name"
pvalue<-cbind(c(supptt[3],
                dosepart1[3], dosepart2[3], dosepart3[3],
                suppbydose5[3], suppbydose1[3], suppbydose2[3]))
pvalue<-as.data.frame(pvalue)
pvalue_name<-"P-Value"
conf.int.lo<-cbind(c(supptt$conf.int[1],
                  dosepart1$conf.int[1], dosepart2$conf.int[1], dosepart3$conf.int[1],
                  suppbydose5$conf.int[1], suppbydose1$conf.int[1], suppbydose2$conf.int[1]))
conf.int.lo_name<-"Lower CI"
conf.int.hi<-cbind(c(supptt$conf.int[2],
                     dosepart1$conf.int[2], dosepart2$conf.int[2], dosepart3$conf.int[2],
                     suppbydose5$conf.int[2], suppbydose1$conf.int[2], suppbydose2$conf.int[2]))
conf.int.hi_name<-"Upper CI"
df<-data.frame(tests, pvalue, conf.int.lo, conf.int.hi)
colnames(df)<-c(tests_name, pvalue_name,conf.int.lo_name, conf.int.hi_name)
df$rejectHo<-c("No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")

#4.0 State your conclusions and the assumptions needed for your conclusions.
accept<-"There is no significant difference between "
reject<-"There is a significant difference between "
df$Conclusion<-ifelse(df$rejectHo=="No", paste(accept, df$'Test Name'), paste(reject, df$'Test Name'))
df
