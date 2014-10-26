Statistical Inference Course Project
=====================================
### Student: Cary C
### Date:    October 24, 2014

This is the project for the statistical inference class.  
There are two parts to this project.  
In part 1 we will use simulation to explore inference and do some simple inferential data analysis.  
In part 2 we will use basic inferential data analysis and explore some Tooth Growth data.



## Part 2-Basic Inferential Analysis on the ToothGrowth dataset
### 2-1.0 Load the ToothGrowth data and perform some basic exploratory data analyses

```r
data(ToothGrowth)
```
### The Data Summary

```r
data.summary<-data.frame(aggregate(len~supp + dose, 
            data=ToothGrowth, FUN=function(x) c(Average=mean(x), Median=median(x), Sdev=sd(x),  
                                                Min=min(x), Max=max(x), N=length(x))))
data.summary
```

```
##   supp dose len.Average len.Median len.Sdev len.Min len.Max  len.N
## 1   OJ  0.5      13.230     12.250    4.460   8.200  21.500 10.000
## 2   VC  0.5       7.980      7.150    2.747   4.200  11.500 10.000
## 3   OJ  1.0      22.700     23.450    3.911  14.500  27.300 10.000
## 4   VC  1.0      16.770     16.500    2.515  13.600  22.500 10.000
## 5   OJ  2.0      26.060     25.950    2.655  22.400  30.900 10.000
## 6   VC  2.0      26.140     25.950    4.798  18.500  33.900 10.000
```
#### Figure 2-1.1 Data Summary for ToothGrowth
### From Figure 2-1.1 we can see the following:
- ToothGrowth has N=60 points
- the points are divided up into 10 subgroups
- each subgroup is composed of two factors: supp= OJ or VC and dose = 0.5 or 1.0 or 2.0
- basic statistics are available in the chart

### Basic BoxPlots

```r
library(ggplot2)
ggplot(aes(x=supp, y=len), data=ToothGrowth)+
        geom_boxplot(aes(fill=supp))+
        theme_bw()+
        ggtitle(expression(atop("Exploratory Tooth Growth Boxplot Analysis",
                                atop("by Supp = OJ or VC"),"")))+
        theme(plot.title=element_text(size=20))+
        labs(x="supp", y="Tooth Length")+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title.x=element_text(size=15))+
        theme(axis.title.y=element_text(size=15))
```

![plot of chunk boxplots](figure/boxplots.png) 
#### Figure 2-1.2 Tooth Length vs Supp (OJ or VC)
#### Preliminary analysis shows that OJ and VC seem to have very little difference between the two with respect to toothgrowth

```r
ggplot(aes(x=factor(dose), y=len), data=ToothGrowth)+
        geom_boxplot(aes(fill=factor(dose)))+
        theme_bw()+
        ggtitle(expression(atop("Exploratory Tooth Growth Boxplot Analysis",
                                atop("by Dose 0.5, 1.0, 2.0"),"")))+
        theme(plot.title=element_text(size=20))+
        labs(x="dose", y="Tooth Length")+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title.x=element_text(size=15))+
        theme(axis.title.y=element_text(size=15))
```

![plot of chunk doseBP](figure/doseBP.png) 
#### Figure 2-1.3 Tooth Length vs Dose (0.5, 1.0, 2.0)
#### Preliminary analysis shows that Tooth Length does seem to very with respect to doseage.  As doseage increases so does Tooth length.


```r
ggplot(ToothGrowth, aes(y=len, x=supp))+
        geom_boxplot(aes(fill=supp)) +
        facet_wrap(~dose)+
        theme_bw()+
        ggtitle(expression(atop("Exploratory Tooth Growth Boxplot Analysis",
                                atop("by Factors OJ, VC and Dose (0.5, 1.0 and 2.0)"),"")))+
        theme(plot.title=element_text(size=20))+
        labs(x="supp", y="Tooth Length")+
        theme(axis.text.x=element_text(size=15))+
        theme(axis.text.y=element_text(size=15))+
        theme(axis.title.x=element_text(size=15))+
        theme(axis.title.y=element_text(size=15))
```

![plot of chunk comboBP](figure/comboBP.png) 
#### Figure 2-1.4 Tooth Length vs Supp and Dose
#### Preliminary analysis shows that we still have Tooth Length varying with dose but we can also see that at dose 2.0.0 that the OJ and VC subsamples have little variability. 

### 2-3.0 Confidence Intervals and Tests
#### In this section we will attempt to calculate whether the variance we see in the graphs are statistically significant or not.
#### There will be 7 total tests based on what we saw with the basic graph;  note: code is shown below but we will summarize in table with our conclusions
- Test 1:  Tooth Length by OJ or VC
- Test 2:  Tooth Length by Dose: 0.5 vs 1.0
- Test 3:  Tooth Length by Dose: 0.5 vs 2.0
- Test 4:  Tooth Length by Dose: 1.0 vs 2.0
- Test 5:  Tooth Length by OJ or VC for Dose =0.5
- Test 6:  Tooth Length by OJ or VC for Dose =1.0
- Test 7:  Tooth Length by OJ or VC for Dose =2.0


```
## Warning: some row.names duplicated: 2,3,4,5,6,7 --> row.names NOT
## used
```

```r
df
```

```
##             Test Name   P-Value Lower CI Upper CI rejectHo
## 1            OJ vs VC   0.06063   -0.171    7.571       No
## 2     Dose 0.5 vs 1.0 1.268e-07  -11.984   -6.276      Yes
## 3     Dose 0.5 vs 2.0 4.398e-14  -18.156  -12.834      Yes
## 4     Dose 1.0 vs 2.0 1.906e-05   -8.996   -3.734      Yes
## 5 OJ vs VC @ Dose 0.5  0.006359    1.719    8.781      Yes
## 6 OJ vs VC @ Dose 1.0  0.001038    2.802    9.058      Yes
## 7 OJ vs VC @ Dose 2.0    0.9639   -3.798    3.638       No
```

```r
df.result
```

```
##                 tests
## 1            OJ vs VC
## 2     Dose 0.5 vs 1.0
## 3     Dose 0.5 vs 2.0
## 4     Dose 1.0 vs 2.0
## 5 OJ vs VC @ Dose 0.5
## 6 OJ vs VC @ Dose 1.0
## 7 OJ vs VC @ Dose 2.0
##                                                        Conclusion
## 1            There is no significant difference between  OJ vs VC
## 2      There is a significant difference between  Dose 0.5 vs 1.0
## 3      There is a significant difference between  Dose 0.5 vs 2.0
## 4      There is a significant difference between  Dose 1.0 vs 2.0
## 5  There is a significant difference between  OJ vs VC @ Dose 0.5
## 6  There is a significant difference between  OJ vs VC @ Dose 1.0
## 7 There is no significant difference between  OJ vs VC @ Dose 2.0
```
#### Figure 2-1.5 Table Summary for all 7 T-Tests

### From Figure 2-1.5 we see see the following from our testing:
- when 0 is between the lower CI and upper CI we cannot reject Ho and can conclude that there is a significant difference in the factors
- this is also confirmed when the p-value is less than our p-cutoff (5% in this case)

### Conclusions:
#### If you ignore dose then there is no significant difference between supp = OJ or VC:
- this corresponds to Test 1 and we suspected there was no difference by looking at Figure 2-1.2
- note that the p-value>0.5 and that 0 was captured between lower CI and upper CI

#### If you ignore supp then as Dose varies from 0.5 - 2.0 there is a significant impact on Tooth Growth.  Higher doses = Longer teeth
- this outcome corresponds to Tests 2-4; from Figure 2-1.3 we can see the +ve impact that dose has on tooth growth
- note that the p-values <0.5 and that 0 is not captured between lower CI and upper CI

#### When you look at both supp and dose there is a significant difference but only when dose=0.5 or 1.0 not at 2.0
- this outcome corresponds to Test 5-7; from Figure 2-1.4 we can see that there is no difference between OJ & VC at dose = 2.0
- the shift in Tooth length at Dose 2.0 is most probably due to the sole influence of the high dose
- OJ at doses at 0.5 or 1.0 has a higher impact on tooth growth than VC

### Assumptions:
- We will assume that there was no biased sampling with respect to the selection of test subjects-> random sampling is preferred
- We will assume that accurate measurements were taken and that equipment was calibrated and not switched out between measures
- We may assume that other Factors didn't play into the data ie: size, age or health of subjects could affect the results as well
- T Tests were set with paired = False because the samples must be independent as test subjects can't be tested with different protocols
- T Tests were set with var.equal=F because we assume that the populations of test subjects were different

## Appendix
### Code for T Tests and Confidence Intervals

```r
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
```

```
## Warning: some row.names duplicated: 2,3,4,5,6,7 --> row.names NOT
## used
```

```r
colnames(df)<-c(tests_name, pvalue_name,conf.int.lo_name, conf.int.hi_name)
df$rejectHo<-c("No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")

#4.0 State your conclusions and the assumptions needed for your conclusions.
accept<-"There is no significant difference between "
reject<-"There is a significant difference between "
df.result<-data.frame(tests)
df.result$Conclusion<-ifelse(df$rejectHo=="No", paste(accept, df$'Test Name'), paste(reject, df$'Test Name'))
```

```r
df
```

```
##             Test Name   P-Value Lower CI Upper CI rejectHo
## 1            OJ vs VC   0.06063   -0.171    7.571       No
## 2     Dose 0.5 vs 1.0 1.268e-07  -11.984   -6.276      Yes
## 3     Dose 0.5 vs 2.0 4.398e-14  -18.156  -12.834      Yes
## 4     Dose 1.0 vs 2.0 1.906e-05   -8.996   -3.734      Yes
## 5 OJ vs VC @ Dose 0.5  0.006359    1.719    8.781      Yes
## 6 OJ vs VC @ Dose 1.0  0.001038    2.802    9.058      Yes
## 7 OJ vs VC @ Dose 2.0    0.9639   -3.798    3.638       No
```

```r
df.result
```

```
##                 tests
## 1            OJ vs VC
## 2     Dose 0.5 vs 1.0
## 3     Dose 0.5 vs 2.0
## 4     Dose 1.0 vs 2.0
## 5 OJ vs VC @ Dose 0.5
## 6 OJ vs VC @ Dose 1.0
## 7 OJ vs VC @ Dose 2.0
##                                                        Conclusion
## 1            There is no significant difference between  OJ vs VC
## 2      There is a significant difference between  Dose 0.5 vs 1.0
## 3      There is a significant difference between  Dose 0.5 vs 2.0
## 4      There is a significant difference between  Dose 1.0 vs 2.0
## 5  There is a significant difference between  OJ vs VC @ Dose 0.5
## 6  There is a significant difference between  OJ vs VC @ Dose 1.0
## 7 There is no significant difference between  OJ vs VC @ Dose 2.0
```







