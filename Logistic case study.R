#Import Proactive File

proactive <- read.csv("Proactive Attrition Management-Logistic Regression Case Study.csv")
View(proactive)
str(proactive)

#Descriptive analysis
var_Summ=function(x){
  if(class(x)=="numeric"){
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    mean<-mean(x,na.rm=T)
    std<-sd(x,na.rm=T)
    var<-var(x,na.rm=T)
    min<-min(x,na.rm=T)
    p1<-quantile(x,0.01,na.rm=T)
    p5<-quantile(x,0.05,na.rm=T)
    p10<-quantile(x,0.1,na.rm=T)
    q1<-quantile(x,0.25,na.rm=T)
    q2<-quantile(x,0.5,na.rm=T)
    q3<-quantile(x,0.75,na.rm=T)
    p90<-quantile(x,0.9,na.rm=T)
    p95<-quantile(x,0.95,na.rm=T)
    p99<-quantile(x,0.99,na.rm=T)
    max<-max(x,na.rm=T)
    UC1=mean(x,na.rm=T)+3*sd(x,na.rm=T)
    LC1=mean(x,na.rm=T)-3*sd(x,na.rm=T)
    UC2=quantile(x,0.99,na.rm=T)
    LC2=quantile(x,0.01,na.rm=T)
    iqr=IQR(x,na.rm=T)
    UC3=q3+1.5*iqr
    LC3=q1-1.5*iqr
    ot1<-max>UC1 | min<LC1 
    ot2<-max>UC2 | min<LC2 
    ot3<-max>UC3 | min<LC3
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,mean=mean,std=std,var=var,min=min,p1=p1,p5=p5,p10=p10,q1=q1,q2=q2,q3=q3,p90=p90,p95=p95,p99=p99,max=max,ot_m1=ot1,ot_m2=ot2,ot_m2=ot3, IQR=iqr,UC1=UC1,LC1=LC1))
  }
  else{
    Var_Type=class(x)
    n<-length(x)
    nmiss<-sum(is.na(x))
    fre<-table(x)
    prop<-prop.table(table(x))
    
    return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
  }
}

#numerical variables
num_var <- sapply(proactive, is.numeric)
View(num_var)


#apply defined function
num_data <- t(data.frame(apply(proactive[num_var], 2, var_Summ)))
View(num_data)
write.csv(num_data,"diag.csv")

#missing values

proactive$REVENUE[is.na(proactive$REVENUE)]<-58.8539614010814
proactive$MOU[is.na(proactive$MOU)]<-525.728392370572
proactive$RECCHRGE[is.na(proactive$RECCHRGE)]<-46.8764916491367
proactive$DIRECTAS[is.na(proactive$DIRECTAS)]<-0.894801146390705
proactive$OVERAGE[is.na(proactive$OVERAGE)]<-40.0953598000875
proactive$ROAM[is.na(proactive$ROAM)]<-1.22152616792083
proactive$CHANGEM[is.na(proactive$CHANGEM)]<--10.8464614076122
proactive$CHANGER[is.na(proactive$CHANGER)]<--1.20592557941739
proactive$AGE1[is.na(proactive$AGE1)]<-31.3751128175007
proactive$AGE2[is.na(proactive$AGE2)]<-21.1577152844434
proactive$PHONES[is.na(proactive$PHONES)]<-1.80861695239704
proactive$MODELS[is.na(proactive$MODELS)]<-1.56179095234074
proactive$EQPDAYS[is.na(proactive$EQPDAYS)]<-380.265630718126


#outliers - Lower Limit
proactive$REVENUE[proactive$REVENUE <15.515]<-15.515
proactive$MOU[proactive$MOU <20.415]<-20.415
proactive$RECCHRGE[proactive$RECCHRGE <10]<- 10
proactive$CHANGEM[proactive$CHANGEM < -376.25]<--376.25
proactive$CHANGER[proactive$CHANGER < -47.5]<--47.5
proactive$MONTHS[proactive$MONTHS <7]<-7
proactive$ACTVSUBS[proactive$ACTVSUBS <1]<-1
proactive$EQPDAYS[proactive$EQPDAYS <42]<-42

#outliers - Upper Limit
proactive$REVENUE[proactive$REVENUE >135.39]<-135.39
proactive$MOU[proactive$MOU >1580.25]<-1580.25
proactive$RECCHRGE[proactive$RECCHRGE > 85]<- 85
proactive$DIRECTAS[proactive$DIRECTAS >4.21]<-4.21
proactive$OVERAGE[proactive$OVERAGE >190.375]<-190.375
proactive$ROAM[proactive$ROAM >5.09]<-5.09
proactive$CHANGEM[proactive$CHANGEM > 345.25]<-345.25
proactive$CHANGER[proactive$CHANGER > 46.218]<-46.218
proactive$AGE1[proactive$AGE1 >62]<-62
proactive$AGE2[proactive$AGE2 >62]<-62
proactive$DROPVCE[proactive$DROPVCE >22]<-22
proactive$BLCKVCE[proactive$BLCKVCE >17.33]<-17.33
proactive$UNANSVCE[proactive$UNANSVCE >97.67]<-97.67
proactive$CUSTCARE[proactive$CUSTCARE >9.33]<-9.33
proactive$THREEWAY[proactive$THREEWAY >1.33]<-1.33
proactive$MOUREC[proactive$MOUREC >440.938]<-440.938
proactive$OUTCALLS[proactive$OUTCALLS >90.33]<-90.33
proactive$INCALLS[proactive$INCALLS >35.67]<-35.67
proactive$PEAKVCE[proactive$PEAKVCE >279.67]<-279.67
proactive$OPEAKVCE[proactive$OPEAKVCE >242]<-242
proactive$DROPBLK[proactive$DROPBLK >35.33]<-35.33
proactive$CALLWAIT[proactive$CALLWAIT >8.67]<-8.67
proactive$MONTHS[proactive$MONTHS >37]<-37
proactive$UNIQSUBS[proactive$UNIQSUBS >3]<-3
proactive$ACTVSUBS[proactive$ACTVSUBS >2]<-2
proactive$EQPDAYS[proactive$EQPDAYS >866]<-866
proactive$SETPRC[proactive$SETPRC >149.99]<-149.99
proactive$PHONES[proactive$PHONES >4]<-4
proactive$MODELS[proactive$MODELS >3]<-3

# Categorical Variable

tab <- xtabs(~CHILDREN + CHURN, data = proactive)         # not significant         
chisq.test(tab)
tab <- xtabs(~CREDITA + CHURN, data = proactive)                  
chisq.test(tab)
tab <- xtabs(~CREDITAA + CHURN, data = proactive)                  
chisq.test(tab)
tab <- xtabs(~CREDITB + CHURN, data = proactive)                  
chisq.test(tab)
tab <- xtabs(~CREDITC + CHURN, data = proactive)                  
chisq.test(tab)
tab <- xtabs(~CREDITDE + CHURN, data = proactive)                  
chisq.test(tab)
tab <- xtabs(~CREDITGY + CHURN, data = proactive)         # not significant         
chisq.test(tab)
tab <- xtabs(~CREDITZ + CHURN, data = proactive)         # not significant         
chisq.test(tab)
tab <- xtabs(~PRIZMRUR + CHURN, data = proactive)                  
chisq.test(tab)
tab <- xtabs(~PRIZMUB + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~PRIZMTWN + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~REFURB + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~WEBCAP + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~TRUCK + CHURN, data = proactive)            # not significant     
chisq.test(tab)
tab <- xtabs(~RV + CHURN, data = proactive)                 # not significant
chisq.test(tab)
tab <- xtabs(~OCCPROF + CHURN, data = proactive)            # not significant     
chisq.test(tab)
tab <- xtabs(~OCCCLER + CHURN, data = proactive)            # not significant      
chisq.test(tab)
tab <- xtabs(~OCCCRFT + CHURN, data = proactive)            # not significant     
chisq.test(tab)
tab <- xtabs(~OCCSTUD + CHURN, data = proactive)             # not significant    
chisq.test(tab)
tab <- xtabs(~OCCHMKR + CHURN, data = proactive)             # not significant     
chisq.test(tab)
tab <- xtabs(~OCCRET + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~OCCSELF + CHURN, data = proactive)              # Not Significant   
chisq.test(tab)
tab <- xtabs(~OWNRENT + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~MARRYUN + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~MARRYYES + CHURN, data = proactive)             # Not Significant    
chisq.test(tab)
tab <- xtabs(~MARRYNO + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~MAILORD + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~MAILRES + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~MAILFLAG + CHURN, data = proactive)               #Not Significant  
chisq.test(tab)
tab <- xtabs(~TRAVEL + CHURN, data = proactive)                 # Not Significant
chisq.test(tab)
tab <- xtabs(~PCOWN + CHURN, data = proactive)                 # Not Significant
chisq.test(tab)
tab <- xtabs(~CREDITCD + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~NEWCELLY + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~NEWCELLN + CHURN, data = proactive)               # Not Significant  
chisq.test(tab)
tab <- xtabs(~INCMISS + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~INCOME + CHURN, data = proactive)                 
chisq.test(tab)
tab <- xtabs(~MCYCLE + CHURN, data = proactive)                 # Not Significant
chisq.test(tab)
tab <- xtabs(~SETPRCM + CHURN, data = proactive)                
chisq.test(tab)

require(MASS)
step <- step(glm(CHURN ~SETPRCM+ INCOME+INCMISS+NEWCELLY+CREDITCD+MAILRES+MAILORD+MARRYNO+MARRYUN+OWNRENT+OCCRET+WEBCAP+REFURB+PRIZMTWN+PRIZMUB+PRIZMRUR+CREDITDE+CREDITC+CREDITB+CREDITAA+CREDITA+REVENUE+MOU+RECCHRGE+DIRECTAS+OVERAGE+ROAM+CHANGEM+CHANGER+DROPVCE+BLCKVCE+UNANSVCE+CUSTCARE+THREEWAY+MOUREC+OUTCALLS+INCALLS+PEAKVCE+OPEAKVCE+CALLFWDV+DROPBLK+CALLWAIT+MONTHS+UNIQSUBS+ACTVSUBS+PHONES+MODELS+EQPDAYS+AGE1+AGE2+SETPRC, data =proactive))


#Transform numeric variables

proactive$REVENUE1 <- log(proactive$REVENUE)
hist(proactive$REVENUE1)

proactive$MOU1 <- log(proactive$MOU)
hist(proactive$MOU1)

proactive$RECCHRGE1 <- log(proactive$RECCHRGE)
hist(proactive$RECCHRGE1)

proactive$MONTHS1 <- sqrt(proactive$MONTHS)
hist(proactive$MONTHS1)

proactive$UNIQSUBS1 <- log(proactive$UNIQSUBS)
hist(proactive$UNIQSUBS1)

proactive$ACTVSUBS1 <- log(proactive$ACTVSUBS)
hist(proactive$ACTVSUBS1)

proactive$PHONES1 <- sqrt(proactive$PHONES)
hist(proactive$PHONES1)

proactive$EQPDAYS1 <- log(proactive$EQPDAYS)
hist(proactive$EQPDAYS1)

proactive$SETPRC1 <- sqrt(proactive$SETPRC)
hist(proactive$SETPRC1)

proactive$CALLWAIT1 <- sqrt(proactive$CALLWAIT)
hist(proactive$CALLWAIT1)

proactive$OPEAKVCE1 <- sqrt(proactive$OPEAKVCE)
hist(proactive$OPEAKVCE1)

proactive$PEAKVCE1 <- sqrt(proactive$PEAKVCE)
hist(proactive$PEAKVCE1)

proactive$INCALLS1 <- sqrt(proactive$INCALLS)
hist(proactive$INCALLS1)

proactive$THREEWAY1 <- sqrt(proactive$THREEWAY)
hist(proactive$THREEWAY1)

proactive$CUSTCARE1 <- sqrt(proactive$CUSTCARE)
hist(proactive$CUSTCARE1)

proactive$BLCKVCE1 <- sqrt(proactive$BLCKVCE)
hist(proactive$BLCKVCE1)

proactive$DROPVCE1 <- sqrt(proactive$DROPVCE)
hist(proactive$BLCKVCE1)

proactive$CHANGER1 <- exp(proactive$CHANGER)
hist(proactive$CHANGER1)

proactive$CHANGEM1 <- exp(proactive$CHANGEM)
hist(proactive$CHANGER1)

proactive$ROAM1 <- exp(proactive$ROAM)
hist(proactive$ROAM1)

proactive$AGE11 <- sqrt(proactive$AGE1)
hist(proactive$AGE11)

proactive$OVERAGE1 <- sqrt(proactive$OVERAGE)
hist(proactive$OVERAGE1)

# splitting data
Develp <- proactive[proactive$CALIBRAT == 1,]
View(Develp)

Validation <- proactive[proactive$CALIBRAT == 0,]
View(Validation)
Validation$CHURNDEP <- NULL

fit_model <- glm(CHURN ~ INCOME + INCMISS + NEWCELLY + CREDITCD + MAILRES + MARRYNO + 
                   WEBCAP + REFURB + PRIZMTWN + PRIZMUB + PRIZMRUR + CREDITDE + 
                   CREDITC + CREDITB + CREDITAA + REVENUE1 + MOU1 + RECCHRGE1 + 
                   OVERAGE1 + ROAM1 + CHANGEM1 + CHANGER1 + DROPVCE1 + BLCKVCE1 + 
                   CUSTCARE1 + THREEWAY1 + INCALLS1 + PEAKVCE1 + OPEAKVCE1 + CALLWAIT1 + 
                   MONTHS1 + UNIQSUBS1 + ACTVSUBS1 + PHONES1 + EQPDAYS1 + AGE11 + 
                   SETPRC1,data=Develp,family = binomial(logit))

summary(fit_model)


Concordance(fit_model)

final_model <- glm(CHURN ~ INCOME + INCMISS + NEWCELLY + MAILRES + MARRYNO + 
                     WEBCAP + REFURB + CREDITDE + 
                     CREDITC + CREDITB + CREDITAA + REVENUE1 + MOU1 + RECCHRGE1 + 
                     OVERAGE1 + ROAM1 +  DROPVCE1 + BLCKVCE1 + 
                      THREEWAY1 + INCALLS1 + PEAKVCE1 + CALLWAIT1 + 
                     MONTHS1 + UNIQSUBS1 + ACTVSUBS1 + EQPDAYS1 + AGE11 + 
                     SETPRC1,data=Develp,family = binomial(logit))

summary(final_model)

Concordance(final_model)                    # 0.6191 concordance

####################################### Development Dataset ###########################################

dev1<- cbind(Develp, Prob=predict(final_model, type="response")) 
View(dev1)

#create deciles
decLocations <- quantile(dev1$Prob, probs = seq(0.1,0.9,by=0.1))
dev1$decile <- findInterval(dev1$Prob,c(-Inf,decLocations, Inf))
View(dev1)
require(dplyr)
dev1$decile<-factor(dev1$decile)
str(dev1)
decile_grp<-group_by(dev1,decile)
decile_summ_dev<-summarize(decile_grp, total_cnt=n(), min_prob=min(p=Prob), max_prob=max(Prob), default_cnt = sum(CHURN), non_default_cnt=total_cnt -default_cnt)
decile_summ_dev<-arrange(decile_summ_dev, desc(decile))
View(decile_summ_dev)
write.csv(decile_summ_dev,"dev1.csv")

#Decile Analysis

require(sqldf)
fit_dev_DA <- sqldf("select decile, min(Prob) as Min_prob,
                      max(Prob) as max_prob,
                      sum(CHURNDEP) as default_cnt
                      from dev1
                      group by decile
                      order by decile desc")
View(fit_dev_DA)

############################################### Validation Dataset ###################################################

val1 <- cbind(Validation,prob=predict(final_model,Validation,type = "response"))
View(val1)

#deciles
delocations1 <- quantile(val1$prob, probs = seq(0.1,0.9,by=0.1), na.rm = TRUE)
val1$decile <- findInterval(val1$prob, c(-Inf,delocations1,Inf))
View(val1)
val1$decile <- factor(val1$decile)
str(test1)
decile_grp <- group_by(val1,decile)
decile_summ1 <- summarize(decile_grp, total_cnt = n(), min_prob = min(prob), max_prob = max(prob), default_cnt=sum(CHURN), non_default_cnt=total_cnt - default_cnt)
View(decile_summ1)
write.csv(decile_summ1, "val2.csv")

fit_test <- sqldf("select decile, min(prob) as min_prob,count(prob) as total,
                   max(prob) as max_prob,
                   sum(CHURNDEP) as default_cnt
                   from test1
                   group by decile
                   order by decile desc")
View(fit_test)

########################################################################################################################