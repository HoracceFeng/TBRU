############################
###   BIOS-526 Midterm   ###
###    By Horace Feng    ###
############################

setwd("C:/Users/Horace/OneDrive/Academic/BIOS_526/Midterm")
load("MidtermData.RData")

### data info & cleaning ###

  ## summary() & str() provides many info:
  ## 1. t2 - t7 have many NA values, how to clean them is a good question
  ## 2. for categorical data: race, gender, dad_edu
  ##    should be changed to categorical structure, and do table() to see frequency
  ## 3. for quantitative data: age , t1 - t7
  ##    should do boxplot to see outlier, trend and distribution
    str(dat)
    summary(dat)

  ## save the original dataset as "dat"
  ## the new dataset "data" will be used in analysis
    data = dat
    
  ## change race from quantitative to categorial
    data$raceF = factor(NA,levels = c("White","Black","Hispanic","Asian"))
    data$raceF[which(data$race==1)] = "White"
    data$raceF[which(data$race==2)] = "Black"
    data$raceF[which(data$race==3)] = "Hispanic"
    data$raceF[which(data$race==5)] = "Asian"
    str(data$raceF)   ## check
    
  ## change gender from quantitative to categorial
    data$genderF = factor(NA,levels = c("male","female"))
    data$genderF[which(data$gender==1)] = "male"
    data$genderF[which(data$gender==2)] = "female"
    str(data$genderF)   ## check
    
  ## change dad_edu from quantitative to categorial
    data$dad_eduF = factor(data$dad_edu)
    str(data$dad_eduF)   ## check


### Relevant summary statistics ### --------------------------------- still need to add things
  
  ## overall info ##
    str(data)
    summary(data)
    sd(data$t1,na.rm=T);sd(data$t2,na.rm=T);sd(data$t3,na.rm=T);sd(data$t4,na.rm=T);
    sd(data$t5,na.rm=T);sd(data$t6,na.rm=T);sd(data$t7,na.rm=T);
    
  ## categorical data ##
    table(data$raceF)
    table(data$genderF)
    table(data$dad_eduF)
    
  ## quantitative data ##  
    par(mfrow=c(2,2))
    boxplot(data$age, main = "Original")
    boxplot(data$age~data$genderF, main = "Gender")
    boxplot(data$age~data$raceF , main = "Race")
    boxplot(data$age~data$dad_eduF , main = "Father's Education Level")
    par(mfrow=c(1,1))
      ## age have no effect on gender, race and dad_edu  --- no interaction term
    
    boxplot(data$t1,data$t2,data$t3,data$t4,data$t5,data$t6,data$t7, 
            ylab = "Math Score", xlab = "Grades", main = "Trend of Math Score vs Grades")
      ## the math score will increase with the grades (age)
    

### missing data pattern ### ---------------------------------------- don't know how to answer
    
  ## Try to figure out the relationship among different stages
    ## data process before comaprison
    k2 = is.na(data$t2)
    k3 = is.na(data$t3)
    k4 = is.na(data$t4)
    k5 = is.na(data$t5)
    k6 = is.na(data$t6)
    k7 = is.na(data$t7)
    max(sum(k2),sum(k3),sum(k4),sum(k5),sum(k6),sum(k7))
    
    ## NA info
    summary(data)[7,]
    
    ## overall union and intersect info
    k = data.frame(k2,k3,k4,k5,k6,k7)
    tmp = which(k[,1]==TRUE)
    for (i in 2:ncol(k)) {
      tmp = union(which(k[,i]==TRUE),tmp)
    }
    length(tmp)     ## 2997 NA union
    for (i in 2:ncol(k)) {
      tmp = intersect(which(k[,i]==TRUE),tmp)
    }
    length(tmp)     ## 45 NA intersect
    
    ## since t3, t6, t7 have many NA values, try to find out their relationship
    ## because their relationship may have bigger effect on analysis than others
    insect36 = intersect(which(k3==TRUE),which(k6==TRUE))
    length(insect36)  ### t3: 2563; t6: 1086; intersect36: 784
    
    insect37 = intersect(which(k3==TRUE),which(k7==TRUE))
    length(insect37)  ### t3: 2563; t7: 1350; intersect36: 956
    
    insect67 = intersect(which(k6==TRUE),which(k7==TRUE))
    length(insect67)  ### t6: 1086; t7: 1350; intersect36: 1015
      ## obviously, people who don't have value in t3 have about 70% not to do t6 & t7
      ## t6 & t7 even have a higher correlation, people not do t6 tent to not do t7 also (normal)
    

### Univariate association between socres & age 
    
  ## Preliminary Analysis
    boxplot(data$t1,data$t2,data$t3,data$t4,data$t5,data$t6,data$t7)
      ## from the boxplot, you can observe a linear relationship with increasing trend,
      ## so we can then do the next step, to make sure if it really has linear relationship
    
  ## Further Analusis
    
    ## construct new dataset called "dataset" for analytical model
    dataset = data[,4:14]
    dataset$ID = factor(row.names(dataset))            # pure dataset 

    library(reshape2)
    library(dplyr)
    dataset = melt(dataset,id = c("ID","age","genderF","raceF","dad_eduF"))
    dataset = arrange(dataset,ID)                      # long format dataset
    names(dataset)[2] = "base_age"                     # original name "age"
    names(dataset)[6] = "ageF"                         # period tag
    names(dataset)[7] = "score"                        # math score
    dataset$age = dataset$base_age                     # age for each period
    
    ## age (month of the test):
    ## t1 - t2 : fall & spring kindergarden      ---- 0  & 6
    ## t3 - t4 : fall & spring 1st grade         ---- 12 & 18
    ## t5, t6, t7 : spring 3rd, 5th, 8th grade   ---- 42 , 66 & 102 
    for (i in 1:nrow(dataset)) {
      if (dataset$ageF[i] == "t1") {dataset$age[i] = dataset$base_age[i] + 0}
      else if (dataset$ageF[i] == "t2") {dataset$age[i] = dataset$base_age[i] + 6}
      else if (dataset$ageF[i] == "t3") {dataset$age[i] = dataset$base_age[i] + 12}
      else if (dataset$ageF[i] == "t4") {dataset$age[i] = dataset$base_age[i] + 18}
      else if (dataset$ageF[i] == "t5") {dataset$age[i] = dataset$base_age[i] + 42}
      else if (dataset$ageF[i] == "t6") {dataset$age[i] = dataset$base_age[i] + 66}
      else if (dataset$ageF[i] == "t7") {dataset$age[i] = dataset$base_age[i] + 102}
      else {is.na(dataset$age[i])}
    }
    
    ## overall scatter plot
    plot(x = dataset$age, y = dataset$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Scatterplot of Math Score vs. Age")
    abline(lm(dataset$score~dataset$age),lwd=2,lty=1,col="red")
    legend("bottomright",legend = c("Linear"),col = c("red"), lwd = 3)
      ## the plot above shows the approximate linear relationship with increase trend 
      ## we can't conclude it is linear or not, but it also seems like a log curve
    
    ## Association with gender
    par(mfrow=c(1,3))
    plot(x = dataset$age, y = dataset$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Overall")
    abline(lm(dataset$score~dataset$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, genderF=="male")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Male")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, genderF=="female")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Female")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    par(mfrow=c(1,1))
      ## the plot above shows that the trend and distribution looks the same 
      ## compared with overall, female and male, which means gender not effect
    
    ## Association with race
#    par(mfrow=c(1,5))
    plot(x = dataset$age, y = dataset$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Overall")
    abline(lm(dataset$score~dataset$age),lwd=2,lty=1,col="red")
    
    par(mfrow=c(2,2))
    tmp.table = filter(dataset, raceF=="White")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "White")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, raceF=="Black")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Black")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, raceF=="Hispanic")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Hispanic")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, raceF=="Asian")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Asian")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    par(mfrow=c(1,1))
      ## From the plot above, we can easily observe that most of the data are from white
      ## so if we construct the model, it may not have good fit on other races
      ## this can also be observed in the summary() result we shown above
      ## What's more, the plots show that :
      ## White, Hispanic & Asian --- tend to have similar plot as overall, higher in middle
      ## Black --- tend to be more "linear" than overall 
    
    ## Association with dad_edu
#    par(mfrow=c(1,5))
    plot(x = dataset$age, y = dataset$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Overall")
    abline(lm(dataset$score~dataset$age),lwd=2,lty=1,col="red")
    
    par(mfrow=c(2,2))
    tmp.table = filter(dataset, dad_eduF =="Colleg or AD")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Colleg or AD")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, dad_eduF=="Graduate of Prof")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Graduate of Prof")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, dad_eduF=="HS or GED")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "HS or GED")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    tmp.table = filter(dataset, dad_eduF=="less HS")
    plot(x = tmp.table$age, y = tmp.table$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "less HS")
    abline(lm(tmp.table$score~tmp.table$age),lwd=2,lty=1,col="red")
    
    par(mfrow=c(1,1))
      ## From the plot above, we can easily observe that "less HS" has less cases
      ## so if we construct the model, it may not have good fit on "other races"less HS"
      ## And, the plots show that : "less HS" tend to more linear
      ## While the other 3 will be higher in the middle stage.
    

### Linear Model Fit ###
    names(dataset)
    fit1 = lm(score ~ age,data = dataset)
    summary(fit1)
    fit2 = lm(score ~ age + I(age^2),data = dataset)
    summary(fit2)
    fit3 = lm(score ~ age + raceF,data = dataset)
    summary(fit3)
    fit4 = lm(score ~ age + genderF,data = dataset)
    summary(fit4)
    fit5 = lm(score ~ age + dad_eduF,data = dataset)
    summary(fit5)
    fit6 = lm(score ~ age + raceF + genderF + dad_eduF,data = dataset)
    summary(fit6)
    fit7 = lm(score ~ age + I(age^2) + raceF + genderF + dad_eduF,data = dataset)
    summary(fit7)
    fit8 = lm(score ~ age + I(age^3),data = dataset)
    summary(fit8)

      ## After fitting 6 models above, my conclusion is:
      ## Y : The response variable should not have to transform
      ## X : the model should include 2-order of age, but don't have to use 3-order
      ##     since R-square is the same, but 2-order will be more flexibel & less over fit
      ## model fit7 will be chosen in this part
    
    summary(fit7)
    AIC(fit7)
    

### Piecewise Splines Model ###

    ## In this part, we will seperate the dataset as 4 parts in the model
    ## Since from the plot above shows four seperate parts in Age
    ## so the break points will be x = 99,123,150
    plot(x = dataset$age, y = dataset$score, main = "Break points of piecewise spline model")
    abline(v =  99,col="Red")
    abline(v = 123,col="Red")
    abline(v = 150,col="Red")
    
    ## Indicator and model
    sp1 = (dataset$age - 99) * as.numeric(dataset$age >= 99)
    sp2 = (dataset$age - 123) * as.numeric(dataset$age >= 123)
    sp3 = (dataset$age - 150) * as.numeric(dataset$age >= 150)
    
    sp_fit1 = lm(score ~ age + sp1 + sp2 + sp3, data = dataset)
    summary(sp_fit1)
    sp_fit2 = lm(score ~ age + I(age^2) + sp1 + sp2 + sp3, data = dataset)
    summary(sp_fit2)
    sp_fit3 = lm(score ~ age + sp1 + sp2 + sp3 + raceF + genderF + dad_eduF, data = dataset)
    summary(sp_fit3)
    
      ## The piecewise model shows that, in 1-order age, piecewise have good effect
      ## 2-order age into the model has little effect on this model
      ## sp_fit3 model will be chosen in this part
    
    summary(sp_fit3)
    AIC(sp_fit3)
    
    ## B-spline model
    library(splines)
    bs_fit = lm(score ~ bs(age , knots=c(99,123,150),degree = 10),data = dataset)
    summary(bs_fit)
    
      ## the effect is worse than the original one.
    

### Random intercept model ### --- with model selection AIC(), r-square and so on 
    
    ## Random intercept overall scatter plot
    plot (dataset$score~dataset$age, xlab = "Age (months)", ylab = "Score", type ="n" )
    title (main = "3600 students with Math Score measured at 7 different grades", cex = 1.5)
    IDF = unique(dataset$ID)
    for (i in 1:length(IDF)){
      y = dataset$score[dataset$ID == IDF[i]]; x = dataset$age[dataset$ID ==IDF[i]]
      lines (y~x, type = "b", col = i, cex = .75, lwd = .5)
    }
    
    ## Overall model --- longitudinal level
#    ri_fit1 = lm(score ~ age + I(age^2) + factor(ID), data = dataset)
#    summary(ri_fit1)
      ## should not use this model, because it is random slope
    
    library(lme4)
    ri_fit2 = lmer(score ~ age + (1|ID),data = dataset, REML = F)
    summary(ri_fit2)
    random.eff.ri_fit2 = ranef(ri_fit2)$ID[,1]
    hist(random.eff.ri_fit2)
    
    ri_fit3 = lmer(score ~ age + I(age^2) + (1|ID),data = dataset, REML = F)
    summary(ri_fit3)
    random.eff.ri_fit3 = ranef(ri_fit3)$ID[,1]
    hist(random.eff.ri_fit3)
    
    ri_fit4 = lmer(score ~ age + (1|ID) + genderF + raceF + dad_eduF ,data = dataset , REML = F)
    summary(ri_fit4)
    
    ri_fit5 = lmer(score ~ age + I(age^2) + (1|ID) + genderF + raceF + dad_eduF ,data = dataset , REML = F)
    summary(ri_fit5)
    
    ri_sp_fit1 = lmer(score ~ age + sp1 + sp2 + sp3 + (1|ID) ,data = dataset , REML = F)
    summary(ri_sp_fit1)
    
    ri_sp_fit2 = lmer(score ~ age + sp1 + sp2 + sp3 + (1|ID) + genderF + raceF + dad_eduF ,data = dataset , REML = F)
    summary(ri_sp_fit2)
    
      ## In the Overall random intercept model, REML = F casue no effect on this option in this case
      ## I will choose ri_sp_fit1 model 
      ## because if we add 2-order age term into the model, the model will not fit well 
      ## due to different scale. So I use piecewise spline to replace 2-order age term 
      ## The effect of replacement had been tested before and prove they are similar
      ## Also, this model has the best performance in those 5 criterias.
    
    summary(ri_sp_fit1)

    
    ## Analysis in each grade level
    
      ## Definition of Grade:
      ## Here I define grade as seven levels : t1 - t7 
      ## Because if we seperate by true "grade", the "kindergarden" and "1-grade" will 
      ## have a 2 times larger size dataset than the other 3, which may affect model precision
      ## and then made artifact difference.
      ## Also, here I will drop out all the NA value
    
    dataset$sp1 = sp1; dataset$sp2 = sp2; dataset$sp3 = sp3
    
    dataset_t1 = dataset[dataset$ageF=="t1",] ; dataset_t1 = na.omit(dataset_t1) 
    dataset_t2 = dataset[dataset$ageF=="t2",] ; dataset_t2 = na.omit(dataset_t2) 
    dataset_t3 = dataset[dataset$ageF=="t3",] ; dataset_t3 = na.omit(dataset_t3) 
    dataset_t4 = dataset[dataset$ageF=="t4",] ; dataset_t4 = na.omit(dataset_t4) 
    dataset_t5 = dataset[dataset$ageF=="t5",] ; dataset_t5 = na.omit(dataset_t5) 
    dataset_t6 = dataset[dataset$ageF=="t6",] ; dataset_t6 = na.omit(dataset_t6) 
    dataset_t7 = dataset[dataset$ageF=="t7",] ; dataset_t7 = na.omit(dataset_t7) 
    
      ## In this part, I will not use random intercept model because each ID will only have one value
      ## So, here I just use linear model with piecewise spline.
    
    t1_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t1)
    summary(t1_fit)
    
    t2_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t2)
    summary(t2_fit)
    
    t3_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t3)
    summary(t3_fit)
    
    t4_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t4)
    summary(t4_fit)
    
    t5_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t5)
    summary(t5_fit)
    
    t6_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t6)
    summary(t6_fit)
    
    t7_fit = lm(score ~ age + genderF + raceF + dad_eduF, data = dataset_t7)
    summary(t7_fit)
    
      ## The result of this part shows that all these model are bad fitted.
      ## We cannot construct a model to predict the score at each grade by using these variables
      ## even just predict the data we use to build up the model
      ## That may because of the huge difference among students
      ## And this difference being displayed after droping the influence of "grade"
      ## In other word, the score are most related to "grade"
      ## If we drop the variable age, the model will be even worse.

    
    
### Limitation & Discussion
    
    ## Confounding
    boxplot(dataset$age~dataset$ageF)
      ## Obviously, grade (ageF) will be a confounder if we add it into the model with age.

    
    ## Effect of Missing Data
    plot(x = dataset$age, y = dataset$score, xlab = "Age (month)", 
         ylab = "Math Score", main = "Effect of Missing Data")
    abline(lm(dataset$score~dataset$age),lwd=2,lty=1,col="red")
    lines(sp_fit2$fitted ~ na.omit(dataset)$age, col = "blue")
    legend("bottomright",legend = c("Linear","piecewise"),col = c("red","blue"), lwd = 3)
    
    test = aggregate(bs_fit$fitted, list(na.omit(dataset)$age),mean)
    lines(test$x ~ test$Group.1 , col = "blue")
    
    
    
    
    
    
    
