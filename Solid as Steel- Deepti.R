#Solid as Steel

library(dplyr)
library(lubridate)

#Import the data file
steel=read.csv(file.choose(), header=TRUE, stringsAsFactors = TRUE)

#convert timestamp to date for shift column
steel = steel %>% mutate(shift = format(strptime(shift, "%m.%d.%y %H:%M:%S"), "%m-%d-%y"))
View(steel)

#mean of throughputs shift wise
throughput_M <- filter(steel, shift.type == "M")$throughput
throughput_N <- filter(steel, shift.type == "N")$throughput
throughput_E <- filter(steel, shift.type == "E")$throughput
mean_throughput_M= mean(throughput_M)
mean_throughput_N= mean(throughput_N)
mean_throughput_E= mean(throughput_E)
#M=763.65
#N=771.01
#E=718.36

#mean of delta throughputs shift wise
delta.throughput_M <- filter(steel, shift.type == "M")$delta.throughput
delta.throughput_N <- filter(steel, shift.type == "N")$delta.throughput
delta.throughput_E <- filter(steel, shift.type == "E")$delta.throughput
mean_delta.throughput_M= mean(delta.throughput_M)
mean_delta.throughput_N= mean(delta.throughput_N)
mean_delta.throughput_E= mean(delta.throughput_E)
#M=37.30
#N=23.15
#E=-16.24

#1.	What is the average number of strips per shift?
steel$Total_Strips = (steel$thickness.1+steel$thickness.2+steel$thickness.3) 
#36.832 i.e. ~37strips/shift

#2. Strip of which thickness cluster are the most common, and strips of which thickness cluster are the least common?
T1 = sum(steel$thickness.1)/sum(steel$Total_Strips)*100
#T1 = 32.31972
T2 = sum(steel$thickness.2)/sum(steel$Total_Strips)*100
#T2 = 54.70243
T3 = sum(steel$thickness.3)/sum(steel$Total_Strips)*100
#T3 = 12.97785

#Thickness 2 is most commonly produced strip while Thickness 3 is the least commonly produced strip. 

barplot(c(T1,T2,T3),T2, col=c("red","orange","yellow"), main = "Thickness vs. Commonness", xlab = "Thickness",ylab = "Commonness(%)", names.arg=c("Thickness 1","Thickness 2", "Thickness 3"), space=1)

#3. What are the min, max, and average values of delta throughput and RTR?
steel1 = data.frame(steel$delta.throughput, steel$RTR)
summary(steel1)

#steel.delta.throughput   steel.RTR     
#Min.   :-661.83        Min.   : 21.70  
#1st Qu.:-132.69        1st Qu.: 81.30  
#Median :  16.88        Median : 88.50  
#Mean   :  15.79        Mean   : 85.78  
#3rd Qu.: 156.34        3rd Qu.: 93.50  
#Max.   : 730.28        Max.   :100.00

boxplot(steel1$steel.RTR, steel1$steel.delta.throughput, 
    main = "Box plot of RTR and Delta Throughput",
    xlab = "Variables", ylab = "Values",
    col = c("yellow", "red"),
    names = c("RTR", "Delta Throughput"))


#4. Are there shifts during which the PPL processes strips of only steel grade 1, or of only steel grade 2, etc?
count_grade1= nrow(filter(steel,grade.1!=0 & grade.2==0 & grade.3==0 & grade.4==0 & grade.5==0 & grade.rest==0))
count_grade2= nrow(filter(steel,grade.1==0 & grade.2!=0 & grade.3==0 & grade.4==0 & grade.5==0 & grade.rest==0))
count_grade3= nrow(filter(steel,grade.1==0 & grade.2==0 & grade.3!=0 & grade.4==0 & grade.5==0 & grade.rest==0))
count_grade4= nrow(filter(steel,grade.1==0 & grade.2==0 & grade.3==0 & grade.4!=0 & grade.5==0 & grade.rest==0))
count_grade5= nrow(filter(steel,grade.1==0 & grade.2==0 & grade.3==0 & grade.4==0 & grade.5!=0 & grade.rest==0))
count_graderest= nrow(filter(steel,grade.1==0 & grade.2==0 & grade.3==0 & grade.4==0 & grade.5==0 & grade.rest!=0))
#count_grade1=5L
#count_grade2=0L
#count_grade3=0L
#count_grade4=8L
#count_grade5=1L
#count_graderest=9L
#L indicates integer rather than numerical
#Only grades 1, 4, 5 and rest have shifts where only 1 grade steel is produced. 


#5. Can the RTR theory adequately explain the deviations from the planned production figures?
#Null hypothesis or the RTR Theory: the lower the run time ratio, the higher the negative deviation from the plan

correlation = cor(steel$RTR, steel$delta.throughput)
print(correlation)
#0.556778
#There is a positive correlation between RTR and delta throughput

#normalizing MTP values
normMTP = (steel$MPT + abs(min(steel$MPT)))/max(steel$MPT + abs(min(steel$MPT)))

#scatterplot - RTR vs. Delta throughput
mod_1=lm(steel$delta.throughput ~ steel$RTR,data=steel)
plot(steel$RTR, steel$delta.throughput, 
        main = "ScatterPlot - RTR vs Delta Throughput",
        xlab = "RTR", ylab = "Delta Throughput")
abline(mod_1,col = "red")

#The linear regression model shows a positive correlation between the RTR and delta throughput.
#When a shift is run efficiently with run time high, the delta throughput is also high. 

summary(mod_1)


#Residuals:
#  Min      1Q  Median      3Q     Max 
#-823.70 -130.00    1.61  118.48  585.88 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -865.5004    59.4543  -14.56   <2e-16 ***
#  steel$RTR     10.2737     0.6868   14.96   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 178.3 on 498 degrees of freedom
#Multiple R-squared:   0.31,	Adjusted R-squared:  0.3086 
#F-statistic: 223.7 on 1 and 498 DF,  p-value: < 2.2e-16

#p-value model is < 2.2e-16, very small. 
#A very small p-value means the rejection of the null hypothesis in favor of the alternative hypothesis.

confint(mod_1)
#              2.5 %       97.5 %
# (Intercept) -982.312602 -748.68811
#steel$RTR      8.924235   11.62315

#This means that we are 95% confident that the true value of the intercept falls within this interval [−982.312602,−748.68811].


#6. Is the MPT theory sufficient to explain the deviations? Explain why or why not.
#MPT theory - material with a low thickness or low width carries a lower weight per meter.
#It takes longer to put 1t of material through the PPL.
#High MPT is the cause for high negative delta throughput for shifts with high RTR

plot(steel$delta.throughput ~ steel$MPT, xlab = "MPT", ylab = "Delta Throughput", main = "Scatterplot MPT vs DeltaThroughPut")
mod_mpt = lm(steel$delta.throughput ~ steel$MPT)
abline(mod_mpt,col = "red")
cor(steel$MPT, steel$delta.throughput)
# -0.6672597  - Negative Correlation

#We can say that if the MPT increases, the negative deviation is higher

summary(mod_mpt)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-690.40  -83.22    3.02   99.90  505.30 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 389.5816    20.0172   19.46   <2e-16 ***
#  steel$MPT   -10.8700     0.5437  -19.99   <2e-16 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 159.8 on 498 degrees of freedom
#Multiple R-squared:  0.4452,	Adjusted R-squared:  0.4441 
#F-statistic: 399.7 on 1 and 498 DF,  p-value: < 2.2e-16

#R-squared = 0.445 - meaning 0<R-squared<1
#Though MPT is not the best fit of the model to the data but it explains the variability in the Delta Throughput


#checking assumptions
mod1pred = predict(mod_1) 
mod1resd = residuals(mod_1)

# Quick look at the actual, predicted, and residual values
steel %>% select(steel$delta.throughput, mod1pred, mod1resd) %>% head()
steel %>% select(steel$delta.throughput, mod1pred, mod1resd) # all actual, predicted, residual
mod1stdr<-(mod1resd-mean(mod1resd))/sd(mod1resd) # standarized residual
mod1stdr