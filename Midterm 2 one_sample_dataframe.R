### Midterm 2 Data Frame Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment
# If you're given data frames on an assignment, you likely have Case 5 #



### DATA ENTRY ###
# Enter problem statement values 
DataFrame = data.frame(Header=c(198,336,70,18,122,9,50,5,163,86))
DataFrame # Check new df
Null = 25 # Null hypothesis value
alpha = 0.05 # Set % confidence, ex) Alpha = 0.01, confidence = 0.99
bounds = 1 # Two-sided or one-sided?

### END DATA ENTRY ### 



## Descriptive Statistics ##
DataSort = DataFrame[order(DataFrame$Header),] #sort ascending
DataSort # view sorted data
summary(DataFrame$Header)
sampleMean = mean(DataSort) # identical answer: mean(DataFrame$Header)
sampleMedian = median(DataSort) # identical answer: median(DataFrame$Header)
print(paste("Mean:",sampleMean))
print(paste("Median:",sampleMedian))
Q1 = summary(DataSort)[2] # First quartile
Q3 = summary(DataSort)[5] # Second quartile
IQR = IQR(DataSort)
upperMild = Q3+(1.5*IQR)
upperExtreme = Q3+(3*IQR)
lowerMild = Q1-(1.5*IQR)
lowerExtreme = Q1-(3*IQR)
writeLines(c(paste("Upper Mild Outliers: Between",upperMild,"&",upperExtreme),
             paste("Upper Extreme Outliers: >",upperExtreme),
             paste("Lower Mild Outliers: Between",lowerMild,"&",lowerExtreme),
             paste("Lower Extreme Outliers: <",lowerExtreme)))
#boxplot.default(DataSort) # print box plot (optional)


## Case 3: Non-normal population, z-test, large sample (unknown σ) (Chapter 8.2)
mean = mean(DataFrame$Header) # Check sample mean
s = sd(DataFrame$Header) # Check sample standard deviation
n = sum(table(DataFrame$Header)) # Counts your data values (n)
test_stat = (mean - Null)/(s/sqrt(n))
abs_test = abs((mean - Null)/(s/sqrt(n)))
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_test,lower.tail = FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_test,lower.tail = FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
writeLines(c(paste("Case 3 (z, unknown σ, estimate mean) =>"),
             paste("Test Stat = ±",test_stat,"| Critical Value = ±",z_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))


## Case 5: One-sample t-test for population mean (unknown σ) (Chapter 8.3)
n = sum(table(DataFrame$Header)) # Counts your data values (n)
mean = mean(DataFrame$Header) # Check mean
s = sd(DataFrame$Header) # Check standard deviation
test_stat = (mean - Null)/(s/sqrt(n)) # test statistic
abs_test = abs((mean - Null)/(s/sqrt(n)))
# Evaluate p-value
if (bounds == 2){
  t_critical = qt(alpha/2,n-1,lower.tail = FALSE)
  p_value = 2*pt(abs_test,n-1,lower.tail = FALSE)
  CItype = "Two-Sided CI"
  upperB = paste(mean + t_critical*(s/sqrt(n)),"]")
  lowerB = paste("[",mean - t_critical*(s/sqrt(n)))
} else {
  t_critical = qt(alpha,n-1,lower.tail = FALSE)
  p_value = pt(abs_test,n-1,lower.tail = FALSE)
  CItype = "Either/Or One-Sided CI"
  upperB = paste("UB: [-infinity,",mean + t_critical*(s/sqrt(n)),"]")
  lowerB = paste("LB: [",mean - t_critical*(s/sqrt(n)),"+infinity]")
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
writeLines(c(paste("Case 5 (t, estimate mean) =>"),
             paste("Test Stat = ±",test_stat,"| Critical Value = ±",t_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))
# Evaluate CI
writeLines(c(paste("Case 5:",CItype),
             paste(lowerB,upperB)))

