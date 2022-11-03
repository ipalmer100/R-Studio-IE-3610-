### Midterm 2 Data Frame Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment
# If you're given data frames on an assignment, you likely have Case 5 #



### DATA ENTRY ###
# Enter problem statement values 
DataFrame = data.frame(Header=c(0.52,1.06,1.26,2.17,1.55,0.99,1.10,1.07,1.81,
                                2.05,0.91,0.79,1.39,0.62,1.52,1.02,1.10,1.78,
                                1.01,1.15))
DataFrame # Check new df
Null = 1 # Null hypothesis value
alpha = 0.01 # Set % confidence, ex) Alpha = 0.01, confidence = 0.99
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
print(paste("Upper Mild Outliers: Between",upperMild,"&",upperExtreme))
print(paste("Upper Extreme Outliers: Greater than",upperExtreme)) 
print(paste("Lower Mild Outliers: Between",lowerMild,"&",lowerExtreme))
print(paste("Lower Extreme Outliers: Less than",lowerExtreme))
#boxplot.default(DataSort) # print box plot (optional)


## Case 3: Non-normal population, large sample (unknown σ) (Chapter 8.2)
mean = mean(DataFrame$Header) # Check sample mean
s = sd(DataFrame$Header) # Check sample standard deviation
n = sum(table(DataFrame$Header)) # Counts your data values (n)
critical = (mean - Null)/(s/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
print(paste("Case 3 (z, unknown σ, estimate mean) =>"))
print(paste("critical =",critical,"| z =",z))
print(paste("p-value =",p_value,"|","alpha =", alpha))
print(Eval)


## Case 5: One-sample t test for population mean (unknown σ) (Chapter 8.3)
n = sum(table(DataFrame$Header)) # Counts your data values (n)
mean = mean(DataFrame$Header) # Check mean
s = sd(DataFrame$Header) # Check standard deviation
critical = (mean - Null)/(s/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  t = qt(alpha/2,n-1,lower.tail = FALSE)
  p_value = 2*pt(critical,n-1,lower.tail=FALSE)
} else {
  t = qt(alpha,n-1,lower.tail = FALSE)
  p_value = pt(critical,n-1,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject null!"
} else {
  Eval = "Fail to reject null!"
}
# Evaluate rejection
print(paste("Case 5 (t, estimate mean) =>"))
print(paste("critical =",critical,"| t =",t))
print(paste("p-value =",p_value,"|","alpha =", alpha))
print(Eval)

