### Midterm 1 Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment



### DATA ENTRY ###
# Enter problem statement values 
DataFrame = data.frame(Header=c(156,236,286,225,138,250,167,173,208,252,226,
                                179,214,217,176,208,200,192,222,141))
DataFrame # Check new df
confidence = 0.95 # Set % confidence
bounds = 2 # Two-sided or one-sided interval?

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
boxplot.default(DataSort)


## Case 3 CI with given data (z critical value)
n = sum(table(DataFrame$Header)) # Counts your data values (n)
mean = mean(DataFrame$Header) # Check sample mean
s = sd(DataFrame$Header) # Check sample standard deviation
alpha = (1-confidence)/bounds # alpha value finds area for critical value
N = sqrt(n)
z = qnorm(alpha,lower.tail = FALSE)
step1 = z*(s/N)
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
Casetxt = "Case 3 (z, unknown σ, estimate mean) =>"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
LowerBoundtxt = "["
UpperBoundtxt = "]"
result_onesided = paste(Casetxt,OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}

## Case 5 CI with given data (t critical value)
n = sum(table(DataFrame$Header)) # Counts your data values (n)
mean = mean(DataFrame$Header) # Check mean
s = sd(DataFrame$Header) # Check standard deviation
alpha = (1-confidence)/bounds
N = sqrt(n)
t = qt(alpha,n-1,lower.tail = FALSE)
step1 = t*(s/N)
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
Casetxt = "Case 5 (t, estimate mean) =>"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
LowerBoundtxt = "["
UpperBoundtxt = "]"
result_onesided = paste(Casetxt,OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## Case 5 PREDICTION Interval (PI) with given data
n = sum(table(DataFrame$Header)) # Counts your data values (n)
mean = mean(DataFrame$Header) # Check mean
s = sd(DataFrame$Header) # Check standard deviation
alpha = (1-confidence)/bounds
N = sqrt(n)
t = qt(alpha,n-1,lower.tail = FALSE)
step1 = t*s*(sqrt(1+(1/n)))
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
Casetxt = "Case 5 Prediction Interval =>"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
LowerBoundtxt = "["
UpperBoundtxt = "]"
result_onesided = paste(Casetxt,OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## CASE 6 VARIANCE CI with given data (chi-squared critical value)
n = sum(table(DataFrame$Header)) # Counts your data values (n)
s = sd(DataFrame$Header) # Check standard deviation
alpha = (1-confidence)/bounds
chiLO = qchisq(alpha,n-1,lower.tail = FALSE)
chiHI = qchisq(1-alpha,n-1,lower.tail = FALSE)
LowerB = ((n-1)*(s^2))/chiLO # Lower confidence bound
UpperB = ((n-1)*(s^2))/chiHI # Upper confidence bound
Casetxt = "Case 6 (chi-squared, estimate variance) =>"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
LowerBoundtxt = "["
UpperBoundtxt = "]"
result_onesided = paste(Casetxt,OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}

## CASE 6 STANDARD DEVIATION CI with given data (chi-squared critical value)
n = sum(table(DataFrame$Header)) # Counts your data values (n)
s = sd(DataFrame$Header) # Check standard deviation
alpha = (1-confidence)/bounds
chiLO = qchisq(alpha,n-1,lower.tail = FALSE)
chiHI = qchisq(1-alpha,n-1,lower.tail = FALSE)
LowerB = sqrt(((n-1)*(s^2))/chiLO) # Lower confidence bound
UpperB = sqrt(((n-1)*(s^2))/chiHI) # Upper confidence bound
Casetxt = "Case 6 (chi-squared, estimate standard deviation) =>"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
LowerBoundtxt = "["
UpperBoundtxt = "]"
result_onesided = paste(Casetxt,OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}

