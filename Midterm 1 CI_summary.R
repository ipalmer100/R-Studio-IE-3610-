### Midterm 1 Critical Value Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment


### DATA ENTRY ###
# Set parameters you aren't using to 0

mean = 0 # Sample mean (x bar)
s = 0 # Case 3,4,5,6 standard deviation (s)
s_pop = 0 # Case 1, 2 standard deviation (sigma)
n = 2343 # number of data
p_hat = 0.53 # Case 4 proportion statistic
confidence = 0.99 # Set % confidence
bounds = 2 # Two-sided or one-sided interval?

### END DATA ENTRY ### 


## Case 1 and 2 ##
Casetxt = "Case 1 & 2 (z, known σ, estimate mean)=>"
LowerBoundtxt = "Lower:"
UpperBoundtxt = "Upper:"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"

alpha = (1-confidence)/bounds # alpha value finds area for critical value
N = sqrt(n)
z = qnorm(alpha,lower.tail = FALSE)
step1 = z*(s_pop/N)
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
result_onesided = paste(Casetxt, OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided) # print regular CI
} else {
  print(result_onesided) # print upper and lower limit 1-sided estimate
  # (NOT prediction interval)
}


## Case 3 ##
Casetxt = "Case 3 (z, unknown σ, estimate mean) =>"
LowerBoundtxt = "["
UpperBoundtxt = "]"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"

alpha = (1-confidence)/bounds # alpha value finds area for critical value
N = sqrt(n)
z = qnorm(alpha,lower.tail = FALSE)
step1 = z*(s/N)
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
result_onesided = paste(Casetxt, OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## Case 4 ##
Casetxt = "Case 4 (z, estimate proportion) =>"
LowerBoundtxt = "["
UpperBoundtxt = "]"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
alpha = (1-confidence)/bounds # alpha value finds area for critical value
q_hat = 1-p_hat
z = qnorm(alpha,lower.tail = FALSE)
step1 = z*(sqrt((p_hat*q_hat)/n))
LowerB = p_hat - step1 # Lower confidence bound
UpperB = p_hat + step1 # Upper confidence bound
result_onesided = paste(Casetxt, OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## Case 5 ##
Casetxt = "Case 5 (t, estimate mean) =>"
LowerBoundtxt = "["
UpperBoundtxt = "]"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
alpha = (1-confidence)/bounds
N = sqrt(n)
t = qt(alpha,n-1,lower.tail = FALSE)
step1 = t*(s/N)
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
result_onesided = paste(Casetxt, OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## Case 5 PREDICTION ##
Casetxt = "Case 5 (t, estimate mean) =>"
LowerBoundtxt = "["
UpperBoundtxt = "]"
OneSidedLowertxt = "One-sided lower:"
OneSidedUppertxt = "One-sided upper:"
alpha = (1-confidence)/bounds
N = sqrt(n)
t = qt(alpha,n-1,lower.tail = FALSE)
step1 = t*s*(sqrt(1+(1/n)))
LowerB = mean - step1 # Lower confidence bound
UpperB = mean + step1 # Upper confidence bound
result_onesided = paste(Casetxt, OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## Case 6 Variance ##
Casetxt = "Case 6 (chi-squared, estimate variance) =>"
LowerBoundtxt = "["
UpperBoundtxt = "]"
OneSidedLowertxt = "One-sided var lower:"
OneSidedUppertxt = "One-sided var upper:"

alpha = (1-confidence)/bounds
chiLO = qchisq(alpha,n-1,lower.tail = FALSE)
chiHI = qchisq(1-alpha,n-1,lower.tail = FALSE)
LowerB = ((n-1)*(s^2))/chiLO # Lower confidence bound
UpperB = ((n-1)*(s^2))/chiHI # Upper confidence bound
result_onesided = paste(OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}


## Case 6 Standard Deviation ##
Casetxt = "Case 6 (chi-squared, estimate standard deviation) =>"
LowerBoundtxt = "["
UpperBoundtxt = "]"
OneSidedLowertxt = "One-sided stdev lower:"
OneSidedUppertxt = "One-sided stdev upper:"

alpha = (1-confidence)/bounds
chiLO = qchisq(alpha,n-1,lower.tail = FALSE)
chiHI = qchisq(1-alpha,n-1,lower.tail = FALSE)
LowerB = sqrt(((n-1)*(s^2))/chiLO) # Lower confidence bound
UpperB = sqrt(((n-1)*(s^2))/chiHI) # Upper confidence bound
result_onesided = paste(OneSidedLowertxt,LowerB,OneSidedUppertxt,UpperB)
result_twosided = paste(Casetxt, LowerBoundtxt,LowerB,",",UpperB,UpperBoundtxt)
if (bounds == 2){
  print(result_twosided)
} else {
  print(result_onesided)
}

