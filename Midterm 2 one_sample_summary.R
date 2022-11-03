### Midterm 2 Summary Statistics Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment


### DATA ENTRY ###
# Fill in Ho (null hypothesis) and other parameters
Ho = 30000
Ha = ">" # Change value in parentheses: >, <, or <>
n = 16
alpha = 0.01
p_hat = 0 # Case 4 only
# Fill in mean and standard deviation
mean = 31000
# sample (s) OR population (s_pop = σ (Case 1 only))
s = 0
s_pop = 1500
### END DATA ENTRY ### 

# RUN THIS: variable conditionals # 
if (Ha == "<>"){
  bounds = 2 # Ha: if not equal to (<>), then 2 ... if > or < , then 1
} else if (Ha == ">"){
  bounds = 1
} else if (Ha == "<"){
  bounds = 1
}



## Case 1: z test for population mean (known σ) (Chapter 8.2)
critical = (mean - Ho)/(s_pop/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Sample size - unfinished
# Beta_sample = pnorm(z + ((Ho - mean)/(s_pop/(sqrt(n)))),lower.tai=FALSE)
# z_beta = qnorm(Beta_sample,lower.tail = FALSE)
# sample_n = ((s_pop*(z+z_beta))/(Ho-mean))^2
# Evaluate rejection
writeLines(c(paste("Case 1 (z, KNOWN σ, estimate mean) =>"),
             paste("critical =",critical,"| z =",z),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))


## Case 3: Non-normal population, large sample (unknown σ) (Chapter 8.2)
critical = (mean - Ho)/(s/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Evaluate rejection
writeLines(c(paste("Case 3 (z, unknown σ, estimate mean) =>"),
             paste("critical =",critical,"| z =",z),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))


## Case 4: One-sample test for proportion (Chapter 8.4)
q_Ho = 1-Ho
critical = (p_hat - Ho)/(sqrt((Ho*q_Ho)/n))
# Evaluate p-value
if (bounds == 2){
  z = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(critical,lower.tail=FALSE)
} else {
  z = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(critical,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Type II Error Probability (beta)
if (Ha == "<>"){
  error_crit = ((Ho - p_hat) + (z*(sqrt((Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n) - 
    ((Ho - p_hat) - (z*(sqrt((Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = pnorm(error_crit)
} else if (Ha == ">"){
  error_crit = ((Ho - p_hat) + (z*(sqrt((Ho*q_Ho)/n))))/
              sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = pnorm(error_crit)
} else if (Ha == "<"){
  error_crit = ((Ho - p_hat) - (z*(sqrt((Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = 1 - pnorm(error_crit)
}
# Sample Size (n) - not yet coded
# Confidence Intervals (Homework 6 Question 4b)
one_lowerB = p_hat - qnorm(alpha,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
one_upperB = p_hat + qnorm(alpha,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
CI_low = p_hat - qnorm(alpha/2,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
CI_high = p_hat + qnorm(alpha/2,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
# Evaluate rejection
writeLines(c(paste("Case 4 (z, estimate proportion) =>"),
             paste("critical =",critical,"| z =",z),
             paste("Type II Error Probability:",TypeIIError),
             paste("p-value =",p_value,"|","alpha =",alpha),Eval,"*Bonus*",
             paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
             paste("One-sided confidence bounds: Lower =",one_lowerB,
                   "Upper =",one_upperB)))



## Case 5: One-sample t test for population mean (unknown σ) (Chapter 8.3)
critical = (mean - Ho)/(s/sqrt(n))
# Evaluate p-value
if (bounds == 2){
  t = qt(alpha/2,n-1,lower.tail = FALSE)
  p_value = 2*pt(critical,n-1,lower.tail=FALSE)
} else {
  t = qt(alpha,n-1,lower.tail = FALSE)
  p_value = pt(critical,n-1,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Evaluate rejection
writeLines(c(paste("Case 5 (t, estimate mean) =>"),
             paste("critical =",critical,"| t =",t),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))

