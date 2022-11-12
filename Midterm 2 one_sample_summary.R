### Midterm 2 Summary Statistics Calculator (IE 3610) ###
# ctrl+L to clear console
rm(list = ls()) # Run to clear global environment


### DATA ENTRY ###
# Fill in Ho (null hypothesis) and other parameters
Ho = 11/100
Ha = ">" # Change value in parentheses: >, <, or <>
n = 100
alpha = 0.05
p_hat = 15/100 # Case 4 only
# Fill in mean and standard deviation
mean = 31000
# sample (s) OR population (s_pop = σ (Case 1 only))
s = 3
s_pop = 1500
### END DATA ENTRY ### 

# RUN THIS: variable conditionals 
if (Ha == "<>"){
  bounds = 2 # Ha: if not equal to (<>), then 2 ... if > or < , then 1
} else if (Ha == ">"){
  bounds = 1
} else if (Ha == "<"){
  bounds = 1
}



## Case 1: z test for population mean (known σ) (Chapter 8.2)
test_stat = (mean - Ho)/(s_pop/sqrt(n)) #Statistic to reference by hand
abs_stat = abs((mean - Ho)/(s_pop/sqrt(n))) #Statistic for computation
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_stat,lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_stat,lower.tail=FALSE)
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
             paste("Test Stat =",test_stat,"| Critical Value = ±",z_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))


## Case 3: Non-normal population, large sample (unknown σ) (Chapter 8.2)
test_stat = (mean - Ho)/(s/sqrt(n)) #Statistic to reference by hand
abs_stat = abs((mean - Ho)/(s/sqrt(n))) #Statistic for computation
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_stat,lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_stat,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Evaluate 
writeLines(c(paste("Case 3 (z, unknown σ, estimate mean) =>"),
             paste("Test Stat =",test_stat,"| Critical Value = ±",z_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))


## Case 4: One-sample test for proportion (Chapter 8.4)
q_Ho = 1-Ho
test_stat = (p_hat - Ho)/(sqrt((Ho*q_Ho)/n))
abs_stat = abs((p_hat - Ho)/(sqrt((Ho*q_Ho)/n)))
# Evaluate p-value
if (bounds == 2){
  z_critical = qnorm(alpha/2,lower.tail = FALSE)
  p_value = 2*pnorm(abs_stat,lower.tail=FALSE)
} else {
  z_critical = qnorm(alpha,lower.tail = FALSE)
  p_value = pnorm(abs_stat,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Type II Error Probability (beta)
if (Ha == "<>"){
  error_crit = ((Ho - p_hat) + (z_critical*(sqrt((Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n) - 
    ((Ho - p_hat) - (z_critical*(sqrt((Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = pnorm(error_crit,lower.tail = FALSE)
} else if (Ha == ">"){
  error_crit = ((Ho - p_hat) + (z_critical*(sqrt((Ho*q_Ho)/n))))/
              sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = pnorm(error_crit,lower.tail = FALSE)
} else if (Ha == "<"){
  error_crit = ((Ho - p_hat) - (z_critical*(sqrt((Ho*q_Ho)/n))))/
    sqrt((p_hat*(1-p_hat))/n)
  TypeIIError = 1 - pnorm(error_crit,lower.tail = FALSE)
}
# Sample Size (n) - not yet coded
# Confidence Intervals (Homework 6 Question 4b)
one_lowerB = p_hat - qnorm(alpha,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
one_upperB = p_hat + qnorm(alpha,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
CI_low = p_hat - qnorm(alpha/2,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
CI_high = p_hat + qnorm(alpha/2,lower.tail = FALSE)*(sqrt((Ho*(q_Ho)/n)))
# Evaluate rejection
writeLines(c(paste("Case 4 (z, estimate proportion) =>"),
             paste("Test Stat =",test_stat,"| Critical Value = ±",z_critical),
             paste("Type II Error Probability:",TypeIIError),
             paste("p-value =",p_value,"|","alpha =",alpha),Eval,"*Bonus*",
             paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
             paste("One-sided confidence bounds: Lower =",one_lowerB,
                   "Upper =",one_upperB)))



## Case 5: One-sample t test for population mean (unknown σ) (Chapter 8.3)
test_stat = (mean - Ho)/(s/sqrt(n))
abs_stat = abs((mean - Ho)/(s/sqrt(n)))
# Evaluate p-value
if (bounds == 2){
  t_critical = qt(alpha/2,n-1,lower.tail = FALSE)
  p_value = 2*pt(abs_stat,n-1,lower.tail=FALSE)
} else {
  t_critical = qt(alpha,n-1,lower.tail = FALSE)
  p_value = pt(abs_stat,n-1,lower.tail=FALSE)
}
if (p_value <= alpha){
  Eval = "Reject Ho!"
} else {
  Eval = "Fail to reject Ho!"
}
# Evaluate rejection
writeLines(c(paste("Case 5 (t, estimate mean) =>"),
             paste("Test Stat =",test_stat,"| Critical Value = ±",t_critical),
             paste("p-value =",p_value,"|","alpha =", alpha),Eval))

