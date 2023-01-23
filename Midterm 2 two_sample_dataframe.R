### Midterm 2 Two-Sample Data Frame Calculator (IE 3610) ###
## Use this to generate summary statistics to plug back into summary calculator

# ctrl+L to clear console
dev.off()  # But only if there IS a plot
# Clear console
cat("\014")  # ctrl+L
# Clear global environment
rm(list = ls())

# ------------------------------- INPUT ------------------------------------- #
DataFrame = data.frame(Header=c(5,-11,-8,-8,-3,-8,-9,-4))
Δ_null = 0
Ha = "<" # Change value in parentheses: >, <, or <>
alpha = 0.05
### END INPUT ###

# Determine alpha or alpha/2
if (Ha == "<>"){
  bounds = 2 # Ha: if not equal to (<>), then 2 ... if > or < , then 1
} else if (Ha == ">"){
  bounds = 1
} else if (Ha == "<"){
  bounds = 1
}
# ----------------------------- END INPUT --------------------------------- #

## Case 5: Paired t-test Procedure
{
  DataFrame # Check new df
  dbar = mean(DataFrame$Header) # Check sample difference mean
  sd = sd(DataFrame$Header) # Check sample standard deviation
  n = sum(table(DataFrame$Header)) # Counts your data values (n)
  # You can substitute these variables in ## First Sample ## in summary calculator
  writeLines(c(paste("Mean Difference =",dbar),
             paste("Sample Standard Deviation =",sd),
             paste("Sample Size =",n)))
  t_test = (dbar - Δ_null) / (sd/sqrt(n))
  # p-value
  if (bounds == 2){
    t_critical = qt(alpha/2,n-1,lower.tail = FALSE)
    p_value = 2*pt(abs(t_test),n-1,lower.tail = FALSE)
  } else {
    t_critical = qt(alpha,n-1,lower.tail = FALSE)
    p_value = pt(abs(t_test),n-1,lower.tail = FALSE)
  }
  # reject?
  if (p_value <= alpha){
    Eval = "Reject Ho!"
  } else {
    Eval = "Fail to reject Ho!"
  }
  # confidence intervals
  CI_lowerB = dbar - (qt(alpha,n-1,lower.tail = FALSE))*(sd/sqrt(n))
  CI_upperB = dbar + (qt(alpha,n-1,lower.tail = FALSE))*(sd/sqrt(n))
  CI_low = dbar - (qt(alpha/2,n-1,lower.tail = FALSE))*(sd/sqrt(n))
  CI_high = dbar + (qt(alpha/2,n-1,lower.tail = FALSE))*(sd/sqrt(n))
  PI_low = dbar - (qt(alpha/2,n-1,lower.tail = FALSE))*sd*sqrt(1 + (1/n))
  PI_high = dbar + (qt(alpha/2,n-1,lower.tail = FALSE))*sd*sqrt(1 + (1/n))
  # Evaluate
  writeLines(c(paste("Case 5: Paired t-test"),
               paste("Test Stat =",t_test,"| Critical Value = ±",t_critical),
               paste("p-value =",p_value,"|","alpha =",alpha),Eval,
               paste("Two-sided CI: [",CI_low,",",CI_high,"]"),
               paste("One-sided Upper: (-∞,",CI_upperB,"]"),
               paste("One-sided Lower: [",CI_lowerB,", +∞)"),
               paste("Prediction Interval:[",PI_low,",",PI_high,"]")))
}
