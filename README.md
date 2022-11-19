# Statistics Calculators
This repo holds my IE 3610 (Clemson University) .R files that I use to solve statistics problems.
Midterm 1: Confidence Interval calculators - Use with Confidence Interval Formula Sheet.pdf
Midterm 2: Hypothesis Testing calculators (p-value, Type II error, etc.) - Use with Hypothesis Testing Formula Sheet.pdf

HOW TO USE:
* Read your textbook question
  - Are you given statistics or a set of values? 
  - If statistics --> use summary file, if set of values --> use dataframe file
* Determine what you need to answer
  - Need to output a basic confidence interval? Use midterm 1 file.
  - Need to check a p-value against a hypothesis test? Use a midterm 2 file.
* Open your .R file in R Studio
  - Look for data entry at the top of your code
  - Set your variables based on your question
  - Run the code line by line to store the variables in R Studio (command + enter MAC) (ctrl + enter PC)
  - Find your case based on the description (Case 1, 2, 3, 4, 5, 6?) or run all of code line by line if unsure
  - Check your console for solutions
  

The "dataframe" files are designed to take in a sample of numbers provided in the problem statement (example data: 430.12, 390.91. 400.00, etc.)
* Sample standard deviation and sample mean are automatically calculated, along with the number of values entered into the dataframe variable
* Confidence intervals and other test results are returned using the print() function, all other variables can be viewed in the environment window in R studio

The "summary" files are designed to take summary statistics as inputs such as sample mean, sample standard deviation, proportion, and other values
* They have feature parity with the "dataframe" files, so equations and output functions used to solve problems should be the same

** Due to the nature of hypothesis testing and variable input, it is important to understand how your statistics formulas work before using these files. 
  I do my best to comment all of my code, but subtleties in statistical analysis such as forming accurate null and alternative hypotheses need to be understood
  so that the right formulas are executed in this code **
  
** This code has been developed and tested only in R Studio. This IDE executes code line-by-line, meaning you need to ctrl + enter (Mac: command + enter) every 
  line whenever you change your inputs. Otherwise, R Studio won't change your variables and your output won't be accurate **
 
If you find any issues or have any recommendations, email me at ipalmer100@icloud.com
Buy me a coffee? Venmo: @ipalmer100
Cheers :)
