# A basic script for calculating your required final exam grade in order to
# obtain a B- or higher in a course based on your current progress. Allows
# for percentage or fractional grade input.
# For simplicity, no action has been taken to prevent invalid input from
# the user, such as negative/textual grades, invalid weights, etc.

library(stringr)

# Collection step
marks <- numeric(0)
weights <- numeric(0)

running <- TRUE

while (running){
  m <- readline("Enter mark as percentage or fraction (`e` to exit): ")
  
  if (m == "e"){
    running <- FALSE
  } else {
    w <- as.numeric(readline("Enter weight (%): "))
    weights <- append(weights, values=w)
    
    if (str_detect(m, pattern="/")){
      ## Fractional grade entered
      frac <- as.numeric(str_split(m, pattern="/", n=2, simplify=TRUE))
      decimalGrade <- frac[1]/frac[2]
      marks <- append(marks, values=decimalGrade)
    } else {
      ## Percentage grade entered
      decimalGrade <- as.numeric(m)/100
      marks <- append(marks, values=decimalGrade)
    }
  }
}


# Computation step
examWeight <- 100 - sum(weights)
points <- sum(marks*weights)

## `gradeCut` can be modified to suit your needs
gradeCut <- c(70, 75, 80, 85, 90)
finalExamGrade <- (gradeCut - points)/(examWeight/100)


# Output
for (i in 1:length(gradeCut)){
  cat("\n", sprintf("For a final grade of %d%% you need a %.2f%% on the final exam.",
                    gradeCut[i], finalExamGrade[i]), sep="")
}

