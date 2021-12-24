#library(dplyr)

class = c("Calc III", "Math Proofs", "Prob & Stats", "Stat Meth", "Linear Algebra", "CS1", "CS2", "CO2","CO1", "Electronics", 
          "Diff Eq", "Gen Physics I", "Gen Phys I Lab") # Put Class Names Here
type = c("MTH", "MTH", "MTH", "MTH", "MTH", "CS", "CS", "CS", "CS", "GEN", "MTH", "GEN", "GEN")
grades =  c(2, 2, 3, 3, 2, 4, 3, 4, 4, 3, 3, 0, 0) # Put Corresponding grades (in order w/ class name) here
                                                   # A = 4, B = 3, C = 2, D = 1, F = 0
credits = c(4, 3, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 1) # Put Corresponding amount of credits for each 
                                                   # class here (in order w/ class name, and grade)

rounding_digit <- 4 # ex. rounding_digit = 3 => GPA = 3.143, rounding_digit = 2 => GPA = 3.14
                    # default = 1

both <- data.frame(type = type, class = class,
                   grades = grades, credits = credits) # Putting names, grades, and credits into 1 variable
both <- both %>% mutate(gradexcred = grades*credits) # Calculating Weights based off grades and credits
both 
# col 1 is total credits, col 2 is total weight, col 3 is GPA
prev_gpa <- both %>% summarize(sum_cred = sum(credits), sum_gradx = sum(gradexcred),
                               gpa = round(sum_gradx/sum_cred,rounding_digit))
# Add current classes, expected grades, and amount of credits here
current_classes <- data.frame(class = c("Lin Reg", "Data Science", "Machine Learning", "Multiculturalism"),
                              type = c("MTH", "MTH", "CS", "GEN"),
                              grades = c(4, 4, 4, 4), credits = c(4, 4, 4, 3))
current_classes <- current_classes %>% mutate(gradexcred = grades * credits) # Calc weights of current classes
current_data <- rbind(both, current_classes) # Adds current classes to data frame
# This gives GPA for all classes
current_gpa <- current_data %>% summarize(sum_cred = sum(credits), sum_gradx = sum(gradexcred),
                                          gpa = round(sum_gradx/sum_cred, rounding_digit))
current_gpa
# Repeat previous process BELOW as before to add more classes and see future gpa
future_classes_fall <- data.frame(class = c("Prob Theory", "App Dev", "Music of World"),
                             type = c("MTH", "CS", "GEN"),
                             grades = c(3.3, 4, 4), credits = c(4, 4, 3))
future_classes_fall <- future_classes_fall %>% mutate(gradexcred = grades * credits)
all_data <- rbind(current_data, future_classes_fall)
all_data
future_gpa <- all_data %>% summarize(sum_cred = sum(credits), sum_gradx = sum(gradexcred),
                                     gpa = round(sum_gradx/sum_cred, rounding_digit))

combine <- rbind(prev_gpa, current_gpa, future_gpa)
semester <- c("Past", "Current", "Future")
summary <- cbind(combine, semester)
summary


all_data %>% filter(type == "CS") %>% summarize(sum_cred = sum(credits), sum_gradx = sum(gradexcred),
                                                gpa = round(sum_gradx/sum_cred, rounding_digit))
