data <- read.csv("./data/raw/unit2_starter_trees.csv")
head(data, 19)
names(data)

#--------------------------------------
# Function name : calc_ba 
# args : DBH in CM 
# RETURN : Basel Area in m^2
#--------------------------------------
calc_ba <- function(dbh){
    # calculating in m^2 
    ba <- pi * (dbh^2) / 40000
    return(ba)
}
#Function name : Calc_cv
#--------------------------------
# % Cofficent of variations : (sd /mean v) * 100
# Args1 : sd nikalna ko lagi chaine ards, mean ko lagi chaine ards, 
#
# first function : args liyera sd return gharnu pareyo
# second function agrgs, mean return garnu pareyo
# cv ko fumnctions, 
# args lincha -> sd chaine, 
# mean lai chaine -> suru ma sd call garcha, 
# result store garcha, 
# mean call garcha results store garcha ani 
# yo sd ra mean jun calculated cha tayo use garera cv calculate garcha,
#  juns arko variable ma store huncha ani 
# next functiom cv lai percentage ma change garcha ani cv %$ return garcha

# *****************************
# RETURN : CV IN %
# ****************************
#------------------------------------

# Function 1: Calculate Mean
calculate_mean <- function(x) {
  sum(x) / length(x)
}
calculate_mean(c(1,2,3,4,5,6))

# Function 2: Calculate Standard Deviation
calculate_sd <- function(x) {
  mean_value <- calculate_mean(x)
  squaring <- (x - mean_value) ^ 2
  sum_squaring <- sum(squaring)
  (sum_squaring / (length(x) - 1)) ^ 0.5
}

calculate_sd(c(1,2,3,4,5,6))


# Function 3: Calculate coefficient of variation as a percentage
measure_cv <- function(x) {
  cv_value <- calculate_sd(x) / calculate_mean(x) * 100
  return(cv_value)
}


calculate_cv(){
  #DATA GUARD
  # if only one data : cannot calculate CV : error
  # if less than 0.: faul;ty data: error 
  #if greater than 200 too big to be true : wraning
# is unavailable data : errro (NA cha ki nai)
 # if number with unit : error (20cm, 20m, 20ft, 20inch) or error namanne vaye teslai clean garnu pareyo,
  #DATA CLEANING

  # if negative cha ra data chai looks valid vane - lai + ma change garne huna sakcha 
  # or aru cleaning k k sincha
  # given data : s. robusta, Shorea robusta, Shorea robusta, Shorea robusta, Shorea robusta, Shorea
  measure_cv();

}

calculate_cv(c(10,12,13,-20))


dbh_mean <- calculate_mean(data$dbh_cm)
dbh_sd <- calculate_sd(data$dbh_cm)
dbh_cv <- calculate_cv(data$dbh_cm)

cat("Mean DBH (cm):", dbh_mean, "\n")
cat("Standard deviation of DBH (cm):", dbh_sd, "\n")
cat("Coefficient of variation of DBH (%):", dbh_cv, "\n")


# Function 1: Calculate Mean
#-----------------------------------------

#calcuate sum of observations
sum_value <- sum(x)

#count total observations 
n_value <- length(x)

#Count total observations
n_value <- length(x)

#calclate mean
mean_value <- sum_value / n_value

#Return mean
return(mean_value)

#calculate Standard Deviation
#-----------------------------------------
#square the deviations
squared_deviation <- (x - mean_value)^2

#Calculate sum of squared deviations
sum_deviation <- sum(squared_deviation)

#Count observations
n_value <- length(x)

#----------------------------------------
#Calculate variance
variance <- sum_deviation / (n_value - 1)

#calculate standard deviation
sd_value <- sqrt(variance)

#return SD
return(sd_value)

#call SD function
sd_result <- calculate_sd(x)

#call Mean function
mean_result <- calculate_mean(x)

#calculate coefficient of variation
cv_value <- sd_result / mean_result

#convert CV into percentage
cv_percent <- cv_value * 100

#Return CV in percentage
Return cv_percent



calculate_cv(20)
