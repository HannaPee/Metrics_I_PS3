########################
######### PS 3 #########
########################

#load packages#
library(dplyr)
library(ggplot2)
library(readr)

##### Q 1 ################

### d ###

N <- c(25, 50, 100, 250, 500) #samples sizes 

for (n in N) {
  
  for (i in 1:100) {
  
  x_i_n <- rpois(n, 2)
  
  x_bar <- mean(x_i_n) 
  
  s2 <- mean((x_i_n - x_bar)^2)
  
  lambda_hat_MLE_n_i <- sum(x_i_n)/n
  
  lambda_hat_GMM_n_i <- (x_bar + s2)/2
  
    if (i == 1) {
  
    est_n <- tibble(
    lambda_hat_MLE = lambda_hat_MLE_n_i, 
    lambda_hat_GMM = lambda_hat_GMM_n_i, 
    i = i,  
    n = n 
      )
    }
  
    else{ 
    est_n <- rbind(est_n, tibble(
    lambda_hat_MLE = lambda_hat_MLE_n_i, 
    lambda_hat_GMM = lambda_hat_GMM_n_i, 
    i = i,  
    n = n

    ))
    }
  }
  
  #Calculate mean estimtors#
  
  lambda_MLE_bar_n <- sum(est_n$lambda_hat_MLE) / 100
  var_lambda_MLE_n <- (1 / 100) * sum((est_n$lambda_hat_MLE - 2)^2)
  
  lambda_GMM_bar_n <- sum(est_n$lambda_hat_GMM)/100
  var_lambda_GMM_n <- (1 / 100) * sum((est_n$lambda_hat_GMM - 2)^2)
  
  if (n == 25) {
    
    est_all <- tibble(  
    lambda_MLE_bar = lambda_MLE_bar_n, 
    var_lambda_MLE = var_lambda_MLE_n, 
    
    lambda_GMM_bar = lambda_GMM_bar_n, 
    var_lambda_GMM = var_lambda_GMM_n, 
    
    n = n 
    )
  }
  
  else {
    est_all <- rbind(est_all, tibble(
      lambda_MLE_bar = sum(est_n$lambda_hat_MLE) / 100, 
      var_lambda_MLE = (sum(est_n$lambda_hat_MLE - 2))^2 / 100, 
      
      lambda_GMM_bar = sum(est_n$lambda_hat_GMM)/100, 
      var_lambda_GMM = (sum(est_n$lambda_hat_GMM - 2))^2 / 100, 
      
      n = n 
    ))
  }
}


# Plot the estimators over n # 

# Lambda bar # 

plot_1 <-ggplot(est_all, aes(x = n)) +
  geom_line(aes(y = lambda_MLE_bar, color = "MLE")) +
  geom_point(aes(y = lambda_MLE_bar, color = "MLE")) +
  geom_line(aes(y = lambda_GMM_bar, color = "GMM")) +
  geom_point(aes(y = lambda_GMM_bar, color = "GMM")) +
  labs(
    y = "Mean λ",
    color = "Estimator"
  ) +
  theme_minimal()

print(plot_1)

ggsave(
  filename = "lambda_bar_estimates.pdf",
  plot     = plot_1,
  width    = 8,
  height   = 6
)

# Variance # 

plot_2 <-ggplot(est_all, aes(x = n)) +
  geom_line(aes(y = var_lambda_MLE, color = "MLE")) +
  geom_point(aes(y = var_lambda_MLE, color = "MLE")) +
  geom_line(aes(y = var_lambda_GMM, color = "GMM")) +
  geom_point(aes(y = var_lambda_GMM, color = "GMM")) +
  labs(
    y = "Variance λ",
    color = "Estimator"
  ) +
  theme_minimal()

print(plot_2)

ggsave(
  filename = "var_lambda_estimates.pdf",
  plot     = plot_2,
  width    = 8,
  height   = 6
)

############## Q2 ###########################


######### d ##############

#Import dataset # 
df_2 <- read_csv("ProblemSet3data.csv")

#### Run a grid search algorithm ### 

# Define objective function # 



obj <- function(r,D,k,B){
  
  
  
}


