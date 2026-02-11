########################
######### PS 3 #########
########################

#load packages#
library(dplyr)
library(ggplot2)
library(readr)
library(xtable)

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

obj <- function(alpha, r,D,k,B){
  g <- r - D * (1 - pmin(B / (alpha * k), 1))
  mean(g)^2
}

# Change dataset# 

df_2_updated <- df_2 %>%
  mutate(k_tp1 = lead(k),
         r_tp1 = lead(r))%>%
  filter(!is.na(r_tp1), 
         !is.na(k_tp1)) %>%
  filter(
    !is.na(k_tp1),
    !is.na(B),
    !is.na(D),
    !is.na(r_tp1),
    k_tp1 > 0
  )

# Run the minimization problem over a grid# 

alpha_grid <- seq(0.01, 1, length.out = 1000)

#Evaluate the function at grid#

for (a in alpha_grid){
  func_val <- obj(a, 
                  r = df_2_updated$r_tp1, 
                  D = df_2_updated$D, 
                  k = df_2_updated$k_tp1, 
                  B = df_2_updated$B)
  
  if (a ==0.01) {
    df_grid <- tibble(
      func_val = func_val, 
      a = a)
  }
  
  else 
    df_grid <- rbind(df_grid, tibble(
      func_val = func_val, 
      a = a))
}

#Choosing the minimizer

alpha_hat <- df_grid %>%
  slice_min(func_val, n = 1) %>%
  pull(a)


#Plotting over alphas# 

plot_3 <- ggplot(df_grid) +
  aes(x = a, y = func_val)+ 
  geom_smooth() +
  labs(
    y = "Function value") + 
  theme_minimal()
              
print(plot_3)

ggsave(
  filename = "grid_seacrh_alpha.pdf",
  plot     = plot_3,
  width    = 8,
  height   = 6
)



#################### Q3 ####################

# Simulating data # 

tax_rev <- rnorm(500, mean = 19, sd = 1)
intgov_trans <- rnorm(500, mean = 1, sd = 1)
u <- rnorm(500, mean = 0, sd = 1)
e <- rnorm(500, mean = 0, sd = 1)

# Creating variables 

fund <- tax_rev + intgov_trans
local_inc <- tax_rev - u
test_score <- tax_rev - intgov_trans + e

# Assemble in a df # 

df_schools <- tibble(tax_rev, intgov_trans,fund,local_inc,test_score)

# Calculating fraction of funding from local tax revenue# 

df_schools <- df_schools %>%
  mutate(frac_tax_rev = tax_rev/fund)

mean_frac <- mean(df_schools$frac_tax_rev)
print(mean_frac)

#Plotting the average fraction# 

pdf("hist_frac_tax_rev.pdf", width = 7, height = 5)

plot_4 <- hist(
  df_schools$frac_tax_rev,
  col  = "steelblue",
  main = "",                 # no title
  xlab = "Fraction of tax revenue",
  ylab = "Frequency"
)


dev.off() 


###### c ######

#Regressing child scores on tax revenue and intergovernmental transfers # 

# solve OLS # 
OLS_estim <- function(y, x) {
  
  X <- cbind(Intercept = 1, as.matrix(x))
  Y <- as.matrix(y)
  
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
  
  rownames(beta_hat) <- colnames(X)
  colnames(beta_hat) <- "Estimate"
  
  # Residuals and SEs
  e_hat <- Y - X %*% beta_hat
  n <- nrow(X)
  k <- ncol(X)
  
  sigma2_hat <- as.numeric(t(e_hat) %*% e_hat / (n - k))
  vcov_beta <- sigma2_hat * solve(t(X) %*% X)
  se_beta <- sqrt(diag(vcov_beta))
  
  results <- data.frame(
    Estimate = as.numeric(beta_hat),
    `Std. Error` = se_beta,
    row.names = colnames(X)
  )
  
  print(
    xtable::xtable(results, digits = 3),
    type = "latex",
    include.rownames = TRUE
  )
  
  return(results)
}


## Run regression ##


#Without control # 

ols_1 <- OLS_estim(df_schools$test_score, df_schools[, c("tax_rev", "intgov_trans")])

#With control # 

ols_2 <- OLS_estim(df_schools$test_score, df_schools[, c("tax_rev", "intgov_trans","local_inc")])

######## d #########

#Estimating funding on test score# 

ols_3 <- OLS_estim(df_schools$test_score, df_schools[, c("fund")]) 

####### e ###########

# Correlation between local average income and funding # 

rho <- cor(df_schools$fund, df_schools$local_inc,use = "complete.obs")

corr_table <- data.frame(
  Correlation = rho
)

print(
  xtable(corr_table, digits = 3),
  type = "latex",
  include.rownames = FALSE
)


######### f #############

#Regressing test score on funding, controlling for local income # 

ols_4 <- OLS_estim(df_schools$test_score, df_schools[, c("fund", "local_inc")])










