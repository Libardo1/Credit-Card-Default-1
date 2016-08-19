# Importing required libraries
library(data.table)
library(dplyr)

# Setting working directory and fetching data
setwd("D:/R/Data")
credit_data <- fread("default.csv", header = TRUE)

# Changing default_payment column to a factor of 2 levels
credit_data$default_payment <- factor(credit_data$default_payment, labels = c("No", "Yes"), levels = c(0,1))

# Merging Education level 5 & 6 with 4
credit_data$EDUCATION[credit_data$EDUCATION > 4] <- 4

## 1st Experimental Design
# Creating table for Chi2 test
credit_table <- table(credit_data$default_payment, credit_data$EDUCATION, dnn = c("Default", "Education_level"))

# Converting the table to matrix for easier manipulation
credit_table <- as.matrix(credit_table)

# Function for calculating Chi2 value
chi2_test <- function(mat){
  
  col_total <- apply(mat, 2, sum)
  row_total <- apply(mat, 1, sum)
  grand_total <- sum(row_total)
  
  exp_val <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  chi2_val <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))
  
  for (i in 1:nrow(mat)) {
    
    exp_val[i,] <- (col_total*row_total[i])/grand_total
    chi2_val[i,] <- ((mat[i,]-exp_val[i,])^2)/exp_val[i,]
  }
  
  chi2 <- sum(chi2_val)
  
  return(chi2)
  
}

# Calculating Chi2 value
chi2 <- chi2_test(credit_table)

# Performing Chi2 test using the built-in 'chisq.test' function
chisq.test(credit_table)

# Calculating Cramer's V
sample_number <- nrow(credit_data)
df <- (nrow(credit_table) - 1)

cramers_v <- sqrt(chi2 / (sample_number * df))

## 2nd Experimental Design
# Inserting a new calculated column, filtering data to contain only the credit defaulters
credit_edu <- credit_data %>%
              mutate(CREDIT_SCORE = PAY_0 + PAY_2 + PAY_3 + PAY_4 +                 PAY_5 + PAY_6) %>%
              filter(default_payment == "Yes") %>%
              select(ID, EDUCATION, CREDIT_SCORE)

# Taking the samples
edu_level <- sort(unique(credit_edu$EDUCATION))

sample_size <- 33

edu_samp <- matrix(0, nrow = sample_size, ncol = length(edu_level))

for (i in edu_level) {
  data <- credit_edu %>%
          filter(EDUCATION == i) %>%
          select(CREDIT_SCORE)
  edu_samp[,i] <- data[1:sample_size,]
}

colnames(edu_samp) <- c(1:4)

summary(edu_samp)

# Visualizing the samples
boxplot(edu_samp, main = "Credit Score of Samples", ylab = "Values", xlab = "Education Level", col = rainbow(6))

# Calculating F value and Eta2

grand_mean <- mean(edu_samp)

mean_edu <- colMeans(edu_samp)

SSb <- sample_size * sum((mean_edu - grand_mean)^2)

ssw_mat <- matrix(0, nrow = sample_size, ncol = length(edu_level))

for (i in edu_level) {
  ssw_mat[,i] <- (edu_samp[,i] - mean_edu[i])^2
}

SSw <- sum(ssw_mat)

k <- length(edu_level)

df_b <- k - 1

df_w <- (sample_size*k) - k 

MSb <- SSb / df_b

MSw <- SSw / df_w

F_val <- MSb / MSw

eta2 <- SSb / (SSb + SSw)

# Using the built in aov command

one <- edu_samp[,1]
two <- edu_samp[,2]
three <- edu_samp[,3]
four <- edu_samp[,4]

edu_samp2 <- data.frame(one, two, three, four)

edu_samp2 <- stack(edu_samp2)

result <- aov(values ~ ind, data = edu_samp2)

summary(result)

# Calculating and plotting Tukey's HSD
tukey <- TukeyHSD(result)

plot(tukey)

