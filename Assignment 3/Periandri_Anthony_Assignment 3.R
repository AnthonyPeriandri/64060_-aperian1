library(caret)
library(e1071)
data <- read.csv("UniversalBank.csv")
set.seed(768)
trainset <- createDataPartition(data$Personal.Loan, p = 0.6, list = FALSE)
training <- data[trainset, ]
validation <- data[-trainset, ]
pivottable <- table(training$CreditCard, training$Online, training$Personal.Loan)
FinalTable <- as.data.frame(pivottable)
colnames(FinalTable) <- c("CreditCard", "Online", "Loan", "Count")
print(FinalTable)
A <- pivottable["1", "1", "1"]
B <- pivottable["1", "1", "0"] + pivottable["1", "1", "1"]
probability <- A / B
print(probability)
onlinevpl <- table(training$Personal.Loan, training$Online)
print(onlinevpl)
ccvpl <- table(training$Personal.Loan, training$CreditCard)
print(ccvpl)
loan_1 <- mean(training$Personal.Loan)
loan_0 <- 1-loan_1
cc_1_loan_1 <- mean(training$CreditCard[training$Personal.Loan ==1])
online_1_loan_1 <- mean(training$Online[training$Personal.Loan == 1])
cc_1_loan_0 <- mean(training$CreditCard[training$Personal.Loan == 0])
online_1_loan_0 <- mean(training$Online[training$Personal.Loan == 0])
num <- cc_1_loan_1 * online_1_loan_1 + loan_1
denom <- num + (cc_1_loan_0 * online_1_loan_0 * loan_0)
naive_bayes <- num/denom
print(naive_bayes)
cat("Probability", probability, "\n")
cat("Naive Bayes", naive_bayes, "\n")
new_model <- naiveBayes(as.factor(Personal.Loan) ~ CreditCard + Online, data = training)
new_data <- data.frame(CreditCard = 1, Online = 1)
pred_prob <- predict(new_model,new_data, type = "raw")
print(pred_prob)
cat("Normal Probability (Loan=1 | CC=1, Online=1):", round(probability, 4), "\n")
cat("Manual Naive Bayes Estimate:", round(naive_bayes, 4), "\n")
cat("NaiveBayes Model Prediction:", round(pred_prob[2], 4), "\n")