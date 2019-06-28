source("run_rjags_model.R")
raw_df <- read.csv('../data/raw_data.csv', header=TRUE)

# some eda
df <- raw_df
names(df) <- c('score', 'merchant_1', 'special_1', 'special_2', 'y')
# df$score = sapply(df$score, function(x) -log(1/x-1))
summary(df)

# base model
base_string = " model {
for (i in 1:length(y)) {
  y[i] ~ dbern(p[i])
  logit(p[i]) = b0 + b[1]*score[i] + b[2]*merchant_1[i] + b[3]*special_1[i] + b[4]*special_2[i]
}
b0 ~ dnorm(0.0, 1.0/10^2)
for (j in 1:4) {
  b[j] ~ dnorm(0.0, 1/10^2)
}
}
"
base_params = c("b0", 'b')
mod_base_dict = run_all(df, base_string, base_params)


# mixed intercepts model
mixed_int_string = " model {
for (i in 1:length(y)) {
  y[i] ~ dbern(p[i])
  logit(p[i]) = a[merchant[i]] + b[1]*score[i] + b[2]*special_1[i] + b[3]*special_2[i]
}
for (j in 1:max(merchant)) {
    a[j] ~ dnorm(a0, prec_a)
}

a0 ~ dnorm(0.0, 1/10^2)
prec_a ~ dgamma(1/2.0, 1/10./2.0)


for (j in 1:3) {
  b[j] ~ dnorm(0.0, 1/10^2) 
}
}
"
mixed_int_params = c('a', 'b')
df_int <- df
df_int$merchant = df_int$merchant_1 + 1
mixed_int_dict = run_all(df_int, mixed_int_string, mixed_int_params)

# hierarchical model


hier_string = " model {
for (i in 1:length(y)) {
  y[i] ~ dbern(p[i])
  logit(p[i]) = b0 + b[1]*score[i] + b[2]*merchant_1[i] + r[st[i]]
}

b0 ~ dnorm(0.0, 1.0/10^2)

for (rf in 1:max(st)){
  r[rf] ~ dnorm(0, rtau) #
}

rtau ~ dgamma(1/2.0, 1/10./2.0)

for (j in 1:2) {
  b[j] ~ dnorm(0.0, 1/10^2) 
}
}
"

hier_params = c('b0', 'b', 'r')
df_hier <- df
df_hier$st = df_hier$merchant_1 + 1 + df$special_1 * 2 + df$special_2 * 2
hier_dict = run_all(df_hier, hier_string, hier_params)




#############################
logit <- function(x){
  return (1/ (1 + exp(-x)))
}
mixed_coefs <- colMeans(mixed_int_dict$csim)
y_pred_1 <- logit(mixed_coefs["a[1]"] + 
                    as.matrix(df_int[df_int$merchant == 1, c(1, 3,4)]) %*% mixed_coefs[c(3,4,5)])
y_pred_2 <- logit(mixed_coefs["a[2]"] + 
                    as.matrix(df_int[df_int$merchant == 2, c(1, 3,4)]) %*% mixed_coefs[c(3,4,5)])
y_preds <- c(y_pred_1, y_pred_2)
y <- c(df_int[df_int$merchant == 1, 5], df_int[df_int$merchant == 2, 5])
library(pROC)
plot.roc(y, y_preds, print.auc=TRUE)
