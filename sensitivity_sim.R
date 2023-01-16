mainwd = "YOUR_MAIN_PATH"
setwd(mainwd)

set.seed(1)

fracsub=1
err_tot = 0.1
K = 200
frac_samp = 0.25

# simulation setting
simNumber = 2
# simNumber = 3
# simNumber = 4

###################################
# 1.
###################################

res = list()

for (err_i in err_tot) {

  # quantiles based on the full shallow chamber
  load(paste0("data/quantiles_subLoc",fracsub, ".RData"))
  dat = dat[complete.cases(dat), ]
  ind = dat$simSetting == simNumber
  datsimu2 = dat[ind,]
  datsimu2_aspectRatio = datsimu2$aspectRatio[1] + rnorm(K, 0, err_i*datsimu2$aspectRatio[1])
  datsimu2_rho = datsimu2$deltaRho[1] + rnorm(K, 0, err_i*datsimu2$deltaRho[1])
  N_simu2 = sum(ind)
  M = round(frac_samp*N_simu2)
  M = round(frac_samp*N_simu2)
  ind = sample(1:N_simu2, M)
  simu2_to_test = datsimu2[ind,]
  time_to_predict_2 = simu2_to_test$time

  aspectRatio = unique(simu2_to_test$simSetting)
  deltaRho = unique(simu2_to_test$deltaRho)
  time = median(simu2_to_test$time)

  ci_time_2 = matrix(NA, M, 2)
  mean_time_2 = rep(NA, M)

  for (i in 1:M){

    obs_vec = c(simu2_to_test$q_0.05[i], simu2_to_test$q_0.25[i],
                simu2_to_test$q_0.5[i], simu2_to_test$q_0.75[i],
                simu2_to_test$q_0.95[i])

    Omega = obj_fun_pred_time_getOmega(obs_vec = obs_vec,
                                       mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                                       aspectRatio = aspectRatio,
                                       deltaRho = deltaRho,
                                       time = time)

    fit_time = rep(NA, K)
    for (j in 1:K){
      fit_time[j] = optimize(obj_fun_pred_time, interval = range(dat$time),
                             obs_vec = obs_vec,
                             mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                             aspectRatio = datsimu2_aspectRatio[j], deltaRho = datsimu2_rho[j])$minimum
    }

    mean_time_2[i] = mean(fit_time)
    ci_time_2[i,] = quantile(fit_time, probs = c(alpha/2, 1 - alpha/2))

    print(i)
  }

  prediction_simu2 = mean_time_2
  true_value_simu2 = time_to_predict_2


  res[[which(err_i==err_tot)]] = cbind.data.frame(Predicted=prediction_simu2/3600, Simulated=true_value_simu2/3600,
                                                  CIlow = ci_time_2[,1]/3600, CIhigh = ci_time_2[,2]/3600)
}


###################################
# 2.
###################################

res2 = list()

for (err_i in err_tot) {

  # quantiles based on the full shallow chamber
  load(paste0("data/quantiles_subLoc", fracsub, ".RData"))
  dat = dat[complete.cases(dat), ]
  ind = dat$simSetting == simNumber
  datsimu2 = dat[ind,]

  datsimu2_aspectRatio = datsimu2$aspectRatio[1]
  datsimu2_rho = datsimu2$deltaRho[1]
  N_simu2 = sum(ind)
  M = round(frac_samp*N_simu2)
  ind = sample(1:N_simu2, M)
  simu2_to_test = datsimu2[ind,]
  time_to_predict_2 = simu2_to_test$time

  ci_time_2 = matrix(NA, M, 2)
  mean_time_2 = rep(NA, M)

  for (i in 1:M){

    obs_vec = c(simu2_to_test$q_0.05[i], simu2_to_test$q_0.25[i],
                simu2_to_test$q_0.5[i], simu2_to_test$q_0.75[i],
                simu2_to_test$q_0.95[i])

    Omega = obj_fun_pred_time_getOmega(obs_vec = obs_vec,
                                       mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                                       aspectRatio = aspectRatio,
                                       deltaRho = deltaRho,
                                       time = time)

    fit_time = rep(NA, K)
    for (j in 1:K){
      obs_vec_random = rnorm(length(obs_vec), obs_vec, err_i*obs_vec)
      fit_time[j] = optimize(obj_fun_pred_time, interval = range(dat$time),
                             obs_vec = obs_vec_random,
                             mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                             aspectRatio = datsimu2_aspectRatio[1], deltaRho = datsimu2_rho[1])$minimum
    }

    mean_time_2[i] = mean(fit_time)
    ci_time_2[i,] = quantile(fit_time, probs = c(alpha/2, 1 - alpha/2))

    print(i)
  }

  prediction_simu2 = mean_time_2 # [1:111]
  true_value_simu2 = time_to_predict_2 # [1:111]

  res2[[which(err_i==err_tot)]] = cbind.data.frame(Predicted=prediction_simu2/3600, Simulated=true_value_simu2/3600,
                                                   CIlow = ci_time_2[,1]/3600, CIhigh = ci_time_2[,2]/3600)
}


###################################
# 3.
###################################


res3 = list()

for (err_i in err_tot) {

  # quantiles based on the full shallow chamber
  load(paste0("data/quantiles_subLoc",fracsub, ".RData"))
  dat = dat[complete.cases(dat), ]
  ind = dat$simSetting == simNumber
  datsimu2 = dat[ind,]

  datsimu2_aspectRatio = datsimu2$aspectRatio[1] + rnorm(K, 0, err_i*datsimu2$aspectRatio[1])
  datsimu2_rho = datsimu2$deltaRho[1] + rnorm(K, 0, err_i*datsimu2$deltaRho[1])
  N_simu2 = sum(ind)
  M = round(frac_samp*N_simu2)
  ind = sample(1:N_simu2, M)
  simu2_to_test = datsimu2[ind,]
  time_to_predict_2 = simu2_to_test$time

  ci_time_2 = matrix(NA, M, 2)
  mean_time_2 = rep(NA, M)

  for (i in 1:M){

    obs_vec = c(simu2_to_test$q_0.05[i], simu2_to_test$q_0.25[i],
                simu2_to_test$q_0.5[i], simu2_to_test$q_0.75[i],
                simu2_to_test$q_0.95[i])

    Omega = obj_fun_pred_time_getOmega(obs_vec = obs_vec,
                                       mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                                       aspectRatio = aspectRatio,
                                       deltaRho = deltaRho,
                                       time = time)

    fit_time = rep(NA, K)
    for (j in 1:K){
      obs_vec_random = rnorm(length(obs_vec), obs_vec, err_i*obs_vec)
      fit_time[j] = optimize(obj_fun_pred_time, interval = range(dat$time),
                             obs_vec = obs_vec_random,
                             mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                             aspectRatio = datsimu2_aspectRatio[1], deltaRho = datsimu2_rho[1])$minimum
    }

    mean_time_2[i] = mean(fit_time)
    ci_time_2[i,] = quantile(fit_time, probs = c(alpha/2, 1 - alpha/2))

    print(i)
  }

  prediction_simu2 = mean_time_2
  true_value_simu2 = time_to_predict_2

  res3[[which(err_i==err_tot)]] = cbind.data.frame(Predicted=prediction_simu2/3600, Simulated=true_value_simu2/3600,
                                                   CIlow = ci_time_2[,1]/3600, CIhigh = ci_time_2[,2]/3600)
}

save.image(file = paste0("data/tmp/fig6_sim", simNumber, ".RData"))
# load("paste0("data/tmp/fig6_sim", simNumber, ".RData")")
