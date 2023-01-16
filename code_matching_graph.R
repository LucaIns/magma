mainwd = "YOUR_MAIN_PATH"
setwd(mainwd)

#############################
# Utilities
#############################

gg_color_hue <- function(n, alpha) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alpha)[1:n]
}

obj_fun_pred_time = function(time, obs_vec, mod1, mod2, mod3, mod4, mod5, aspectRatio, deltaRho){
  to_pred = data.frame(time = time, aspectRatio = aspectRatio, deltaRho = deltaRho)
  model_pred1 = predict(mod1, to_pred, se.fit = TRUE)
  model_pred2 = predict(mod2, to_pred, se.fit = TRUE)
  model_pred3 = predict(mod3, to_pred, se.fit = TRUE)
  model_pred4 = predict(mod4, to_pred, se.fit = TRUE)
  model_pred5 = predict(mod5, to_pred, se.fit = TRUE)
  pred_vec = as.numeric(c(model_pred1$fit, model_pred2$fit, model_pred3$fit, model_pred4$fit, model_pred5$fit))
  delta = (pred_vec - obs_vec)
  Omega = diag(as.numeric(c(1/(model_pred1$se.fit^2),
                            1/(model_pred2$se.fit^2),
                            1/(model_pred3$se.fit^2),
                            1/(model_pred4$se.fit^2),
                            1/(model_pred5$se.fit^2))))
  Omega = Omega/max(Omega)
  t(delta)%*%Omega%*%delta
}

# same as the function on top, but returns Omega instead
obj_fun_pred_time_getOmega = function(time, obs_vec, mod1, mod2, mod3, mod4, mod5, aspectRatio, deltaRho){
  to_pred = data.frame(time = time, aspectRatio = aspectRatio, deltaRho = deltaRho)
  model_pred1 = predict(mod1, to_pred, se.fit = TRUE)
  model_pred2 = predict(mod2, to_pred, se.fit = TRUE)
  model_pred3 = predict(mod3, to_pred, se.fit = TRUE)
  model_pred4 = predict(mod4, to_pred, se.fit = TRUE)
  model_pred5 = predict(mod5, to_pred, se.fit = TRUE)
  pred_vec = as.numeric(c(model_pred1$fit, model_pred2$fit, model_pred3$fit, model_pred4$fit, model_pred5$fit))
  delta = (pred_vec - obs_vec)
  Omega = diag(as.numeric(c(1/(model_pred1$se.fit^2),
                            1/(model_pred2$se.fit^2),
                            1/(model_pred3$se.fit^2),
                            1/(model_pred4$se.fit^2),
                            1/(model_pred5$se.fit^2))))
  Omega/max(Omega)
}

#############################
# Model fit
#############################

# fraction of sub-sampled locations
fracsub=1

# quantiles based on the full upper chamber
load(paste0("data/quantiles_subLoc",fracsub, ".RData"))
dat = dat[complete.cases(dat), ]

par(mfrow=c(3,2))
hist(dat$q_0.05)
hist(dat$q_0.25)
hist(dat$q_0.5)
hist(dat$q_0.75)
hist(dat$q_0.9)
par(mfrow=c(1,1))

mod1 = lm(q_0.05 ~ time*aspectRatio*deltaRho + I(time^2):aspectRatio*deltaRho +
            + I(time^3):aspectRatio*deltaRho + I(time^4):aspectRatio*deltaRho, x=1, data = dat)
summary(mod1)
plot(mod1$residuals, type = "l")
hist(mod1$residuals)
length(mod1$residuals)
acf(mod1$residuals)

mod2 = lm(q_0.25 ~ time*aspectRatio*deltaRho + I(time^2):aspectRatio*deltaRho +
            + I(time^3):aspectRatio*deltaRho + I(time^4):aspectRatio*deltaRho, x=1, data = dat)
summary(mod2)
plot(mod2$residuals, type = "l")
hist(mod2$residuals)
length(mod2$residuals)
acf(mod2$residuals)

mod3 = lm(q_0.5 ~ time*aspectRatio*deltaRho + I(time^2):aspectRatio*deltaRho +
            + I(time^3):aspectRatio*deltaRho + I(time^4):aspectRatio*deltaRho, x=1, data = dat)
summary(mod3)
plot(mod3$residuals, type = "l")
hist(mod3$residuals)
length(mod3$residuals)
acf(mod3$residuals)

mod4 = lm(q_0.75 ~ time*aspectRatio*deltaRho + I(time^2):aspectRatio*deltaRho +
            + I(time^3):aspectRatio*deltaRho + I(time^4):aspectRatio*deltaRho, x=1, data = dat)
summary(mod4)
plot(mod4$residuals, type = "l")
hist(mod4$residuals)
length(mod4$residuals)
acf(mod4$residuals)

mod5 = lm(q_0.95 ~ time*aspectRatio*deltaRho + I(time^2):aspectRatio*deltaRho +
            + I(time^3):aspectRatio*deltaRho + I(time^4):aspectRatio*deltaRho, x=1, data = dat)
summary(mod5)
plot(mod5$residuals, type = "l")
hist(mod5$residuals)
length(mod5$residuals)
acf(mod5$residuals)

#############################
# Accuracy
#############################
alpha = 0.05
counter = 0
results = list()
# Let's use simulation 2-4
for (k in 2:4){
  ind = dat$simSetting == k
  datsimu2 = dat[ind,]
  datsimu2_aspectRatio = datsimu2$aspectRatio[1]
  datsimu2_rho = datsimu2$deltaRho[1]
  N_simu2 = sum(ind)
  M = round(1*N_simu2)
  ind = sample(1:N_simu2, M)
  simu2_to_test = datsimu2[ind,]
  time_to_predict_2 = simu2_to_test$time

  ci_time_2 = matrix(NA, M, 2)
  mean_time_2 = rep(NA, M)

  for (i in 1:M){
    obs_vec = c(simu2_to_test$q_0.05[i], simu2_to_test$q_0.25[i],
                simu2_to_test$q_0.5[i], simu2_to_test$q_0.75[i],
                simu2_to_test$q_0.95[i])

    mean_time_2[i] = optimize(obj_fun_pred_time, interval = range(dat$time),
                              obs_vec = obs_vec,
                              mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                              aspectRatio = datsimu2_aspectRatio, deltaRho = datsimu2_rho)$minimum
  }

  res = cbind(mean_time_2, time_to_predict_2)
  colnames(res) = c("Predicted", "Simulated")
  counter = counter + 1
  results[[counter]] = res
}

cols = gg_color_hue(3, alpha = 0.9)

res_fin = rbind.data.frame(cbind.data.frame(results[[1]], sim="$\\Delta\\rho$ = 20 kg/m$^3$, $a$ = 2", col=cols[1]),
                           cbind.data.frame(results[[2]], sim="$\\Delta\\rho$ = 30 kg/m$^3$, $a$ = 1", col=cols[2]),
                           cbind.data.frame(results[[3]], sim="$\\Delta\\rho$ = 160 kg/m$^3$, $a$ = 0.5", col=cols[3]))

names(res_fin)
res_fin[,1:2] = res_fin[,1:2]/3600

save.image(file = "data/tmp/fig4.RData")
# load("data/tmp/fig4.RData")
