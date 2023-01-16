mainwd = "YOUR_MAIN_PATH"
setwd(mainwd)

set.seed(123)

subChamber_tot = 0:2
# NOTE:
  # subChamber = 0 # uses the whole shallow chamber data
  # subChamber = 1 # uses only upper half shallow chamber data
  # subChamber = 2 # uses only lower half shallow chamber data

res_all = list()
ind_pl = 1

for (subChamber in subChamber_tot) {

  if (subChamber==0) {
    titData = ".RData"
  } else if (subChamber==1) {
    titData = "_upper.RData"
  } else if (subChamber==2) {
    titData = "_lower.RData"
  }

  sim_tot = 2:4
  frac_sub_tot = c(1, 0.6, 0.3, 0.1)

  for (sim_i in sim_tot) {

    sol = list()

    for (frac_sub_i in frac_sub_tot) {

      fracsub = frac_sub_i
      ksim = sim_i

      # quantiles based on the full/lower half/upper half of the shallow chamber
      load(paste0("data/quantiles_subLoc",fracsub, titData))
      dat = dat[complete.cases(dat), ]

      ind = dat$simSetting == ksim
      datsimu_i = dat[ind,]
      datsimu_i_aspectRatio = datsimu_i$aspectRatio[1]
      datsimu_i_rho = datsimu_i$deltaRho[1]
      N_simu_i = sum(ind)

      M = round(1*N_simu_i)
      ind = sample(1:N_simu_i, M)
      simu_i_to_test = datsimu_i[ind,]
      time_to_predict_i = simu_i_to_test$time

      mean_time = rep(NA, M)

      for (i in 1:M){

        obs_vec = c(simu_i_to_test$q_0.05[i], simu_i_to_test$q_0.25[i],
                    simu_i_to_test$q_0.5[i], simu_i_to_test$q_0.75[i],
                    simu_i_to_test$q_0.95[i])

        mean_time[i] = optimize(obj_fun_pred_time, interval = range(dat$time),
                                obs_vec = obs_vec,
                                mod1 = mod1, mod2 = mod2, mod3 = mod3, mod4 = mod4, mod5 = mod5,
                                aspectRatio = datsimu_i_aspectRatio, deltaRho = datsimu_i_rho)$minimum

        if (i %% 100 == 0) {
          print(paste0("sub-chamb: ", subChamber, "; simul: ", sim_i,
                       "; sub-sampling: ", frac_sub_i, "; sample: ",i))
        }
      }

      sol[[which(frac_sub_i==frac_sub_tot)]] = cbind.data.frame(predicted=mean_time,
                                                                simulated=time_to_predict_i)

    }

    res_fin_2 = rbind.data.frame(cbind.data.frame(sol[[1]], sub=frac_sub_tot[1], col=cols[1]),
                                 cbind.data.frame(sol[[2]], sub=frac_sub_tot[2], col=cols[2]),
                                 cbind.data.frame(sol[[3]], sub=frac_sub_tot[3], col=cols[3]),
                                 cbind.data.frame(sol[[4]], sub=frac_sub_tot[4], col=cols[4]))

    names(res_fin_2)
    res_fin_2[,1:2] = res_fin_2[,1:2]/3600
    res_fin_2$sub = as.factor(res_fin_2$sub)

    res_all[[ind_pl]] = res_fin_2

    ind_pl = ind_pl+1

  }

}

save.image(file = "data/tmp/fig5.RData")
# load("data/tmp/fig5.RData")
