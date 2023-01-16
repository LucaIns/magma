rm(list=ls())
set.seed(1)

mainwd = "YOUR_MAIN_PATH"
setwd(paste0(mainwd, "/data"))

# iterates across these (takes a huge amount of time due to data size)
subChamber = 0 # use the whole chamber data
# subChamber = 1 # use only upper chamber data
# subChamber = 2 # use only lower chamber data

# aux function: loads an RData file, and returns it
loadRData <- function(fileName){
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# quantiles of interest
my_quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)
# shallow chamber thresholds across simulations
thrsh = c(-3400, -3800, -3570, -3400, -3800)
# fraction of subsampled timestamps
perc_timestamps = 0.3
# fraction of subsampled locations
perc_loc_tot = c(1, 0.6, 0.3, 0.1)

# main simulation parameters (Table 1)
inputParams <- loadRData("inputParameters.RData")
# simulation scenarios
dat_seq=1:5
dat_tot = paste0("compAll", dat_seq, ".RData")
# start plotting histograms of each quantile for each dataset (in a single pdf)
pdf(paste0("subChamber", subChamber, "_preprocessing.pdf"))

#############################
## Data generation
#############################

# proportions of subsamples locations of the upper chamber in each simulation
for (perc_loc_i in perc_loc_tot) {

  # iterate across simulations
  for (dat_i in dat_tot) {

    # rows: composition at a given coordinate
    # cols: increasing time stamps
    dataset_i = loadRData(dat_i)
    coor = row.names(dataset_i)
    dataset_i = as.data.frame(dataset_i)

    # select shallow chamber only
    coor = gsub(paste(c("[(]", "[)]"), collapse = "|"), "", coor)
    coor=unlist(strsplit(coor, ","))
    x_coor=as.numeric(coor[seq(1, length(coor), by=2)])
    y_coor=as.numeric(coor[seq(2, length(coor), by=2)])
    indkeep <- y_coor >  thrsh[which(dat_i==dat_tot)]

    if (subChamber==1) {
      # subsample from upper half chamber only
      indkeep[indkeep] = y_coor[indkeep] >= mean(y_coor[indkeep])
    } else if (subChamber==2) {
      # subsample from lower half chamber only
      indkeep[indkeep] = y_coor[indkeep] < mean(y_coor[indkeep])
    }
    dataset_i=dataset_i[indkeep,]

    # subsample time points
    idxCol = sample(1:ncol(dataset_i), round(ncol(dataset_i)*perc_timestamps), replace = FALSE)
    dataset_i = dataset_i[, idxCol]
    dim(dataset_i)

    # points in the chamber
    idxRow = sample(1:nrow(dataset_i), round(nrow(dataset_i)*perc_loc_i), replace = FALSE)
    dataset_i_tmp = dataset_i[idxRow, ]

    # coordinates retained
    par(mfrow=c(1, 1))
    plot(x_coor[indkeep][idxRow], y_coor[indkeep][idxRow],
         main=paste0("sampled coordinates for simulation: ", which(dat_i==dat_tot),
                     "\nlocations subsampling proportion: ", perc_loc_i))

    # compute quantiles of interest across time
    qi_across_time = matrix(NA, length(my_quantiles), ncol(dataset_i_tmp))
    par(mfrow=c(ceiling(length(my_quantiles)/2), 2))
    for (q_i in 1:length(my_quantiles)) {

      print(q_i)

      qi_across_time[q_i,] = apply(dataset_i_tmp, 2, quantile,
                                   probs=my_quantiles[q_i], na.rm=T)

      tit_qi = paste0("simulation: ", which(dat_i==dat_tot),
                      "\n locations subsampling proportion: ", perc_loc_i,
                      "\n quantile: ", my_quantiles[q_i])
      hist(qi_across_time[q_i,], main=tit_qi)
    }
    matplot(t(qi_across_time), type="l",
            xlab = "time",
            ylab="composition",
            ylim=c(0,1), lwd=2,
            col=1:length(my_quantiles),
            main=paste0("simulation: ", which(dat_i==dat_tot),
                        "\n locations subsampling proportion: ", perc_loc_i))
    legend("right", legend = my_quantiles,col=1:length(my_quantiles),
           cex=0.8,fill=1:length(my_quantiles))

    # responses (quantiles for a given simulation setting)
    Ys = t(qi_across_time)
    Ys = as.data.frame(Ys)
    names(Ys) = paste0("q_", my_quantiles)

    # predictors (for a given simulation setting, including time stamps)
    aspectRatio = inputParams[which(dat_i==dat_tot), 1]
    deltaRho = inputParams[which(dat_i==dat_tot), 2]
    Xs = cbind.data.frame(time = as.numeric(colnames(dataset_i_tmp)),
                          aspectRatio, deltaRho)

    # combine Y's and X's (and simulation setting too)
    Znew = cbind.data.frame(Ys, Xs,
                            simSetting=which(dat_i==dat_tot))

    # merge all simulations
    if (dat_i==dat_tot[1]) {
      dat = Znew
      print(paste0("Generating: ", dat_i))
    } else{
      dat = rbind.data.frame(dat, Znew)
      print(paste0("Appending: ", dat_i))
    }

  }

  if (subChamber==1) {
    # draws from the upper half part of the chamber
    save(dat, file=paste0("quantiles_subLoc", perc_loc_i,"_upper.RData"))
  } else if (subChamber==2) {
    # draws from the lower half part of the chamber
    save(dat, file=paste0("quantiles_subLoc", perc_loc_i,"_lower.RData"))
  } else {
    # draws from the full chamber
    save(dat, file=paste0("quantiles_subLoc", perc_loc_i,".RData"))
  }
}

# turn off pdf generation
dev.off()
par(mfrow=c(1, 1))
