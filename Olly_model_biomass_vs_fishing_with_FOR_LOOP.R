#Source necessary function and parameter files
source('sizemodel_functions.r')
source('params.r')

#Set model parameters using parameter input file
params<-set_param()

#Load data on refuge availability:
#First with branching corals
refuges <- read.delim("high_coral_cover_refuges.txt", header = TRUE)
#...and then without
refuges_nb <- read.delim("high_coral_cover_refuges_no_branches.txt", header = TRUE)

#Load data on invertebrate 
inverts <- read.delim("Kramer_invert_data.txt", header = TRUE)

#==============================
#Initial runs and model checks
#==============================

#Generate model output for default scenario without refuges  
#Set refuge density to zero in all size classes
params$refuge <- rep(0, length(refuges[,5]))


#Plot equilibrium Biomass against Fishing Mortality Rate
#for wk9 meeting - ask whether these two lines can be automated/looped instead of having to write two long vectors
fishing_mortality_rate = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1)
pred_bio_output = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#for loop for fishing mortality rate from 0 - 1 being passed into the predator biomass output result
for (i in 1: length(fishing_mortality_rate))
     {
  params$Fmort_pred = fishing_mortality_rate[i]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  pred_bio_output[i] = results$Pred_gm
  #print(pred_bio_output)
  print(fishing_mortality_rate[i])
  print(pred_bio_output[i])
  }

#write to .csv

#plot full fishing
library("Hmisc")
x.expression = expression("Fishing Mortality Rate / yr"^-1)
plot(pred_bio_output~fishing_mortality_rate, type = "l", lwd = 1, las = 1, xlab = x.expression, ylab = "Total Predator Biomass", cex.lab = 1.3, cex.axis = 1, xaxs = "i", yaxs = "i", xlim = c(0, 1.05), ylim = c(0,40), col = "black")
minor.tick(ny = 5, tick.ratio = 0.5)
legend(x="topright", legend = c("Predator GCE = 0.135"), col = c("black"), lty = 1)

#==============================

high_fishing_mortality_rate = seq(0.955, 0.96, by = 0.005)
high_pred_bio_output = c(0, 0)# 0, 0, 0)#, 0, 0, 0)#, 0, 0, 0)
fisheries_pred_prod_output = c(0, 0)
print(high_fishing_mortality_rate)

#for loop for fishing mortality rate from 0.9 - 1 being passed into the predator biomass output result
for (i in seq(high_fishing_mortality_rate))
{
  params$Fmort_pred = high_fishing_mortality_rate[i]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  high_pred_bio_output[i] = results$Pred_gm
  fisheries_pred_prod_output[i] = results$Fpred_prod
  #print(pred_bio_output)
  print(high_fishing_mortality_rate[i])
  print(high_pred_bio_output[i])
  print(fisheries_pred_prod_output[i])
}

#plot high fishing
x.expression = expression("Fishing Mortality Rate / yr"^-1)
plot(high_pred_bio_output~high_fishing_mortality_rate, type = "o", pch = 16, cex = 1, lwd = 1, las = 1, xlab = x.expression, ylab = "Total Predator Biomass", cex.lab = 1.3, cex.axis = 1, xaxs = "i", yaxs = "i", xlim = c(0.95758, 0.95762), ylim = c(0,40), col = "black")
minor.tick(ny = 5, tick.ratio = 0.5)
legend(x="topright", legend = c("Predator GCE = 0.135"), col = c("black"), lty = 1)

plot(fisheries_pred_prod_output~high_fishing_mortality_rate, type = "o", pch = 16, cex = 1, lwd = 1, las = 1, xlab = x.expression, ylab = "Fisheries predator productivity", cex.lab = 1.3, cex.axis = 1, xaxs = "i", yaxs = "i", xlim = c(0.95758, 0.95762), ylim = c(0,40), col = "black")
minor.tick(ny = 5, tick.ratio = 0.5)

