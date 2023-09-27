#Source necessary function and parameter files
source('sizemodel_functions.r')
source('params.r')
library("Hmisc")

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
fishing_mortality_rate = c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 1)
pred_bio_output = c(0.0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#start for loop here

params$Fmort_pred = 0.0 #fishing_mortality_rate[1]
results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
pred_bio_output[1] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[2]
results <- try(run_model(params, initial.run = T))
pred_bio_output[2] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[3]
results <- try(run_model(params, initial.run = T))
pred_bio_output[3] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[4]
results <- try(run_model(params, initial.run = T))
pred_bio_output[4] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[5]
results <- try(run_model(params, initial.run = T))
pred_bio_output[5] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[6]
results <- try(run_model(params, initial.run = T))
pred_bio_output[6] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[7]
results <- try(run_model(params, initial.run = T))
pred_bio_output[7] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[8]
results <- try(run_model(params, initial.run = T))
pred_bio_output[8] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[9]
results <- try(run_model(params, initial.run = T))
pred_bio_output[9] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[10]
results <- try(run_model(params, initial.run = T))
pred_bio_output[10] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[11]
results <- try(run_model(params, initial.run = T))
pred_bio_output[11] = results$Pred_gm

params$Fmort_pred = fishing_mortality_rate[12]
results <- try(run_model(params, initial.run = T))
pred_bio_output[12] = results$Pred_gm

#params$Fmort_pred = fishing_mortality_rate[13]
#results <- try(run_model(params, initial.run = T))
#pred_bio_output[13] = results$Pred_gm

#=================================================================================

#write to .csv
#write.csv()

#plot
x.expression = expression("Fishing Mortality Rate / yr"^-1)
plot(pred_bio_output~fishing_mortality_rate, type = "l", lwd = 1, las = 1, xlab = x.expression, ylab = "Total Predator Biomass", cex.lab = 1.3, cex.axis = 1, xaxs = "i", yaxs = "i", xlim = c(0, 1.05), ylim = c(0,40), col = "black")
minor.tick(ny = 5, tick.ratio = 0.5)
legend(x="topright", legend = c("Predator GCE = 0.135"), col = c("black"), lty = 1)

#potentially look at and plot herb bio

