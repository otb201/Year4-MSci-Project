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
pred_GCE = c(0.05, 0.10, 0.15, 0.20, 0.25, 0, 0.3)#, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.6)
###

A = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
B = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
C = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
D = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
E = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
G = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
H = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)

FA = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
FB = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
FC = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
FD = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
FE = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
FG = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)
FH = matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 1)



#===================
#for loop for fishing mortality rate from 0 - 1 being passed into the predator biomass output result
for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[1]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  A[i] = results$Pred_gm
  print(pred_GCE[1])
  print(fishing_mortality_rate[i])
  print(A[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[2]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  B[i] = results$Pred_gm
  print(pred_GCE[2])
  print(fishing_mortality_rate[i])
  print(B[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[3]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  C[i] = results$Pred_gm
  print(pred_GCE[3])
  print(fishing_mortality_rate[i])
  print(C[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[4]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  D[i] = results$Pred_gm
  print(pred_GCE[4])
  print(fishing_mortality_rate[i])
  print(D[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[5]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  E[i] = results$Pred_gm
  print(pred_GCE[5])
  print(fishing_mortality_rate[i])
  print(E[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[6]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  G[i] = results$Pred_gm
  print(pred_GCE[6])
  print(fishing_mortality_rate[i])
  print(G[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[7]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  H[i] = results$Pred_gm
  print(pred_GCE[7])
  print(fishing_mortality_rate[i])
  print(H[i])
}

pred_bio_output_matrix = rbind(A, B, C, D, E, G, H)
 
print(pred_bio_output_matrix)

#plot full fishing
windowsFonts(A = windowsFont("Times New Roman"))
x.expression = expression("Predator fishing mortality rate / year"^-1)
y.expression = expression(Total~predator~biomass~"/"~gm^-2)
par(mar=c(4,5,1,1))
plot(pred_bio_output_matrix[1, ]~fishing_mortality_rate, type = "l", family = "A", lwd = 2, las = 1, pch = 0, lty = 3, xlab = x.expression, ylab = y.expression, cex.lab = 1.7, cex.axis = 1.5, xaxs = "i", yaxs = "i", xlim = c(0, 1.05), ylim = c(0,40), col = "black")
points(pred_bio_output_matrix[2, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 2)
points(pred_bio_output_matrix[3, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 1)
#points(pred_bio_output_matrix[4, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 4)
#points(pred_bio_output_matrix[5, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 5)
#points(pred_bio_output_matrix[6, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 6)
#points(pred_bio_output_matrix[7, ]~fishing_mortality_rate, type = "l", col = "red", lwd = 1, las = 1, pch = 0, lty = 7)
minor.tick(ny = 5, tick.ratio = 0.5)
legend(cex = 1.5, ncol = 1, title = "Predator GCE", x="topright", inset = 0.01, legend = c("0.05", "0.10", "0.15"), col = c("black", "black", "black"), lty = c(3, 2, 1), lwd = c(2,1,1))
text(x=.03, y=38.5, cex = 1.5, labels = substitute(paste(bold('a)'))))



#===================
for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[1]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FA[i] = results$Fpred_prod
  print(pred_GCE[1])
  print(fishing_mortality_rate[i])
  print(FA[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[2]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FB[i] = results$Fpred_prod
  print(pred_GCE[2])
  print(fishing_mortality_rate[i])
  print(FB[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[3]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FC[i] = results$Fpred_prod
  print(pred_GCE[3])
  print(fishing_mortality_rate[i])
  print(FC[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[4]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FD[i] = results$Fpred_prod
  print(pred_GCE[4])
  print(fishing_mortality_rate[i])
  print(FD[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[5]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FE[i] = results$Fpred_prod
  print(pred_GCE[5])
  print(fishing_mortality_rate[i])
  print(FE[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[6]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FG[i] = results$Fpred_prod
  print(pred_GCE[6])
  print(fishing_mortality_rate[i])
  print(FG[i])
}

for (i in 1: length(fishing_mortality_rate))
{
  params$Fmort_pred = fishing_mortality_rate[i]
  params$K.u = pred_GCE[7]
  results = try(run_model(params, initial.run = T)) #Run model and assign to "initial_res"
  FH[i] = results$Fpred_prod
  print(pred_GCE[7])
  print(fishing_mortality_rate[i])
  print(FH[i])
}

fpred_prod_output_matrix = rbind(FA, FB, FC, FD, FE, FG, FH)

print(fpred_prod_output_matrix)

x.expression = expression("Predator fishing mortality rate / year"^-1)
y.expression = expression(Fisheries~predator~productivity~"/"~g*m^-2*year^-1)
plot(fpred_prod_output_matrix[1, ]~fishing_mortality_rate, main = "A", family = "A", type = "l", lwd = 2, las = 1, pch = 0, lty = 3, xlab = x.expression, ylab = y.expression, cex.lab = 1.7, cex.axis = 1.5, xaxs = "i", yaxs = "i", xlim = c(0, 1.05), ylim = c(0,25), col = "black")

points(fpred_prod_output_matrix[2, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 2)
points(fpred_prod_output_matrix[3, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 1)
#points(fpred_prod_output_matrix[4, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 4)
#points(fpred_prod_output_matrix[5, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 5)
#points(fpred_prod_output_matrix[6, ]~fishing_mortality_rate, type = "l", lwd = 1, las = 1, pch = 0, lty = 6)
#points(fpred_prod_output_matrix[7, ]~fishing_mortality_rate, type = "l", col = "red", lwd = 1, las = 1, pch = 0, lty = 7)
minor.tick(ny = 5, tick.ratio = 0.5)
legend(yjust = 2, cex = 1.5, ncol = 1, title = "Predator GCE", x="bottomleft", inset = 0.01, legend = c("0.05", "0.10", "0.15"), col = c("black", "black", "black"), lty = c(3, 2, 1), lwd = c(2,1,1))
text(x=.03, y=24, cex = 1.5, labels = substitute(paste(bold('b)'))))


plot(log10(output$Mod_pred_biomass_5)[0:N-1]~modeloutput$Body.size[ref:end]
plot(log10(results$Preds)~results$Body.size[1:156], type = "l", xlim = c(-12,4.5), col = "red",  xlab = 'log10 body size', ylab = 'log10 population')
points(log10(results$Preds)~results$Body.size[1:156], type = "l", xlim = c(-12,4.5), col = "red")
#==================
#Plot non-complex size spectra  lines 550 - 610 in sizemodel_functions
plotsizespectrum<-function(modeloutput,params,timeaveraged=F, add.points=F) {
  
  par(mfrow = c(1,1))
  with(params, {  
    
    plot(log10(modeloutput$Preds)[ref:end]~modeloutput$Body.size[ref:end], type = "l", xlim = c(-3.5,4.5), ylim = c(-10,5), col = "red",  xlab = 'log10 body size', ylab = 'log10 population')
    #points(log10(modeloutput$Herbs)[ref.herb:end]~modeloutput$Body.size[ref.herb:end], type = "l", col = "green")
    #points(log10(modeloutput$Invs)[refinv:end]~modeloutput$Body.size[refinv:end], type = "l", col = "brown")
    
    if(add.points==T){
      
      #points(log10(herb_data$G1)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$G2)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$G3)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$GS1)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$GS2)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$GS3)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$GN1)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$GN2)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$GN3)~herb_data$x, col = "green", pch = 1)
      #points(log10(herb_data$M1)~herb_data$x, col = "green", pch = 19)
      #points(log10(herb_data$M2)~herb_data$x, col = "green", pch = 19)
      #points(log10(herb_data$M3)~herb_data$x, col = "green", pch = 19)
      #points(log10(herb_data$M4)~herb_data$x, col = "green", pch = 19)
      
      #points(log10(pred_data$G1)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$G2)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$G3)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$GS1)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$GS2)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$GS3)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$GN1)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$GN2)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$GN3)~pred_data$x, col = "red", pch = 1)
      #points(log10(pred_data$M1)~pred_data$x, col = "red", pch = 19)
      #points(log10(pred_data$M2)~pred_data$x, col = "red", pch = 19)
      #points(log10(pred_data$M3)~pred_data$x, col = "red", pch = 19)
      #points(log10(pred_data$M4)~pred_data$x, col = "red", pch = 19)
      
    }
    
  })
}

plotsizespectrum_full<-function(modeloutput,params,timeaveraged=F) {
  
  par(mfrow = c(1,1))
  #Here is where to add the fish data for comparison
  #herb_data <- read.delim("data/herb_ss_dat.txt", header = TRUE)
  #pred_data <- read.delim("data/pred_ss_dat.txt", header = TRUE)
  
  with(params, {  
    
    plot(log10(modeloutput$Preds)~modeloutput$Body.size, type = "l", xlim = c(-12,4.5), ylim = c(-10,20), col = "red",  xlab = 'log10 body size', ylab = 'log10 population')
    #points(log10(modeloutput$Herbs)[ref.herb:end]~modeloutput$Body.size[ref.herb:end], type = "l", col = "green")
    #points(log10(modeloutput$Invs)[refinv:end]~modeloutput$Body.size[refinv:end], type = "l", col = "brown")
    
    
  })
}

plotsizespectrum(initial.res, params)

plotsizespectrum_full(initial.res, params)
plotsizespectrum(complex_check, params)
#Add invertebrate data to check for fit

legend(0, 15, legend = c("Predator", "Herbivore", "Invertebrate"), col = c("red", "green", "brown"), lty = 1)


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
plot(high_pred_bio_output~high_fishing_mortality_rate, type = "o", pch = 16, cex = 1, lwd = 1, las = 1, xlab = x.expression, ylab = "Total Predator Biomass", cex.lab = 1.3, cex.axis = 1, xaxs = "i", yaxs = "i", xlim = c(0.955, 0.9605), ylim = c(0,40), col = "black")
minor.tick(ny = 5, tick.ratio = 0.5)
legend(x="topright", legend = c("Predator GCE = 0.135"), col = c("black"), lty = 1)

plot(fisheries_pred_prod_output~high_fishing_mortality_rate, type = "o", pch = 16, cex = 1, lwd = 1, las = 1, xlab = x.expression, ylab = "Fisheries predator productivity", cex.lab = 1.3, cex.axis = 1, xaxs = "i", yaxs = "i", xlim = c(0.955, 0.9605), ylim = c(0,40), col = "black")
minor.tick(ny = 5, tick.ratio = 0.5)


