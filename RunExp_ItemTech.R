source("AIbenchmark_analysis.R")
##################################################################################################
############################################## ALE ###############################################
##################################################################################################

## Generate Sources
genSources.Atari() # Generate sources
scatterScores.Atari() # Scores Plot (scatter)

## IRT param estimation (ITEMS)
stuff.Atari <- estimateParm.IRT2PL.Atari(mirt = F)
abil.Atari <- stuff.Atari[[1]]
params.Atari <- stuff.Atari[[2]]
Atari.Variability <- stuff.Atari[[3]]
plotICC.most(params.Atari, abil.Atari, Atari.Variability,  howMany = 3, "top", filename="Atari") #c(0.8, 0.8) Dif/Disc c(0.15, 0.35), Neg c(0.15, 0.15)
plotICC.summ(params.Atari, abil.Atari, Atari.Variability, c(0.93, 0.8), filename= "Atari.IRT.summary", howMany = 3)

# Regularity (Techniques)
ResultsAtari <- generality.ETL.Atari(TRUE, abil.Atari)
plot.GenAb(ResultsAtari, abil.Atari, Atari.Variability, howMany = 10, isAtari = TRUE, LM = F)

## Teorical Scores
# Results <- readRDS("Atari.results.norm.rds")
# Results <- Results/100
# Results <- Results[,colnames(Results) %in% colnames(Atari.Variability)]
# Results.var <- Results[rownames(Results) %in% rownames(Atari.Variability),]

# Plot Theoretical ACCs
# All 
plot.ACC.Teo(abil.Atari, params.Atari, Atari.Variability,minDiff = -6, maxDiff = 6, filename= "Atari.ACC.teo.all", variance = F)
# Filtered
filter.Atari.Rnd <- rownames(Atari.Variability)[sample(nrow(Atari.Variability),5)] #Random selection
filter.Atari <- c("Talvitie_B.PRO", "Wang.noops_DQN", "Nair.noop_Gorila.humnorm", "vanHasselt.noops_Double.DQN", "Gruslys_REACTOR.M1","Schaul_DDQN.rank.based")
plot.ACC.Teo(abil.Atari, params.Atari, Atari.Variability, minDiff = -6, maxDiff = 6, 
             filename= "Atari.ACC.teo.filter.var", filter = filter.Atari, variance = T, legpos = "top", cols = 2, medianDisc = T)

## Plot Empirical ACCs
# All
Atari.cuts <-  plot.ACC.Empirical(abil.Atari, params.Atari, Atari.Variability, Atari.Variability,
                   cuts = c(-11,-1, 0, 1, 10), minDiff = -6, maxDiff = 6, filename= "Atari.ACC.emp.cont.all", cont = T,variance = TRUE, legpos = "top")
# Filter
Atari.cuts.filter <- plot.ACC.Empirical(abil.Atari, params.Atari, Atari.Variability, Atari.Variability, cuts = c(-11,-1, 0, 1, 10), minDiff = -6, maxDiff = 6, 
                   filename= "Atari.ACC.emp.cont.filter.var", cont = T, filterTechs = filter.Atari, variance = TRUE,legpos = "top", cols = 2)#c(0.85,0.85)
# Windowed
# plot.ACC.Emp.window(abil.Atari, params.Atari, Atari.Variability, minDiff = -3, maxDiff = 5, 
#                     filename= "Atari.ACC.emp.window.all", window = 5)
# plot.ACC.Emp.window(abil.Atari, params.Atari, Atari.Variability, minDiff = -3, maxDiff = 5, 
#                     filename= "Atari.ACC.emp.window.filter.var", window = 5, filterTechs = filter.Atari, variance = T)

# Scatter plots (Slope/Generality vs. Regularity)
genEmpACC(Atari.cuts, ResultsAtari, abil.Atari, filename="Atari.GenSlope",filter.Atari)

# Ranges
params.Atari <- data.frame(params.Atari)
sapply(filter(params.Atari, Dffclt > -900),range)
# Gussng     Dffclt     Dscrmn id
# [1,]      0 -10.516049 -0.6350841  1
# [2,]      0   8.221943 58.2687017 41

# Correlations
cor(ResultsAtari$MeanScore, ResultsAtari$abil)
cor(ResultsAtari$var.Inv, ResultsAtari$abil)

# IRT Params to markdown
library(knitr)
kable(params.Atari, format = "markdown", padding = 0)

##################################################################################################
############################################# GVGAI ##############################################
##################################################################################################
source("AIbenchmark_analysis.R")

## Generate Sources
genSources.GVGP(Score = F) # Generate sources
scatterScoresZoom.GVGP() # Scores Plot (scatter)

## IRT param estimation (ITEMS)

stuff.GVGP <- estimateParm.IRT2PL.GVGP(mirt =F)
abil.GVGP <- stuff.GVGP[[1]]
params.GVGP <- stuff.GVGP[[2]]
GVGP.Variability <- stuff.GVGP[[3]]
plotICC.most(params.GVGP, abil.GVGP, GVGP.Variability, howMany = 3, "top", filename="GVGP") # c(0.3, 0.25) Dif c(0.15, 0.25) - Disc c(0.15, 0.25)
plotICC.summ(params.GVGP, abil.GVGP, GVGP.Variability, c(0.9, 0.8), filename= "GVGAI.IRT.summary")

# Regularity (Techniques)
ResultsGVGP <- generality.ETL.GVGP(abil.GVGP)
plot.GenAb(ResultsGVGP, abil.GVGP, GVGP.Variability, howMany = 14, isAtari = FALSE, LM = F,  group = F)

## Teorical Scores
# Results.var.GVGAI <- readRDS("GVGP.results.norm.rds")
# Results.var.GVGAI2 <- sapply(Results.var.GVGAI, pnorm)
# rownames(Results.var.GVGAI2) <- rownames(Results.var.GVGAI)
# Results.var.GVGAI2 <- Results.var.GVGAI2[,colnames(Results.var.GVGAI2) %in% colnames(GVGP.Variability)]
# Results.var.GVGAI <- Results.var.GVGAI2[rownames(Results.var.GVGAI2) %in% rownames(GVGP.Variability),]


## Plot Empirical ACCs
# All
plot.ACC.Teo(abil.GVGP, params.GVGP, GVGP.Variability, minDiff = -6, maxDiff =6, filename= "GVGAI.ACC.teo.all")
#Filter
filter.GVGP.rnd <- rownames(GVGP.Variability)[sample(nrow(GVGP.Variability),5)]
filter.GVGP <- c("jaydee", "NovTea","mrtndwrd","simulatedAnnealing", "sampleMCTS", "AIJim")# Alg. selection
plot.ACC.Teo(abil.GVGP, params.GVGP, GVGP.Variability, minDiff = -6, maxDiff =6, filename= "GVGAI.ACC.teo.filter.var",
             filter = filter.GVGP, variance = T, legpos = "top", cols = 2, medianDisc = T)

## Plot Empirical ACCs
# All
GVGP.cuts <-  plot.ACC.Empirical(abil.GVGP, params.GVGP, GVGP.Variability, GVGP.Variability, minDiff = -6, maxDiff = 6, 
                   cuts= c(-600,-1,1,2, 600), filename= "GVGAI.ACC.emp.cont", cont = T, legpos = "right")
# Filtered (cont)
plot.ACC.Empirical(abil.GVGP, params.GVGP, GVGP.Variability, GVGP.Variability,  minDiff = -6, maxDiff = 6,  cuts= c(-600,-1,1,2, 600), 
                   filename= "GVGAI.ACC.emp.cont.filt.var", cont = T, filterTechs = filter.GVGP, variance = T, legpos = "top", cols = 2)# legpos = c(0.15,0.1)

#plot.ACC.Empirical(abil.GVGP, params.GVGP, GVGP.Variability, GVGP.Variability, minDiff = -6, maxDiff = 6,  groups = 4, 
#                   filename= "GVGAI.ACC.emp.cont.filt.var", cont = T, filterTechs = filter.GVGP, variance = T, legpos = "none", cols = 2)# legpos = c(0.15,0.1)


# Windowed (cont)
# plot.ACC.Emp.window(abil.GVGP, params.GVGP, GVGP.Variability, minDiff = -3, maxDiff = 0.1, filename= "GVGAI.ACC.emp.window.all", window = 40)
# plot.ACC.Emp.window(abil.GVGP, params.GVGP, GVGP.Variability, minDiff = -3, maxDiff = 0.1, 
#                     filename= "GVGAI.ACC.emp.window.filter", window = 40, filterTechs = filter.GVGP, variance = F)
# plot.ACC.Emp.window(abil.GVGP, params.GVGP, GVGP.Variability, minDiff = -3, maxDiff = 0.1, 
#                     filename= "GVGAI.ACC.emp.window.filter.var", window = 40, filterTechs = filter.GVGP, variance = T)

genEmpACC(GVGP.cuts, ResultsGVGP, abil.GVGP, filename = "GVGAI.GenSlope", filter.GVGP, ylimit=30)


#Ranges
params.GVGP <- data.frame(params.GVGP)
sapply(params.GVGP,range)
# Gussng    Dffclt     Dscrmn  id
# [1,]      0 -913.3886 -0.4260713   1
# [2,]      0   38.1645 35.5587588 208
sapply(filter(params.GVGP, Dffclt > -900),range)
# Gussng    Dffclt     Dscrmn  id
# [1,]      0 -64.68033 -0.4260713   1
# [2,]      0  38.16450 35.5587588 208

# Correlations
cor(ResultsGVGP$MeanScore, ResultsGVGP$abil)
cor(ResultsGVGP$var.Inv, ResultsGVGP$abil)

# IRT Params to markdown
library(knitr)
kable(params.GVGP, format = "markdown", padding = 0)
