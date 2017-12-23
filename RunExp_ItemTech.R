source("AIbenchmark_analysis.R")
###############
#### Atari ####
###############

## SOURCES
genSources.Atari() # Generate sources
scatterScores.Atari() # Scores Plot (scatter)

## IRT param estimation (ITEMS)
stuff.Atari <- estimateParm.IRT2PL.Atari()
abil.Atari <- stuff.Atari[[1]]
params.Atari <- stuff.Atari[[2]]
Atari.Variability <- stuff.Atari[[3]]
plotICC.most.Atari(params.Atari, abil.Atari, Atari.Variability,10, c(0.15, 0.15)) # Dif/Disc c(0.15, 0.35), Neg c(0.15, 0.15)

# Generality (TECHNIQUES)
ResultsAtari <- generality.ETL.Atari(TRUE, abil.Atari)
plot.GenAb(ResultsAtari, abil.Atari, Atari.Variability, howMany = 10, isAtari = TRUE, LM = FALSE)


##############
#### GVGP ####
##############
source("AIbenchmark_analysis.R")

## SOURCES
genSources.GVGP() # Generate sources
scatterScores.GVGP() # Scores Plot (scatter)

## IRT param estimation (ITEMS)
stuff.GVGP <- estimateParm.IRT2PL.GVGP()
abil.GVGP <- stuff.GVGP[[1]]
params.GVGP <- stuff.GVGP[[2]]
GVGP.Variability <- stuff.GVGP[[3]]
plotICC.most.GVGP(params.GVGP, abil.GVGP, GVGP.Variability,10, c(0.15, 0.35)) # Dif c(0.15, 0.25) - Disc c(0.15, 0.25)

# Generality (TECHNIQUES)
ResultsGVGP <- generality.ETL.GVGP(abil.GVGP)
plot.GenAb(ResultsGVGP, abil.GVGP, GVGP.Variability, howMany = 10, isAtari = FALSE, LM = FALSE)
