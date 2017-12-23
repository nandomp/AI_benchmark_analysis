##############################################################################
#
# This is R code for the following paper:
#
#  - 
#
# This code implements: 
#
# This code has been developed by
#   FERNANDO MARTINEZ-PLUMED, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#   fmartinez@dsic.upv.es
#   JOSE HERNANDEZ-ORALLO, UNIVERSITAT POLITECNICA DE VALENCIA, SPAIN
#   jorallo@dsic.upv.es
#
# LICENCE:
#   GPL
#
# VERSION HISTORY:
#  - V.1.0    17 May 2017. Some functionalities (IRT-2PL parameter estimation).
#  - V.2.0    22 Dec 2017. A lot of improvements (Plot, Generality Analysis, Atari & GVGP ETL, ...)
#
# FUTURE FEATURES:
#
##############################################################################




##############################################################################
############################# CODE ORGANISATION ##############################
#
#
##############################################################################



##############################################################################
######################## LIBRARIES AND I/O OPTIONS ###########################
##############################################################################
options("scipen"=1000000)

require(ggplot2)
require(ltm)
require("SciViews")
require("GGally")
require(corrplot)
require(ggrepel)
require(DT)
library(dplyr)
library(reshape2)
library(ggforce)
library(stringr)
library(jsonlite)
library(gridExtra)

set.seed(288)

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}

# PDFEPS <- 1 # 0 None, 1 PDF, 2 EPS
# PDFheight= 6 # 7 by default, so 14 makes it double higher than wide, 5 makes letters bigger (in proportion) for just one 
# PDFwidth= 9 # 7 by default

Atari <- read.csv("atari_ale2.csv")
progress <- fromJSON("progressAtari.json")
GVGP.results <- read.csv("controllerOutcomeMaster - CLEAN.csv", header= FALSE)


##############################################################################
################################ FUNCTIONS ###################################
##############################################################################


## ATARI ETL: Cleansing Scores for IRT (orig, discretised and normalised scores)
##############################################################################

normalise <- function(x){
  t <- scale(x)
  pnorm(t)
}

ge100 <- function(x){
  ifelse(x>=100, 1,0)
}


genSources.Atari <- function(){
  
  colnames(Atari)[1] <- "Game"
  ## Melted dataframe to plot Scores
  Atari.melt <- melt(Atari)
  Atari.melt$Technique <- str_extract(Atari.melt$variable,"(\\_).*")
  Atari.melt$Technique <- str_replace(Atari.melt$Technique, "\\_","")
  Atari.melt$Technique <- str_replace(Atari.melt$Technique, "\\.\\.","\\.")
  Atari.melt$Author <- str_extract(Atari.melt$variable,"(.*\\_)")
  Atari.melt$Author <- str_replace(Atari.melt$Author, "\\_","")
  colnames(Atari.melt)[2] <- "TechAuthor"
  colnames(Atari.melt)[3] <- "Score"
  saveRDS(Atari.melt,file="Atari.results.melt.rds")
  
  # Original Results
  Atari.clean <- Atari
  Atari.T <- data.frame(t(Atari.clean[,2:ncol(Atari.clean)]))
  colnames(Atari.T) <- Atari[,1] 
  saveRDS(Atari.T, file = "Atari.results.orig.rds" )
  
  # Binarise Results for dicotomic models
  Atari.T.bin <- ge100(Atari.T)
  saveRDS(Atari.T.bin, file = "Atari.results.bin.rds" )
  
  # Normalise results (0,100) for Continuous Response Models
  Atari.norm <- as.data.frame(sapply(Atari.T, normalise))
  rownames(Atari.norm) <- rownames(Atari.T)
  Atari.norm <- Atari.norm * 100
  saveRDS(Atari.norm, file = "Atari.results.norm.rds" )
}


genSources.Atari.EFF <- function(){
  
  Games <- progress[[1]][[5]][[1]][[5]]
  
  ## Melted dataframe to plot
  data <- data.frame(progress[[1]][[5]][[1]][[9]][[1]])
  data$target <- progress[[1]][[5]][[1]][[15]][1]
  for(i in 2:length(progress[[1]][[5]][[1]][[9]])){
    t <- progress[[1]][[5]][[1]][[9]][[i]]
    t$target <- progress[[1]][[5]][[1]][[15]][i]
    data <- rbind(data,t)
  }
  
  extractGame <- function(x){
    x1 <- str_extract(x, '\\(([^\\)]+)\\)')
    x2 <- str_replace(x1, '\\(Atari 2600 ',"")
    str_replace(x2, '\\)',"")
  }
  data$Game <- factor(extractGame(data$metric))
  data$humanRelative <- (data$value/data$target)*100
  
  data.agg <- group_by(data, Game)
  data.sum <- summarise(data.agg, maxScore = max(humanRelative))
  
  for(i in 1:nrow(data)){
    data$humanRelMax[i] <- as.numeric(filter(data.sum, Game == data$Game[i])[1,"maxScore"])
  }
  saveRDS(data, file = "Atari.EFF.results.melt.rds")
  
  
  # Original Results 
  Atari.EFF <- data[,c("Game","label","humanRelative")]
  t <- melt(Atari.EFF, id.vars = c("Game", "label"))
  Atari.EFF <- dcast(t, Game ~ label + variable)

  row.names(Atari.EFF) <- Atari.EFF$Game
  Atari.EFF <- Atari.EFF[,2:ncol(Atari.EFF)]
  colnames(Atari.EFF) <- str_replace(str_extract(colnames(Atari.EFF), "(.*\\_)"), "(\\_)", "")
  
  # Complete cases
  na <- sapply(Atari.EFF, function(x) sum(is.na(x)))
  nas <- as.data.frame(na)
  
  Atari.EFF <- Atari.EFF[,!colnames(Atari.EFF)%in%rownames(nas)[which(nas$na>=50)]]
  Atari.EFF <- Atari.EFF[complete.cases(Atari.EFF),]
  
  Atari.EFF.T <- data.frame(t(Atari.EFF))
  saveRDS(Atari.EFF.T, file = "Atari.EFF.results.orig.rds")
  
  
  # Binarise Results for dicotomic models
  Atari.EFF.T.bin <- ge100(Atari.EFF.T)
  saveRDS(Atari.EFF.T.bin, file = "Atari.EFF.results.bin.rds")
  
  # Normalise results (0,100) for Continuous Response Models
  Atari.EFF.norm <- as.data.frame(sapply(Atari.EFF.T, normalise))
  rownames(Atari.EFF.norm) <- rownames(Atari.EFF.T)
  Atari.EFF.norm <- Atari.EFF.norm * 100
  saveRDS(Atari.EFF.norm, file = "Atari.EFF.results.norm.rds" )
}



## GVGP ETL: Cleansing Scores for IRT (orig, discretised and normalised scores)
##############################################################################

genSources.GVGP <- function(Dis = 50){
  
  ################
  # DERIVES SIZES AND NAMES OF AGENTS, GAMES, LEVELS, ETC.
  ################
  
  Nresults <- nrow(GVGP.results)
  # 28175
  # 23 controllers x 49 games x 25 trials (5 times per level x 5 repetitions)
  
  colnames(GVGP.results) <- c("Agent", "Game", "Level", "Win/Loss", "Score", "Seconds (0 - 2000)")
  head(GVGP.results)
  
  # summary(GVGP.results)
  # plot(GVGP.results[,"Level"], GVGP.results[,"Win/Loss"])
  # cor(GVGP.results[,"Level"], GVGP.results[,"Win/Loss"])
  # 
  # plot(GVGP.results[,"Level"], GVGP.results[,"Score"])
  # cor(GVGP.results[,"Level"], GVGP.results[,"Score"])
  # 
  # plot(GVGP.results[,"Score"], GVGP.results[,"Win/Loss"])
  # cor(GVGP.results[,"Score"], GVGP.results[,"Win/Loss"])
  
  PERFORMANCE_METRIC <- "Score"
  NORMALISE_MATRIX <- TRUE  # If true it scales the matrix so that we get variances and means that are tantamount to the correlations
  
  agentnames <- unique(GVGP.results[["Agent"]])
  Nagents <- length(agentnames)
  gamenames <- unique(GVGP.results[["Game"]])
  Ngames <- length(gamenames)
  levelnames <- unique(GVGP.results[["Level"]])
  Nlevels <- length(levelnames)
  repnames <- 1:5
  Nreps <- length(repnames)
  
  gamelevelnames <- rep(0,Ngames*Nlevels)
  count <- 1
  for (i in 1:Ngames) {
    for (j in 1:Nlevels) {
      gamelevelnames[count] <- paste(gamenames[i], levelnames[j], sep=":")
      count <- count +1
    }  
  }
  gamelevelnames
  Ngamelevels <- length(gamelevelnames)
  
  
  gamelevelrepnames <- rep(0,Ngames*Nlevels*Nreps)
  count <- 1
  for (i in 1:Ngames) {
    for (j in 1:Nlevels) {
      for (k in 1:Nreps) {
        gamelevelrepnames[count] <- paste(gamenames[i], levelnames[j], repnames[k], sep=":")
        count <- count +1
      }  
    }  
  }
  gamelevelrepnames
  Ngamelevelreps <- length(gamelevelrepnames)
  
  ################
  # AGGREGATES REPETITIONS
  ################
  
  Nresultsagg <- Nresults / Nreps
  resultsagg <- GVGP.results[1:Nresultsagg,]
  count <- 1
  winsum <- 0
  for (i in 1:Nresults) {
    winsum <- winsum + GVGP.results[i, PERFORMANCE_METRIC]
    #scoresum <- scoresum + results[i, "Score"]
    #secondssum <- seconddsum + results[i, "Seconds (0 - 2000)"] 
    if (i %% Nreps == 0) {
      resultsagg[count,] <- GVGP.results[i,]
      resultsagg[count, PERFORMANCE_METRIC] <- winsum/Nreps
      winsum <- 0
      count <- count + 1
    }  
  }

  # Should be:
  #resultmatrix <- matrix(results[[PERFORMANCE_METRIC]], nrow=Nagents, ncol=Ngamelevelreps, byrow=TRUE,                        dimnames = list(agentnames,gamelevelrepnames))
  # Columns: tasks  : 49 games x 5 level x 5 level= 245*5 = 1225
  # Rows: agents  : 23 controllers

  # Should be:
  resultmatrixagg <- matrix(resultsagg[[PERFORMANCE_METRIC]], nrow=Nagents, ncol=Ngamelevels, byrow=TRUE,
                            dimnames = list(agentnames,gamelevelnames))
  
  #resultmatrixagg
  AImethods <- rownames(resultmatrixagg)
  # Columns: tasks  : 49 games x 5 levels = 245
  # Rows: agents  : 23 controllers

  # # We include some very small noise to avoid NAN correlation
  # noise <- 0.0000001
  # resultmatrixaggN <- resultmatrixagg + runif(length(resultmatrixagg),-noise,+noise)
  # 
  # 
  # data <- resultmatrixaggN
  
  GVGP <- data.frame(resultmatrixagg)
  saveRDS(GVGP, file= "GVGP.results.orig.rds")
 
  normaliseB <- function(x){
    t <- scale(x)
  }
  
  GVGP.norm <- data.frame(sapply(GVGP,normaliseB))
  rownames(GVGP.norm) <- AImethods
  saveRDS(GVGP.norm, file= "GVGP.results.norm.rds")
  
  geDis <- function(x){
      ifelse(x>=median(x), 1,0)
  }
  
  GVGP.bin <- sapply(GVGP, geDis)
  saveRDS(GVGP.bin, file= "GVGP.results.bin.rds")
  
  
  
  ## Melted dataframe to plot Scores
  GVGP$Technique <- rownames(GVGP)
  GVGP.melt <- melt(GVGP)
  colnames(GVGP.melt) <- c("Technique","Game","Score")
  saveRDS(GVGP.melt,file="GVGP.results.melt.rds")
  
  ## Melted dataframe to plot Scores
  GVGP.norm$Technique <- rownames(GVGP.norm)
  GVGP.scaled.melt <- melt(GVGP.norm)
  colnames(GVGP.scaled.melt) <- c("Technique","Game","Score")
  saveRDS(GVGP.scaled.melt,file="GVGP.results.scaled.melt.rds")

}


## IRT  Dicotomic (ATARI)
##############################################################################

getCorrelations.Atari <- function(clean = TRUE){
  
  Atari <- readRDS("Atari.results.orig.rds")
  
  if(clean){
    Atari.clean <- Atari[!rownames(Atari) %in% c("Schaul_DQN.baseline", 
                                                 "vanHasselt.Humanstarts_DQN", "vanHasselt.Humanstarts_Double.DQN..tuned.", 
                                                 "Wang.Humanstarts_DUEL", "Wang.Humanstarts_PRIOR."),]
  }
  # GAMES  0.3318
  corTechniques <- cor(Atari.clean[,2:ncol(Atari.clean)])
  cT <- mean(corTechniques[lower.tri(corTechniques)])
  # TECHS  0.287
  Atari.T <- t(Atari.clean[,2:ncol(Atari.clean)])
  colnames(Atari.T) <- Atari.clean[,1] 
  corGames <- cor(Atari.T)
  cG <- mean(corGames[lower.tri(corGames)])
  
  print(paste("Correlation between Games: ",cT," - Techniques: ", cG, sep =""))
  
}

estimateParm.IRT2PL.Atari <- function(clean = TRUE){
  Atari.T.bin <- readRDS("Atari.results.bin.rds")
  if(clean){
    Atari.T.bin <- Atari.T.bin[!rownames(Atari.T.bin) %in% c("Schaul_DQN.baseline", 
                                                       "vanHasselt.Humanstarts_DQN", "vanHasselt.Humanstarts_Double.DQN..tuned.", 
                                                       "Wang.Humanstarts_DUEL", "Wang.Humanstarts_PRIOR."),]
  }
  Atari.T.bin.var <- Atari.T.bin[,apply(Atari.T.bin, 2, var, na.rm=TRUE) != 0]#remove games with variability = 0
  fit <- tpm(Atari.T.bin.var, type = "latent.trait",  IRT.param=TRUE, constraint = cbind(1:ncol(Atari.T.bin.var), 1, 0))
  r = factor.scores(fit,resp.patterns=Atari.T.bin.var)
  abil = r$score.dat$z1
  params <- coef(fit)
  params <- cbind(params, id = 1:ncol(Atari.T.bin.var))
  #plot(fit)
  return(list(abil,params, Atari.T.bin.var))
}


## IRT  Dicotomic (GVGP)
##############################################################################

estimateParm.IRT2PL.GVGP <- function(){
  
  GVGP.bin <- readRDS("GVGP.results.bin.rds")
  GVGP.bin.var <- GVGP.bin[,apply(GVGP.bin, 2, var, na.rm=TRUE) != 0]#remove games with variability = 0
  GVGP.bin.var <- data.frame(GVGP.bin.var)
  #rownames(GVGP.bin.var) <- rownames(GVGP)
  
  # IRT 2PL
  fit <- tpm(GVGP.bin.var, type = "latent.trait",  IRT.param=TRUE, constraint = cbind(1:ncol(GVGP.bin.var), 1, 0))
  r = factor.scores(fit,resp.patterns=GVGP.bin.var)
  abil = r$score.dat$z1
  params <- coef(fit)
  params <- cbind(params, id = 1:nrow(params))
  
  return(list(abil,params, GVGP.bin.var))
  
}






## Generality Analysis (ATARI)
##############################################################################

myvar <- function(x) {
  n <- length(x)
  var(x)*(n-1)/(n)  # Population variance
}

unevenness1 <- function(v,x) {
  v / (x*(1-x))
}

unevenness2 <- function(m) {
  v <- myvar(m)
  x <- mean(m)
  v / (x*(1-x))
}

var.Inv <- function(x){
  myvar(x)^(-1)
}

normalise2 <- function(array, x=0,y=1){
  range <- max(array) - min(array)
  array <- (array - min(array)) / range
  range2 <- y - x
  (array*range2) + x
}


generality.ETL.Atari <- function(clean = TRUE, abil ){
  # Original SCORES
  Atari <- readRDS("Atari.results.norm.rds")
  if(clean){
    Atari.clean <- Atari[!rownames(Atari) %in% c("Schaul_DQN.baseline", 
                                                 "vanHasselt.Humanstarts_DQN", "vanHasselt.Humanstarts_Double.DQN..tuned.", 
                                                 "Wang.Humanstarts_DUEL", "Wang.Humanstarts_PRIOR."),]
  }
  
  Atari <- data.frame(t(Atari.clean/100))
  ResultsAtari <- data.frame(t(sapply(Atari,range)))
  colnames(ResultsAtari)<-c("Min","Max")
  ResultsAtari$MeanScore <- sapply(Atari,mean)
  ResultsAtari$Var <-  sapply(Atari,var)
  ResultsAtari$myvar <-  sapply(Atari,myvar)
  ResultsAtari$unevenness <-  sapply(Atari,unevenness2)
  ResultsAtari$var.Inv <-  sapply(Atari,var.Inv)
  ResultsAtari$abil <- abil
  ResultsAtari$AItechnique <- rownames(ResultsAtari)
  ResultsAtari$Approach <- 
    c("SARSA", "DQN", "DDQN", "DUEL", 
      "DQN", "DUEL", "DQN","DQN",
      "DQN","Gorila", "DQN", "Gorila",
      "Q-learning","Q-learning","Q-learning","Q-learning",
      "DQN", "Q-learning", "DQN", "DQN",
      "Q-learning",  "Q-learning","DQN", "DDQN",
      "DDQN","DDQN", "DDQN", "DQN",
      "DDQN", "DUEL", "DQN", "DUEL",
      "DDQN", "DUEL", "SARSA", "SARSA",
      "SARSA", "SARSA","SARSA", "DDQN"
    )
  
  return(ResultsAtari)
}

generality.ETL.GVGP <- function(abil){
  # Scaled SCORES
  GVGP.norm <- readRDS("GVGP.results.norm.rds")
 
  GVGP.pnorm <- data.frame(sapply(GVGP.norm,pnorm))
  rownames(GVGP.pnorm) <- rownames(GVGP.norm)
  GVGP.pnorm.T <- data.frame(t(GVGP.pnorm))
  
  # Scaled SCORES
  ResultsGVGP <- data.frame(t(sapply(GVGP.pnorm.T,range)))
  colnames(ResultsGVGP)<-c("Min","Max")
  
  ResultsGVGP$MeanScore <- sapply(GVGP.pnorm.T,mean)
  ResultsGVGP$Var <-  sapply(GVGP.pnorm.T,var)
  ResultsGVGP$myvar <-  sapply(GVGP.pnorm.T,myvar)
  ResultsGVGP$unevenness <-  sapply(GVGP.pnorm.T,unevenness2)
  ResultsGVGP$var.Inv <-  sapply(GVGP.pnorm.T,var.Inv)
  ResultsGVGP$abil <- abil
  ResultsGVGP$AItechnique <- rownames(ResultsGVGP)
  
  # plot(ResultsGVGP$MeanScore, ResultsGVGP$myvar)
  # lm(ResultsGVGP$myvar ~ ResultsGVGP$MeanScore)
  
  return(ResultsGVGP)
}


## Plot Correlations (ATARI)
##############################################################################

plotCorrelations.Atari <- function (coeff = 0.95){
  
  Atari <- readRDS("Atari.results.orig.rds")
  Atari.T <- data.frame(t(Atari))
  M <- cor(Atari.T)
  p1 <- ggcorr(Atari.T,  hjust = 1, size = 2.5, label = TRUE, label_size = 2.5)
  
  p2 <- ggcorr(Atari.T, geom = "blank", label = TRUE, hjust = 1,size = 2.5, label_size = 2.5) +
    geom_point(size = 10, aes(color = coefficient > 0, alpha = abs(coefficient) >= 0.95)) +
    scale_alpha_manual(values = c("TRUE" = 0.25, "FALSE" = 0)) +
    guides(color = FALSE, alpha = FALSE)
  
  openPDFEPS("Atari.Corr", height= 12, width= 12)
  plot(p1)
  dev.off()
  
  openPDFEPS(paste("Atari.Scores",coeff, sep="."), height= 12, width= 12)
  plot(p2)
  dev.off()
  
}


## Plot Scatter Scores (ATARI)
##############################################################################

scatterScores.Atari <- function(){
  
  Atari.melt <- readRDS("Atari.results.melt.rds")
  
  pg <- ggplot(Atari.melt, aes(reorder(Game, Score), Score)) +
    geom_point(aes(colour = Technique), size = 2, alpha = 0.3) + 
    geom_hline(aes(yintercept =100), colour="#990000", linetype="dotted", size = 0.5) +
    annotate("text", x = "Gravitar", y = 120, label = "Above Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    annotate("text", x = "Gravitar", y = 80, label = "Below Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    #geom_hline(aes(yintercept =(target/target)*100), colour="#990000", linetype="dashed", size = 1) +
    #annotate("text", x = "Skiing", y = 600, label = "Above Human level (100%)", color="black", size=3 , angle=0, fontface="bold") +
    #coord_flip() + 
    theme_light() + 
    xlab("Game") + ylab("Score % (Human normalised)") + facet_zoom(xy = Score >-100 & Atari.melt$Score<1000,horizontal = T) +
    theme(legend.position = c(0.1, 0.7),
          legend.text = element_text(size=7),
          legend.title = element_text(size=8),
          legend.key.size =  unit(0.09, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
    scale_color_discrete("AI Technique") 
  
  # pb <- ggplot_build(plot1)
  # pb$data[[3]][1, 'alpha'] <- 0
  # pb$data[[4]][1, 'alpha'] <- 0
  # pg <- ggplot_gtable(pb)
  
  openPDFEPS("Atari.Scores", height= 4.5, width= 14)
  plot(pg)
  dev.off()
}


scatterScores.Atari.EFF <- function(){
  
  data <- readRDS("Atari.EFF.results.melt.rds")
  
  pg <- ggplot(data, aes(reorder(Game, humanRelMax), humanRelative)) +
    geom_point(aes(colour = label, group = label), size = 3) + 
    geom_hline(aes(yintercept =(target/target)*100), colour="#990000", linetype="dashed", size = 1) +
    annotate("text", x = "Skiing", y = 600, label = "Above Human level (100%)", color="black", size=3 , angle=0, fontface="bold") +
    coord_flip() + theme_bw() + ylab("Score (%)") + xlab("Game") +
    scale_y_continuous(labels = c("-1500%", "-1000%", "-500%", "0%", "500%", "1000%", "1500%", "2000%","2500%", "3000%","3500%", "4000%","4500%"),
                       breaks = c(-1500, -1000, -500, 0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500)) + 
    theme(axis.text.x = element_text(angle = 0, hjust = 1)) + theme(legend.position = c(0.85, 0.3)) + scale_color_discrete("AI Technique")
  
  openPDFEPS("Atari.EFF.Scores", height= 8, width= 15)
  plot(pg)
  dev.off()
}


## Plot Scatter Scores (GVGP)
##############################################################################

scatterScores.GVGP <- function(){
  
  GVGP.melt <- readRDS("GVGP.results.scaled.melt.rds")
  gameLabels <- str_replace(levels(GVGP.melt$Game),"(.*)\\.([1-4])","")
  gameLabels <- str_replace(gameLabels,"\\.([0])","")

  
  
  pg <- ggplot(GVGP.melt, aes(reorder(Game, Score), Score)) +
    geom_point(aes(colour = Technique), size = 2, alpha = 0.3) + 
    #geom_hline(aes(yintercept =100), colour="#990000", linetype="dotted", size = 0.5) +
    #annotate("text", x = "Gravitar", y = 120, label = "Above Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    #annotate("text", x = "Gravitar", y = 80, label = "Below Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    theme_light() + 
    xlab("Game") + ylab("Score % (Scaled)") + facet_zoom(xy = Score >-1 & Score<0.1,horizontal = T) +
    theme(legend.position = c(0.1, 0.8),
          legend.text = element_text(size=6),
          legend.title = element_text(size=7),
          legend.key.size =  unit(0.07, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
    scale_color_discrete("AI Technique") +
    scale_x_discrete(labels = gameLabels) 
  
  # pb <- ggplot_build(plot1)
  # pb$data[[3]][1, 'alpha'] <- 0
  # pb$data[[4]][1, 'alpha'] <- 0
  # pg <- ggplot_gtable(pb)
  
  openPDFEPS("GVGP.Scores", height= 4.5, width= 14)
  plot(pg)
  dev.off()
}



## Plot ICCs "MOST" (ATARI)
##############################################################################

plotICC.most.Atari <- function(params,abil, Atari.T.bin.var, howMany = 6, legpos = c(0.15, 0.25)) {
  
  params <- data.frame(params)
  
  mostDiff <- params[order(-params$Dffclt),"id"][1:howMany]
  mostDisc <- params[order(-params$Dscrmn),"id"][1:howMany]
  negDisc <- which(params$Dscrmn<0)
  
  
  
  dif <-plotICC.n(mostDiff, Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos) #legend.position = c(0.15, 0.25)
  disc <- plotICC.n(mostDisc, Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos) #legend.position = c(0.15, 0.25)
  neg <- plotICC.n(negDisc, Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos)
  all <- plotICC.n(1:nrow(params), Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos)
  

  openPDFEPS("Atari.Diff", height= 6, width= 9)
  plot(dif)
  dev.off()
  
  openPDFEPS("Atari.Disc", height= 6, width= 9)
  plot(disc)
  dev.off()
  
  openPDFEPS("Atari.neg", height= 6, width= 9)
  plot(neg)
  dev.off()
  
  openPDFEPS("Atari.all", height= 6, width= 9)
  plot(all)
  dev.off()
}



## Plot ICCs "MOST" (GVGP)
##############################################################################

plotICC.most.GVGP <- function(params, abil, GVGP.bin.var, howMany = 6, legpos = c(0.15, 0.25)){
  
  params <- data.frame(params)
  
  mostDiff <- params[order(-params$Dffclt),"id"][1:howMany]
  mostDisc <- params[order(-params$Dscrmn),"id"][1:howMany]
  negDisc <- which(params$Dscrmn<0)
  
  dif <-plotICC.n(mostDiff, GVGP.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos) #legend.position = c(0.15, 0.25)
  disc <- plotICC.n(mostDisc, GVGP.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos) #legend.position = c(0.15, 0.25)
  neg <- plotICC.n(negDisc, GVGP.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos)
 
  all <- plotICC.n(1:nrow(params), GVGP.bin.var, params, abil)
  
  openPDFEPS("GVGP.Diff", height= 6, width= 9)
  plot(dif)
  dev.off()
  
  openPDFEPS("GVGP.Disc", height= 6, width= 9)
  plot(disc)
  dev.off()
  
  openPDFEPS("GVGP.neg", height= 6, width= 9)
  plot(neg)
  dev.off()
  
  openPDFEPS("GVGP.all", height= 6, width= 9)
  plot(all)
  dev.off()
}


## Plot Multiple ICCs
##############################################################################

plotICC.n <- function (ind_game, resp, params, abil, max4Legend = 6, legend.pos = c(0.15, 0.25)){
  
  Probability <- c()
  Ability <- seq(-6,6,0.05)
  df_param <- data.frame(item = NA, Ability = NA, Probability = NA)
  df_resp <- data.frame(item = NA, Ability = NA, resp = NA)
  
  for(item in ind_game){
    print(item)
    Probability <- c()
    for (theta in Ability){
      a = params[item,3]
      b = params[item,2]
      c = 0
      #
      y_temp <- c + (1-c)/(1+exp(-a*(theta-b)))
      #y_temp <- (exp(a*(theta-b)))/(1+exp(a*(theta-b)))
      Probability <- c(Probability,y_temp)
    }
    t <- data.frame(item = rep(rownames(params)[item], length(Probability)), Ability = Ability, Probability = Probability)
    df_param <- rbind(df_param,t)
    t2 <- data.frame(item = rep(rownames(params)[item], length(abil)), Ability = abil,resp =resp[,item])
    df_resp <- rbind(df_resp,t2)
  }
  df_param <- df_param[2:nrow(df_param),]
  df_resp <- df_resp[2:nrow(df_resp),]
  
  df_param$item <- factor(df_param$item)
  class(df_param$Ability)
  
  df_resp$resp <-  ifelse(df_resp$resp == 1, df_resp$resp + 0.05, df_resp$resp - 0.05)
  
  plot <- ggplot(df_param, aes(Ability, Probability, colour = item))
  
  if(length(ind_game)<=max4Legend){
    plot <- plot + geom_line(aes(linetype = item), size=1.2)
  }else{
    plot <- plot + geom_line(size=1.2)
    legend.pos <- "None"
  }
    plot <- plot + geom_point(data = df_resp, aes(x = Ability, resp, colour =item), size = 4, alpha = 0.3) +  
    theme_light() + 
    theme(legend.position = legend.pos,#c(0.15, 0.25)
          legend.text = element_text(size=9),#9
          legend.title = element_text(size=10),#10
          legend.key.size =  unit(0.2, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    scale_color_discrete("Item")  + scale_linetype(guide=FALSE) + 
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))
  
  return(plot)
}

plotICC.n2 <- function (ind_game, resp, params, abil){
  
  Probability <- c()
  Ability <- seq(-6,6,0.05)
  df_param <- data.frame(item = NA, Ability = NA, Probability = NA)
  df_resp <- data.frame(item = NA, Ability = NA, resp = NA)
  
  for(item in ind_game){
    print(item)
    Probability <- c()
    for (theta in Ability){
      a = params[item,3]
      b = params[item,2]
      c = 0
      #
      y_temp <- c + (1-c)/(1+exp(-a*(theta-b)))
      #y_temp <- (exp(a*(theta-b)))/(1+exp(a*(theta-b)))
      Probability <- c(Probability,y_temp)
    }
    t <- data.frame(item = rep(rownames(params)[item], length(Probability)), Ability = Ability, Probability = Probability)
    df_param <- rbind(df_param,t)
    t2 <- data.frame(item = rep(rownames(params)[item], length(abil)), Ability = abil,resp =resp[,item])
    df_resp <- rbind(df_resp,t2)
  }
  df_param <- df_param[2:nrow(df_param),]
  df_resp <- df_resp[2:nrow(df_resp),]
  
  df_param$item <- factor(df_param$item)
  class(df_param$Ability)
  
  df_resp$resp <-  ifelse(df_resp$resp == 1, df_resp$resp + 0.05, df_resp$resp - 0.05)
  
  plot <- ggplot(df_param, aes(Ability, Probability, colour = item)) +
    #geom_line(aes(linetype = item), size=1.2) + 
    geom_line(size=1.2) + 
    geom_point(data = df_resp, aes(x = Ability, resp, colour =item), size = 4, alpha = 0.3) +  #0.05 all
    #geom_point(data = df_resp, aes(Ability, as.numeric(resp))+
    #geom_text_repel(data = df2, aes(abil, resp, label=row.names(df2)),colour = "black") +
    #geom_abline(slope = a*(1-c)/4, intercept = (2*(1+c)-a*(1-c)*b )/4, colour="red", linetype = "dotted",size=0.4) +
    #geom_vline(xintercept = b, col="green", linetype = "dotted",size=0.4) +
    #geom_hline(yintercept = c, col="gray", linetype = "dotted",size=0.4) +
    #ggtitle(colnames(resp)[ind_game]) +
    theme_light() + 
    theme(legend.position = "None",#c(0.15, 0.25)
          legend.text = element_text(size=9),#9
          legend.title = element_text(size=10),#10
          legend.key.size =  unit(0.2, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    scale_color_discrete("Atari Game")  + scale_linetype(guide=FALSE) + 
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))
  
  return(plot)
}

plotICC <- function (ind_game, resp, params, abil){
  Probability <- c()
  Ability <- seq(-6,6,0.05)
  for (theta in Ability){
    a = params[ind_game,3]
    b = params[ind_game,2]
    c = 0
    #
    y_temp <- c + (1-c)/(1+exp(-a*(theta-b)))
    #y_temp <- (exp(a*(theta-b)))/(1+exp(a*(theta-b)))
    Probability <- c(Probability,y_temp)
  }
  
  df <- data.frame(Ability, Probability)
  df2 <- data.frame(abil,resp =resp[,ind_game])
  
  plot <- ggplot(df, aes(Ability, Probability)) +
    geom_line(size=0.8) + 
    geom_point(data = df2, aes(abil, resp)) + 
    #geom_text_repel(data = df2, aes(abil, resp, label=row.names(df2)),colour = "black") +
    geom_abline(slope = a*(1-c)/4, intercept = (2*(1+c)-a*(1-c)*b )/4, colour="red", linetype = "dotted",size=0.4) +
    geom_vline(xintercept = b, col="green", linetype = "dotted",size=0.4) +
    geom_hline(yintercept = c, col="gray", linetype = "dotted",size=0.4) +
    ggtitle(colnames(resp)[ind_game]) +
    theme_bw()
  
  return(plot)
}


## Plot Generality (abil & generality)
##############################################################################


#Results = ResultsAtari; data.var = Atari.Variability
plot.GenAb <- function(Results, abil, data.var, howMany = 6, isAtari = TRUE, LM = FALSE){
  
  # ABILITY PLOT
  abil.df <- data.frame(AITechnique = rownames(data.var), abil = abil)
  range(abil.df$abil) #-2.148880  2.224539
  
  plotAb <- ggplot(Results, aes(reorder(AItechnique, abil), abil, colour = abil)) + 
    geom_point(size=5) +
    scale_colour_gradient2() +
    theme_light() + 
    theme(legend.position = "None",#c(0.15, 0.25)
          legend.text = element_text(size=7),#9
          legend.title = element_text(size=8),#10
          legend.key.size =  unit(0.1, "in"),
          axis.text=element_text(size=9),
          #axis.text.x = element_text(angle = 45, hjust = 1),margin = margin(0.2, unit = "cm"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 80, hjust = 0),
          axis.title.x = ) +
    #legend.direction = "vertical", 
    ylab("Ability") + xlab("") + scale_x_discrete(position = "top")  + ylim(c(-2.5,3))
  
  # GENERALITY PLOT
  
  Bernoulli <- data.frame(x=seq(0, 1, 0.01))
  Bernoulli$y <- Bernoulli$x * (1 - Bernoulli$x)
  
  mostAble <- Results[order(-Results$abil),][1:howMany,]
  lessAble <- Results[order(Results$abil),][1:howMany,]
  
  # MeanScore VS myvar
  plotGen <- ggplot(Bernoulli, aes(x,y)) + geom_line(colour = "gray48", size = 1, linetype="dashed")
  
  if(isAtari){
    plotGen <- plotGen + geom_point(data = Results, aes(MeanScore, myvar, shape = Approach, colour= abil), size = 6)
  }else{
    plotGen <- plotGen + geom_point(data = Results, aes(MeanScore, myvar, colour= abil), size = 6)
  }
    plotGen <- plotGen + geom_text_repel(data = mostAble, aes(MeanScore, myvar, label = AItechnique), 
                    point.padding = 0.3,
                    box.padding = 0.5,
                    nudge_x = 45,
                    size = 3,
                    segment.alpha = 0.2) +
      geom_text_repel(data = lessAble, aes(MeanScore, myvar, label = AItechnique), 
                    point.padding = 0.3,
                    box.padding = 0.5,
                    nudge_x = -45,
                    size = 3,
                    segment.alpha = 0.2) +
      theme_classic() + ylab("Variance") + xlab("Score") + #ggtitle("MeanScore VS myvar") + 
      scale_shape_manual(values = c(15, 16, 17, 18, 11,8)) + scale_colour_gradient2() +
      theme_light() + 
      theme(legend.position = c(0.9, 0.75),#c(0.15, 0.25)
          legend.text = element_text(size=8),#9
          legend.title = element_text(size=9),#10
          legend.key.size =  unit(0.18, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
      #legend.direction = "vertical", 
      xlim(c(0,1))
    
    if(LM){
      linMod <- lm(Results$myvar ~ Results$MeanScore)
      plotGen <- plotGen + geom_abline(intercept =linMod[[1]][1], slope = linMod[[1]][2], colour = "darkred", size = 1.2, linetype ="dashed") +
        annotate("text", x= 0.5, y = 0.01, label= paste("y =",linMod[[1]][2],"x +", linMod[[1]][1] ,sep=""), colour= "darkred", size = 5)
    }
  
    
    Data <-  ifelse(isAtari, "Atari", "GVGP")
 
    openPDFEPS(paste(Data,"Ability",sep="."), height = 4, width = 7)
    plot(plotAb)
    dev.off()
    
    
    openPDFEPS(paste(Data,"Generality",sep="."), height = 7, width = 8)
    plot(plotGen)
    dev.off()
    
    g <- grid.arrange(plotAb, plotGen, layout_matrix = rbind(c(1),c(2,2),c(2,2)))
    openPDFEPS(paste(Data,"AbGen",sep="."), height = 10, width = 8)
    plot(g)
    dev.off()
}


















