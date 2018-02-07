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
#  - V.2.1    25 Jan 2018. Regularity, Generality, slope functions.
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
options( java.parameters = "-Xmx6g" )

.lib<- c("ggplot2", "ltm", "SciViews", "GGally", "corrplot", "ggrepel", "DT", "plyr", "dplyr", "reshape2",
         "ggforce", "stringr", "jsonlite", "gridExtra", "mirt", "Hmisc")

.inst <- .lib %in% installed.packages()
if (length(.lib[!.inst])>0) install.packages(.lib[!.inst], repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com")) 
lapply(.lib, require, character.only=TRUE)

source("color_palette.R")
set.seed(288)

# Read Data
Atari <- read.csv("atari_ale2.csv")
progress <- fromJSON("progressAtari.json")
GVGP.results <- read.csv("controllerOutcomeMaster - CLEAN.csv", header= FALSE)


##############################################################################
################################ FUNCTIONS ###################################
##############################################################################

## PLOTS TO PDF
##############################################################################

openPDFEPS <- function(file, height= PDFheight, width= PDFwidth, PDFEPS = 1) {
  if (PDFEPS == 1) {
    pdf(paste(file, ".pdf", sep=""), width, height)
  } else if (PDFEPS == 2) {
    postscript(paste(file, ".eps", sep=""), width, height, horizontal=FALSE)
  }
}

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

genSources.GVGP <- function(Score = T){ # Score == F -> Win/Loss value
  
  colnames(GVGP.results) <- c("Agent", "Game", "Level", "Win/Loss", "Score", "Seconds (0 - 2000)")
  
  GVGP.results$GameB <- paste(GVGP.results$Game,GVGP.results$Level,sep=".")
  GVGP.results.g <- group_by(GVGP.results, Agent, GameB)
  
  GVGP.results.s <- summarise(GVGP.results.g, ScoreM = mean(Score))
  GVGP.results.WL <- summarise(GVGP.results.g, ScoreM = sum(`Win/Loss`))
  
  t <- melt(GVGP.results.s, id.vars = c("Agent", "GameB"))
  GVGP <- data.frame(dcast(t, Agent ~ GameB))
  tWL <- melt(GVGP.results.WL, id.vars = c("Agent", "GameB"))
  GVGP.WL <- data.frame(dcast(tWL, Agent ~ GameB))
  
  rownames(GVGP) <- GVGP[,1] 
  GVGP <- GVGP[,2:ncol(GVGP)]
  rownames(GVGP.WL) <- GVGP.WL[,1] 
  GVGP.WL <- GVGP.WL[,2:ncol(GVGP.WL)]
  
  if(Score){
    saveRDS(GVGP, file= "GVGP.results.orig.rds")
  }else{
    saveRDS(GVGP.WL, file= "GVGP.results.orig.rds")
  }
  
  normaliseS <- function(x){
    scale(x)
  }

  if(Score){
    GVGP.norm <- data.frame(sapply(GVGP,normaliseS))
    rownames(GVGP.norm) <- rownames(GVGP)
    saveRDS(GVGP.norm, file= "GVGP.results.norm.rds")
  }else{
   GVGP.norm <- data.frame(sapply(GVGP.WL,normaliseS))
   rownames(GVGP.norm) <- rownames(GVGP.WL)
   bad <- sapply(GVGP.norm, function(x) all(is.nan(x)))
   saveRDS(GVGP.norm[,!bad], file= "GVGP.results.norm.rds")
  }
  
  geDis <- function(x){
      ifelse(x>=median(x), 1,0)
  }
  geDisWL <- function(x){
    ifelse(x>=2.5, 1,0)
  }
  
  if(Score){
    GVGP.bin <- data.frame(sapply(GVGP, geDis))
    rownames(GVGP.bin) <- rownames(GVGP)
    saveRDS(GVGP.bin, file= "GVGP.results.bin.rds")
  }else{
    GVGP.bin <- data.frame(sapply(GVGP.WL, geDisWL))
    rownames(GVGP.bin) <- rownames(GVGP.WL)
    saveRDS(GVGP.bin, file= "GVGP.results.bin.rds")
  }
  
  if(Score){
    ## Melted dataframe to plot Scores
    GVGP$Technique <- rownames(GVGP)
    GVGP.melt <- melt(GVGP)
    colnames(GVGP.melt) <- c("Technique","Game","Score")
    saveRDS(GVGP.melt,file="GVGP.results.melt.rds")
  }else{
    ## Melted dataframe to plot Scores
    GVGP.WL$Technique <- rownames(GVGP.WL)
    GVGP.WL.melt <- melt(GVGP.WL)
    colnames(GVGP.WL.melt) <- c("Technique","Game","Score")
    GVGP.WL.melt$Score <- GVGP.WL.melt$Score/5
    saveRDS(GVGP.WL.melt,file="GVGP.results.melt.rds")
  }
 
  
  # ## Melted dataframe to plot Scores
  # GVGP.norm$Technique <- rownames(GVGP.norm)
  # GVGP.scaled.melt <- melt(GVGP.norm)
  # colnames(GVGP.scaled.melt) <- c("Technique","Game","Score")
  # saveRDS(GVGP.scaled.melt,file="GVGP.results.scaled.melt.rds")

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

estimateParm.IRT2PL.Atari <- function(clean = TRUE, mirt = F){
  Atari.T.bin <- readRDS("Atari.results.bin.rds")
  if(clean){
    Atari.T.bin <- Atari.T.bin[!rownames(Atari.T.bin) %in% c("Schaul_DQN.baseline", 
                                                       "vanHasselt.Humanstarts_DQN", "vanHasselt.Humanstarts_Double.DQN..tuned.", 
                                                       "Wang.Humanstarts_DUEL", "Wang.Humanstarts_PRIOR."),]
  }
  
  Atari.T.bin.var <- Atari.T.bin[,apply(Atari.T.bin, 2, var, na.rm=TRUE) != 0]#remove games with variability = 0
  
  if(!mirt){#LTM
    fit <- tpm(Atari.T.bin.var, type = "latent.trait",  IRT.param=TRUE, constraint = cbind(1:ncol(Atari.T.bin.var), 1, 0))
    r = factor.scores(fit,resp.patterns=Atari.T.bin.var)
    abil = r$score.dat$z1
    params <- coef(fit)
    params <- cbind(params, id = 1:ncol(Atari.T.bin.var))
    #plot(fit)
    return(list(abil,params, Atari.T.bin.var))
    
  }else{#MIRT
    print("-MIRT-")
    fit <- mirt(Atari.T.bin.var,1,itemtype = '2PL', technical = list(NCYCLES = 5000), optimizer = 'NR')
    temp = coef(fit, simplify = T, IRTpars =T)$items
    params <- data.frame(temp[,c("g","b","a")])
    colnames(params)<-c("Gussng","Dffclt","Dscrmn")
    params$id <- 1:nrow(params)
      
    ## computing the abilities 'ab_vector' of the respondents   
      
    abil<-t(fscores(fit))
    abil <- as.vector(abil)
    
    return(list(abil, params, Atari.T.bin.var))
    }
  }

## IRT  Dicotomic (GVGP)
##############################################################################

estimateParm.IRT2PL.GVGP <- function(mirt = FALSE){
  
  GVGP.bin <- readRDS("GVGP.results.bin.rds")
  GVGP.bin.var <- GVGP.bin[,apply(GVGP.bin, 2, var, na.rm=TRUE) != 0]#remove games with variability = 0
  GVGP.bin.var <- data.frame(GVGP.bin.var)
  rownames(GVGP.bin.var) <- rownames(GVGP.bin)
  
  # IRT 2PL
  if(!mirt){#LTM
    fit <- tpm(GVGP.bin.var, type = "latent.trait",  IRT.param=TRUE, constraint = cbind(1:ncol(GVGP.bin.var), 1, 0))
    r = factor.scores(fit,resp.patterns=GVGP.bin.var)
    abil = r$score.dat$z1
    params <- coef(fit)
    params <- cbind(params, id = 1:nrow(params))
    
    return(list(abil,params, GVGP.bin.var))
  }else{
    print("-MIRT-")
    fit <- mirt(GVGP.bin.var,1,itemtype = '2PL', technical = list(NCYCLES = 5000), optimizer = 'BFGS')
    temp = coef(fit, simplify = T, IRTpars =T)$items
    params <- data.frame(temp[,c("g","b","a")])
    colnames(params)<-c("Gussng","Dffclt","Dscrmn")
    params$id <- 1:nrow(params)
    
    ## computing the abilities 'ab_vector' of the respondents   
    
    abil<-t(fscores(fit))
    abil <- as.vector(abil)
    
    return(list(abil, params, GVGP.bin.var))
  }
  
}

## Generality Analysis (ATARI & GVGP)
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
  GVGP.pnorm.T <- GVGP.pnorm.T[complete.cases(GVGP.pnorm.T),]
  
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
  ResultsGVGP$Approach <- 
    c("MCTS", "MCTS", "Search", "Search",
      "Search","GA","Search","Search",
      "GA","GA","MCTS","MCTS",
      "Iterative Width", "LA", "MCTS", "GA",
      "MCTS","LA","Random","Search",
      "MCTS", "Minimax","GA")
  # ResultsGVGP$Approach <- 
  #   c("MCTS", "MCTS", "A*", "BFS",
  #     "DFS","GA","HC","ID",
  #     "GA","GA","MCTS","MCTS",
  #     "Iterative Width", "LA", "MCTS", "GA",
  #     "MCTS","LA","R","SA",
  #     "MCTS", "Minimax","GA")
  
  # plot(ResultsGVGP$MeanScore, ResultsGVGP$myvar)
  # lm(ResultsGVGP$myvar ~ ResultsGVGP$MeanScore)
  ResultsGVGP$Approach <- factor(ResultsGVGP$Approach) 
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
  
  openPDFEPS("Atari.Scores", height= 4, width= 14)
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
  GVGP.melt <- GVGP.melt[complete.cases(GVGP.melt),]
  gameLab <- str_replace(GVGP.melt$Game,"(.*)\\.([1-4])","")
  gameLab <- str_replace(gameLab,"\\.([0])","")
  #GVGP.melt$gameLabels <- gameLab
  
  GVGP.melt.2 <- group_by(GVGP.melt, Game)
  GVGP.melt.3 <- summarise(GVGP.melt.2, max = median(Score))

  GVGP.melt$max <- rep(GVGP.melt.3$max, each = (nrow(GVGP.melt)/nrow(GVGP.melt.3)))
  #pg <- ggplot(GVGP.melt, aes(reorder(Game, Score), Score)) +
  pg <- ggplot(GVGP.melt, aes(Game, Score)) +
    geom_point(aes(colour = Technique), size = 1, alpha = 0.3) + 
    #geom_hline(aes(yintercept =100), colour="#990000", linetype="dotted", size = 0.5) +
    #annotate("text", x = "Gravitar", y = 120, label = "Above Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    #annotate("text", x = "Gravitar", y = 80, label = "Below Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    theme_light() + 
    xlab("Game") + ylab("Score % (Scaled)") + 
    #facet_zoom(xy = Score >=-1 & Score <= 1, horizontal = T) +
    theme(legend.position = c(0.82, 0.1),
          legend.text = element_text(size=6),
          legend.title = element_text(size=7),
          legend.key.size =  unit(0.07, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
    scale_color_discrete("AI Technique") +
    scale_x_discrete(labels = gameLab) + guides(colour=guide_legend(ncol=6))
    #scale_x_discrete(labels = GVGP.melt[order(GVGP.melt$gameLabels, GVGP.melt$max),])
  
  # pb <- ggplot_build(plot1)
  # pb$data[[3]][1, 'alpha'] <- 0
  # pb$data[[4]][1, 'alpha'] <- 0
  # pg <- ggplot_gtable(pb)
  
  openPDFEPS("GVGP.Scores", height= 4.5, width= 14)
  plot(pg)
  dev.off()
}

scatterScoresZoom.GVGP <- function(){
  
  GVGP.melt <- readRDS("GVGP.results.melt.rds")
  GVGP.melt <- GVGP.melt[complete.cases(GVGP.melt),]
  gameLab <- str_replace(levels(GVGP.melt$Game),"(.*)\\.([1-4])","")
  gameLab <- str_replace(gameLab,"\\.([0])","")
  #GVGP.melt$gameLabels <- gameLab
  
  GVGP.melt.2 <- group_by(GVGP.melt, Game)
  GVGP.melt.3 <- summarise(GVGP.melt.2, median = median(Score))
  
  #GVGP.melt$max <- rep(GVGP.melt.3$max, each = (nrow(GVGP.melt)/nrow(GVGP.melt.3)))
  #pg <- ggplot(GVGP.melt, aes(reorder(Game, Score), Score)) +
  pg <- ggplot(GVGP.melt, aes(Game, Score)) +
    geom_point(aes(colour = Technique), size = 1, alpha = 0.3) + 
    geom_point(data = GVGP.melt.3, aes(x = Game, y =median), colour="#990000", size = 1, shape = 3) +

    #annotate("text", x = "Gravitar", y = 120, label = "Above Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    #annotate("text", x = "Gravitar", y = 80, label = "Below Human level", color="#990000", size=2 , angle=0, fontface="bold") +
    theme_light() + 
    xlab("Game") + ylab("Score % (Scaled)") + 
    facet_zoom(xy = Score >=-1 & Score <= 2, horizontal = T) +
    theme(legend.position = c(0.25, 0.90),
          legend.text = element_text(size=7),
          legend.title = element_text(size=8),
          legend.key.size =  unit(0.08, "in"),
          axis.text=element_text(size=7),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
    scale_color_discrete("AI Technique") +
    scale_x_discrete(labels = gameLab) + guides(colour=guide_legend(ncol=8))
  #scale_x_discrete(labels = GVGP.melt[order(GVGP.melt$gameLabels, GVGP.melt$max),])
  
  # pb <- ggplot_build(plot1)
  # pb$data[[3]][1, 'alpha'] <- 0
  # pb$data[[4]][1, 'alpha'] <- 0
  # pg <- ggplot_gtable(pb)
  
  openPDFEPS("GVGP.Scores", height= 4, width= 14)
  plot(pg)
  dev.off()
}


## Plot ICCs "MOST" ("howMany" most Diff, Disc, Neg, All ICCs)
##############################################################################
# abil <- abil.Atari 
# params <- params.Atari 
# Atari.T.bin.var <- Atari.Variability 
# abil <- abil.GVGP 
# params <- params.GVGP 
# params <-  filter(data.frame(params), Dffclt >-100)
# GVGP.bin.var <- GVGP.Variability

plotICC.most <- function(params,abil, results, howMany = 5, legpos = c(0.15, 0.25), filename="") {
  
  params <- data.frame(params)
  params.filter <- filter(params, Dscrmn >=0)
  
  byDffclt <- ddply(params.filter, c('Dffclt'))
  rownames(byDffclt) <- byDffclt$id
  byDscrmn <- ddply(params.filter, c('Dscrmn'))
  rownames(byDscrmn) <- byDscrmn$id
  
  
  # mostDiff <- params.filter[order(-params.filter$Dffclt),"id"][1:howMany]
  # mostDisc <- params[order(-params$Dscrmn),"id"][1:howMany]
   negDisc <- which(params$Dscrmn<0)
  # lessDiff <- params.filter[order(params.filter$Dffclt),"id"][1:howMany]
  # lessDisc <-  params[order(params$Dscrmn),"id"][1:howMany]
  
  mostDiff <- tail(byDffclt,howMany)$id
  lessDiff <- head(byDffclt,howMany)$id
  mostDisc <- tail(byDscrmn,howMany)$id
  lessDisc <- head(byDscrmn,howMany)$id
  
  dif <-plotICC.n2(c(mostDiff,lessDiff), results, params, abil, max4Legend = howMany*2, 
                   legend.pos = legpos, sortBy = "Dffclt", cols = 2) #legend.position = c(0.15, 0.25)
  disc <- plotICC.n2(c(mostDisc,lessDisc), results, params, abil, max4Legend = howMany*2, 
                     legend.pos = legpos, sortBy = "Dscrmn", cols = 2) #legend.position = c(0.15, 0.25)
  neg <- plotICC.n2(negDisc, results, params, abil, max4Legend = length(negDisc), 
                    legend.pos = legpos, sortBy = "Dscrmn", cols = 2, neg = T)
  
  #dif <-plotICC.n(c(mostDiff,lessDiff), Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos) #legend.position = c(0.15, 0.25)
  #disc <- plotICC.n(c(mostDisc,lessDisc), Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos) #legend.position = c(0.15, 0.25)
  #neg <- plotICC.n(negDisc, Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos)
  # all <- plotICC.n2(1:nrow(params), Atari.T.bin.var, params, abil, max4Legend = howMany, legend.pos = legpos)
  

  openPDFEPS(paste(filename,".Diff",sep=""), height= 7, width= 6)
  plot(dif)
  dev.off()
  
  openPDFEPS(paste(filename,".Disc",sep=""), height= 7, width= 6)
  plot(disc)
  dev.off()
  
  openPDFEPS(paste(filename,".neg",sep=""), height= 7, width= 6)
  plot(neg)
  dev.off()
  
  # openPDFEPS("Atari.all", height= 6, width= 9)
  # plot(all)
  # dev.off()
}

plotICC.summ <- function(params,abil, results, legpos = c(0.15, 0.25), howMany = 1, filename = "") {
  
  params <- data.frame(params)
  params.filter <- filter(params, Dscrmn >=0)
  
  byDffclt <- ddply(params.filter, c('Dffclt'))
  rownames(byDffclt) <- byDffclt$id
  byDscrmn <- ddply(params, c('Dscrmn'))
  rownames(byDscrmn) <- byDscrmn$id

  negDisc <- which(params$Dscrmn<0)
  mostDiff <- tail(byDffclt,howMany)$id
  mostDisc <- tail(byDscrmn,howMany)$id

  all <- plotICC.All(1:nrow(params), negDisc, mostDiff, mostDisc,  results, params, abil, legend.pos = legpos)
  
  
  openPDFEPS(filename, height= 7, width= 10)
  plot(all)
  dev.off()
}


## Plot Multiple ICCs
##############################################################################
# params =params.Atari
# abil = abil.Atari
# resp = Atari.Variability
# max4Legend = 10
# legend.pos = c(0.15, 0.15)
# ind_game = 1:nrow(params)
# ind_gameNeg = negDisc
# ind_gameDiff = mostDiff
# ind_gameDisc = mostDisc

plotICC.All <- function (ind_game, ind_gameNeg, ind_gameDiff, ind_gameDisc, 
                         resp, params, abil, max4Legend = 6, legend.pos = c(0.15, 0.25), maxDiff = 6, minDiff = -6, 
                       sortBy="Dffclt", cols = 1){
  
  Probability <- c()
  Ability <- seq(minDiff,maxDiff,0.05)
  df_param <- data.frame(item = NA, Ability = NA, Probability = NA, itemID = NA, itemDiff = NA, itemDisc = NA)
  df_resp <- data.frame(item = NA, Ability = NA, resp = NA, itemID = NA)
  
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
    t <- data.frame(item = rep(rownames(params)[item], length(Probability)), Ability = Ability, 
                    Probability = Probability, itemID = rep(item, length(Probability)),
                    itemDiff = rep(b, length(Probability)), itemDisc = rep(a, length(Probability)))
    df_param <- rbind(df_param,t)
    
    t2 <- data.frame(item = rep(rownames(params)[item], length(abil)), Ability = abil,resp =resp[,item], itemID = rep(item, length(abil)))
    df_resp <- rbind(df_resp,t2)
  }
  df_param <- df_param[2:nrow(df_param),]
  df_resp <- df_resp[2:nrow(df_resp),]
  
  df_param$item <- factor(df_param$item)
  #df_param$itemComplete <- paste(df_param$item," [",round(params[item,2],2),", ",round(params[item,3],2),"]", sep="")
  for (i in 1:nrow(df_param)){
    df_param$itemComplete[i] <- paste(df_param$item[i]," [",round(params[df_param$itemID[i],2],2),", ",round(params[df_param$itemID[i],3],2),"]", sep="")
  }
  print(unique(df_param$itemComplete))
  #class(df_param$Ability)
  
  df_resp$resp <-  ifelse(df_resp$resp == 1, df_resp$resp + 0.05, df_resp$resp - 0.05)
  #df_resp$itemComplete <- paste(df_resp$item," [",round(params[item,2],2),", ",round(params[item,3],2),"]", sep="")
  for (i in 1:nrow(df_resp)){
    df_resp$itemComplete[i] <- paste(df_resp$item[i]," [",round(params[df_resp$itemID[i],2],2),", ",round(params[df_resp$itemID[i],3],2),"]", sep="")
  }
  
  # Plot
  df_param$itemComplete  <- with(df_param, reorder(itemComplete, itemDisc))

  
  plot <- ggplot(df_param, aes(Ability, Probability, group = itemComplete)) + 
    geom_line(size=1.2, alpha = 0.05) + #geom_point(data = df_resp, aes(x = Ability, resp), size = 4, alpha = 0.05) +  
    theme_light() 
  #neg
  plot <- plot + geom_line(data=filter(df_param, itemID  %in%ind_gameNeg), aes(Ability, Probability, colour = "Dscrmn < 0") , size = 1.5, alpha = 0.8) +
    geom_point(data = filter(df_resp, itemID %in% ind_gameNeg), aes(x = Ability, resp), size = 4, alpha = 0.1, colour = "red4")  

  #diff
  plot <- plot + geom_line(data=filter(df_param, itemID  %in% ind_gameDiff), aes(Ability, Probability, colour = "Most Dffclt"), size = 1.5, alpha = 0.8) +
    geom_point(data = filter(df_resp, itemID  %in% ind_gameDiff), aes(x = Ability, resp), size = 4, alpha = 0.5, colour = "deepskyblue")
  #disc
  plot <- plot + geom_line(data=filter(df_param, itemID  %in% ind_gameDisc), aes(Ability, Probability, colour = "Most Dscrmn"), size = 1.5, alpha = 0.8) +
    geom_point(data = filter(df_resp, itemID  %in% ind_gameDisc), aes(x = Ability, resp), size = 4, alpha = 0.5, colour = "mediumseagreen")
  
  
  
  plot <- plot +  theme(legend.position = legend.pos,#c(0.15, 0.25)
          legend.text = element_text(size=11),#9
          legend.title = element_text(size=12),#10
          #legend.key.size =  unit(0.2, "in"),
          axis.text=element_text(size=12),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    #scale_color_discrete("Item [Dfclt, Dscrmn]") + 
    #scale_colour_manual(values = ICCcolour4(n = length(unique(df_param$itemComplete))))+ #scale_linetype(guide=FALSE) +  
    #  guides(colour=guide_legend(ncol=cols)) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_colour_manual("",values = c("firebrick1","deepskyblue","mediumseagreen"))
  return(plot)
}


plotICC.n <- function (ind_game, resp, params, abil, max4Legend = 6, legend.pos = c(0.15, 0.25), 
                         maxDiff = 6, minDiff = -6, sortBy="Dffclt", cols = 1){
  
  Probability <- c()
  Ability <- seq(minDiff,maxDiff,0.05)
  df_param <- data.frame(item = NA, Ability = NA, Probability = NA, itemID = NA, itemDiff = NA, itemDisc = NA)
  df_resp <- data.frame(item = NA, Ability = NA, resp = NA, itemID = NA)
  
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
    t <- data.frame(item = rep(rownames(params)[item], length(Probability)), Ability = Ability, 
                    Probability = Probability, itemID = rep(item, length(Probability)),
                    itemDiff = rep(b, length(Probability)), itemDisc = rep(a, length(Probability)))
    df_param <- rbind(df_param,t)
    
    t2 <- data.frame(item = rep(rownames(params)[item], length(abil)), Ability = abil,resp =resp[,item], itemID = rep(item, length(abil)))
    df_resp <- rbind(df_resp,t2)
  }
  df_param <- df_param[2:nrow(df_param),]
  df_resp <- df_resp[2:nrow(df_resp),]
  
  df_param$item <- factor(df_param$item)
  #df_param$itemComplete <- paste(df_param$item," [",round(params[item,2],2),", ",round(params[item,3],2),"]", sep="")
  for (i in 1:nrow(df_param)){
    df_param$itemComplete[i] <- paste(df_param$item[i]," [",round(params[df_param$itemID[i],2],2),", ",round(params[df_param$itemID[i],3],2),"]", sep="")
  }
  print(unique(df_param$itemComplete))
  #class(df_param$Ability)
  
  df_resp$resp <-  ifelse(df_resp$resp == 1, df_resp$resp + 0.05, df_resp$resp - 0.05)
  #df_resp$itemComplete <- paste(df_resp$item," [",round(params[item,2],2),", ",round(params[item,3],2),"]", sep="")
  for (i in 1:nrow(df_resp)){
    df_resp$itemComplete[i] <- paste(df_resp$item[i]," [",round(params[df_resp$itemID[i],2],2),", ",round(params[df_resp$itemID[i],3],2),"]", sep="")
  }
  
  # Plot
  if (sortBy == "Dffclt"){
    df_param$itemComplete  <- with(df_param, reorder(itemComplete, itemDiff))
    
  }else{
    if(sortBy == "Dscrmn"){
      df_param$itemComplete  <- with(df_param, reorder(itemComplete, itemDisc))
    }else{
      print("No specific order")
    }
  }
  
  plot <- ggplot(df_param, aes(Ability, Probability, colour = itemComplete))
  if(length(ind_game)<=max4Legend){
    plot <- plot + geom_line(aes(linetype = itemComplete), size=1.2)
  }else{
    plot <- plot + geom_line(size=1.2)
    legend.pos <- "None"
  }
  plot <- plot + geom_point(data = df_resp, aes(x = Ability, resp, colour = itemComplete), size = 4, alpha = 0.3) +  
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
    #scale_color_discrete("Item [Dfclt, Dscrmn]") + 
    scale_colour_manual(values = ICCcolour4(n = length(unique(df_param$itemComplete))))+ #scale_linetype(guide=FALSE) +  
    guides(colour=guide_legend(ncol=cols)) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))
  
  return(plot)
}

plotICC.n2 <- function (ind_game, resp, params, abil, max4Legend = 6, legend.pos = c(0.15, 0.25), 
                       maxDiff = 6, minDiff = -6, sortBy="Dffclt", cols = 1, neg = FALSE){
  
  Probability <- c()
  Ability <- seq(minDiff,maxDiff,0.05)
  df_param <- data.frame(item = NA, Ability = NA, Probability = NA, itemID = NA, itemDiff = NA, itemDisc = NA)
  df_resp <- data.frame(item = NA, Ability = NA, resp = NA, itemID = NA)
  
  for(item in 1:nrow(params)){
    
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
    t <- data.frame(item = rep(rownames(params)[item], length(Probability)), Ability = Ability, 
                    Probability = Probability, itemID = rep(item, length(Probability)),
                    itemDiff = rep(b, length(Probability)), itemDisc = rep(a, length(Probability)))
    df_param <- rbind(df_param,t)
    
    t2 <- data.frame(item = rep(rownames(params)[item], length(abil)), Ability = abil,resp =resp[,item], itemID = rep(item, length(abil)))
    df_resp <- rbind(df_resp,t2)
  }
  df_param <- df_param[2:nrow(df_param),]
  df_resp <- df_resp[2:nrow(df_resp),]
  
  df_param$item <- factor(df_param$item)
  #df_param$itemComplete <- paste(df_param$item," [",round(params[item,2],2),", ",round(params[item,3],2),"]", sep="")
  for (i in 1:nrow(df_param)){
    df_param$itemComplete[i] <- paste(df_param$item[i]," [",round(params[df_param$itemID[i],2],2),", ",round(params[df_param$itemID[i],3],2),"]", sep="")
  }
  print(unique(df_param$itemComplete))
  #class(df_param$Ability)
  
  df_resp$resp <-  ifelse(df_resp$resp == 1, df_resp$resp + 0.05, df_resp$resp - 0.05)
  #df_resp$itemComplete <- paste(df_resp$item," [",round(params[item,2],2),", ",round(params[item,3],2),"]", sep="")
  for (i in 1:nrow(df_resp)){
    df_resp$itemComplete[i] <- paste(df_resp$item[i]," [",round(params[df_resp$itemID[i],2],2),", ",round(params[df_resp$itemID[i],3],2),"]", sep="")
  }
  
  # Plot
  if (sortBy == "Dffclt"){
    df_param$itemComplete  <- with(df_param, reorder(itemComplete, itemDiff))
    
  }else{
    if(sortBy == "Dscrmn"){
      df_param$itemComplete  <- with(df_param, reorder(itemComplete, itemDisc))
    }else{
      print("No specific order")
    }
  }
  
  plot <- ggplot(df_param, aes(Ability, Probability, group = itemComplete)) + 
    geom_line(size=1.2, alpha = 0.03)

  if(!neg){
    plot <- plot + geom_line(data = filter(df_param,itemID  %in% ind_game), aes(Ability, Probability, colour = itemComplete, linetype = itemComplete), size=2, alpha = 0.8) +
      geom_point(data = filter(df_resp, itemID %in% ind_game), aes(x = Ability, resp, colour = itemComplete), size = 4, alpha = 0.1)  
  }else{
    plot <- plot + geom_line(data = filter(df_param,itemID  %in% ind_game), aes(Ability, Probability, colour = itemComplete), size=2, alpha = 0.8) +
      geom_point(data = filter(df_resp, itemID %in% ind_game), aes(x = Ability, resp, colour = itemComplete), size = 4, alpha = 0.1)  
  }
  plot <- plot + theme_light() + 
    theme(legend.position = legend.pos,#c(0.15, 0.25)
          legend.text = element_text(size=13),#9
          legend.title = element_text(size=14),#10
          legend.key.size =  unit(0., "in"),
          axis.text=element_text(size=14),
          axis.text.x = element_text(angle = 0, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) 
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    #scale_color_discrete("Item [Dfclt, Dscrmn]") +
  if(neg){
    plot <- plot +  scale_colour_manual(values = ICCcolur.filter(n = length(ind_game)))+ #scale_linetype(guide=FALSE) +  
      guides(colour=guide_legend(ncol=cols)) +
      
      scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))
  }else{
    plot <- plot +  scale_colour_manual(values = ICC.diverging(n = length(ind_game)))+ #scale_linetype(guide=FALSE) +  
      guides(colour=guide_legend(ncol=cols)) +
      scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))
    
  }
   
  
    
  
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
#Results = ResultsGVGP; abil = abil.GVGP; data.var = GVGP.Variability

plot.GenAb <- function(Results, abil, data.var, howMany = 6, isAtari = TRUE, LM = FALSE, group = F, named = F){
  #library(RColorBrewer)
  library(scales) # needed for rescale
  #cols <- brewer.pal(n = 3, name = "RdBu") 
  cols <- c("red","violet","blue")
  # ABILITY PLOT
  abil.df <- data.frame(AITechnique = rownames(data.var), abil = abil)
  range(abil.df$abil) #-2.148880  2.224539
  
  plotAb <- ggplot(Results, aes(reorder(AItechnique, abil), abil, colour = abil)) + 
    geom_point(size=5) +
    #scale_colour_gradient2(mid="black") +
    scale_colour_gradientn(colours = cols, 
                           values = rescale(c(-3, -1, 0, 1, 3)),
                           guide = "colorbar", limits=c(-3, 3))+
    theme_light() + 
    theme(legend.position = "None",#c(0.15, 0.25)
          legend.text = element_text(size=7),#9
          legend.title = element_text(size=8),#10
          legend.key.size =  unit(0.1, "in"),
          axis.text=element_text(size=10),
          #axis.text.x = element_text(angle = 45, hjust = 1),margin = margin(0.2, unit = "cm"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.x = element_text(angle = 80, hjust = 0),
          axis.title.x = ) +
    #legend.direction = "vertical", 
    ylab("Ability") + xlab("") + scale_x_discrete(position = "top")  + ylim(c(-3,3))
  
  # GENERALITY PLOT
  
  Bernoulli <- data.frame(x=seq(0, 1, 0.01))
  Bernoulli$y <- Bernoulli$x * (1 - Bernoulli$x)
  
  mostAble <- Results[order(-Results$abil),][1:howMany,]
  lessAble <- Results[order(Results$abil),][1:howMany,]
  
  # MeanScore VS myvar
  plotGen <- ggplot(Bernoulli, aes(x,y)) + geom_line(colour = "gray48", size = 1, linetype="dashed")
  if(group){
    plotGen <- plotGen + geom_point(data = Results, aes(MeanScore, myvar, shape = Approach, colour= abil), size = 6)
    
  }else{
    plotGen <- plotGen + geom_point(data = Results, aes(MeanScore, myvar, colour= abil), size = 6)
    
  }
if(named){
  plotGen <- plotGen + geom_text_repel(data = mostAble, aes(MeanScore, myvar, label = AItechnique), 
                                       point.padding = 0.3,
                                       box.padding = 0.5,
                                       nudge_x = 45,
                                       size = 2,
                                       segment.alpha = 0.2) +
    geom_text_repel(data = lessAble, aes(MeanScore, myvar, label = AItechnique), 
                    point.padding = 0.3,
                    box.padding = 0.5,
                    nudge_x = -45,
                    size = 2,
                    segment.alpha = 0.2)
}
  plotGen <- plotGen + theme_classic() + ylab("Variance") + xlab("Score")  #ggtitle("MeanScore VS myvar") + 
    
    if(isAtari){
        plotGen <- plotGen + scale_shape_manual(values = c(15, 16, 17, 18, 11,8))
    }else{
      plotGen <- plotGen + scale_shape_manual(values = c(15, 16, 17, 18, 11, 8, 13, 12))
    }
    plotGen <- plotGen + #scale_colour_gradient2(mid="black") +
      scale_colour_gradientn(colours = cols, 
                             values = rescale(c(-3, -1, 0, 1, 3)),
                             guide = "colorbar", limits=c(-3, 3)) +
      theme_light() + 
      theme(legend.position = c(0.9, 0.75),#c(0.15, 0.25)
          legend.text = element_text(size=8),#9
          legend.title = element_text(size=9),#10
          legend.key.size =  unit(0.18, "in"),
          axis.text=element_text(size=12),
          axis.text.x = element_text(angle = 0, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) + 
      #legend.direction = "vertical", 
      xlim(c(0,1))
    
    if(LM){
      linMod <- lm(Results$myvar ~ Results$MeanScore)
      plotGen <- plotGen + geom_abline(intercept =linMod[[1]][1], slope = linMod[[1]][2]-0.02, colour = "darkred", size = 1.2, linetype ="dashed") +
        annotate("text", x= 0.5, y = 0.01, label= paste("y =",linMod[[1]][2],"x +", linMod[[1]][1] ,sep=""), colour= "darkred", size = 5)
    }
  
    
    Data <-  ifelse(isAtari, "Atari", "GVGP")
 
    openPDFEPS(paste(Data,"Ability",sep="."), height = 4, width = 7)
    plot(plotAb)
    dev.off()
    
    
    openPDFEPS(paste(Data,"Generality",sep="."), height = 5, width = 7)
    plot(plotGen)
    dev.off()
    
    g <- grid.arrange(plotAb, plotGen, layout_matrix = rbind(c(1),c(2,2),c(2,2)))
    openPDFEPS(paste(Data,"AbGen",sep="."), height = 12, width =7)
    plot(g)
    dev.off()
}

## Plot ACCs Teorethical
##############################################################################
# abil <- abil.Atari <- stuff.Atari[[1]]
# params <- params.Atari <- stuff.Atari[[2]]
# results <- Atari.Variability <- stuff.Atari[[3]] 

plot.ACC.Teo <- function(abil, params, results, mod =2, minDiff = -3, maxDiff = 3, filename, 
                         filter = c(), variance = FALSE, legpos = "rigth", cols = 1, medianDisc = F){
  
  getA <- function(b){
    t <- filter(params, Dffclt>=(round(b,2)-1) & Dffclt<=(round(b,2)+1) & Dscrmn >= 0 & Dscrmn <= 3)
    t <- mean(t$Dscrmn)
    #t<-filter(params.sum, cuts<b)$mDscrm
    #t <- t[length(t)]
    if(!is.na(t)){
      #print("->value")
      print(t)
      return(t)
      
    }else{
      print("->median")
      return(mean(filter(params, Dscrmn >= 0 & Dscrmn <= 3)$Dscrmn))
      #return(NA)
    }
  }
  
  getC <- function(b){
    t <- filter(params, Dffclt>=(round(b,2)-1) & Dffclt<=(round(b,2)+1))
    return(mean(t$Gussng))
    
  }
  Probability <- function(b,theta, mod = 2, mDisc=T) {
    
    a = mean(filter(params, Dscrmn >= 0)$Dscrmn)
    if(mod == "3"){
      if(!mDisc){
        a =getA(b)
      }
      c = getC(b)
    }else{
      if(mod == "2"){
        if(!mDisc){
          a =getA(b)
        }
        c = 0
      }else{
        a=1
        c=0
      }
    }
    #theta = abils["Rnd"]
    return(c + ((1-c)/(1+exp(-a*(theta-b)))))
    #1 / (1 + exp(x-abils["Rnd"])) 
  }
  params <- data.frame(params)
  #onlyCuts <- cut2(params$Dffclt, g = 20, onlycuts = T)
  #params$cutDiff <- cut2(params$Dffclt, g = 20)
  #table(params$cutDiff)
  #library(dplyr)
  #params.sum <-group_by(params, cutDiff)
  #params.sum <- summarize(params.sum, mDscrm = mean(Dscrmn))
  #params.sum$cuts <- onlyCuts[1:(length(onlyCuts)-1)]
  
  abils <- data.frame(Agent = rownames(results), 
                      Ability = abil)
  rownames(abils)<- rownames(results)
  
  #myf <- function(x) {1 / (1 + exp(x-abils["Talvitie_B.PRO","Ability"])) }
  #plot(myf,xlim=c(-3.32,0.54), ylim=c(0.25,1.0), col="red",xlab= "Difficulty", ylab= "Probability",lwd=3)
  
  df <- data.frame(difficulty=seq(minDiff,maxDiff,0.1))
  for(i in 1:nrow(df)){
    print(i)
    for(j in rownames(abils)){
      df[i,j] = Probability(df[i,1], abils[j,"Ability"], medianDisc)
    }
  }

  df.m <- melt(df, id.vars = c("difficulty"))
  df.m <- df.m[complete.cases(df.m),]
  
  
  if(length(filter)>0){
    df.m <- filter(df.m, variable%in%filter)
  }
  
  g <- ggplot(df.m, aes(difficulty, value, colour = variable))
  
  if(variance){
    g <- g + geom_ribbon(aes(ymin = value - ((value*(1 - value))/2), ymax = value + ((value*(1 - value))/2), fill = variable),  alpha = 0.1, linetype= "dashed")
  }
  
  g <- g +
    geom_line(size = 1.2, alpha = 0.7) + 
    #facet_wrap(~variable)+
    theme_bw() + xlim(c(minDiff,maxDiff)) +
    xlab("Difficulty") + ylab("Prob. Correct Response") +
    geom_rug(data= filter(params, Dscrmn >=0), aes(x = Dffclt, y = 0), sides="b", col="darkred") +
    theme_light() + 
    theme(legend.position = legpos,
      legend.text = element_text(size=13),#9
      legend.title = element_text(size=14),#10
      legend.key.size =  unit(0.15, "in"),
      axis.text=element_text(size=14),
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()) +
    guides(colour=guide_legend(ncol=1)) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))+
    guides(colour=guide_legend(ncol=cols)) 
  
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    #scale_color_discrete("Item [Dfclt, Dscrmn]") + 

    g <- g + scale_colour_manual(values = ICCcolour3(n = length(unique(as.character(df.m$variable))))) #scale_linetype(guide=FALSE) +  
  
  openPDFEPS(paste(filename, sep=""),height = 5, width = 8)
  plot(g)
  dev.off()

}

## Plot ACCs Empirical
##############################################################################
# abil <- abil.Atari; params <-  params.Atari; results <-  Atari.Variability;cuts= c(-600,-1,1,2, 600)
# filterTechs <- filter.Atari; results <- Atari.Variability;  results.var <- Atari.Variability

plot.ACC.Empirical <- function(abil, params, results, results.var, mod =2, minDiff = -3, maxDiff = 3, 
                               cuts =c(), groups = 10, filename, filterTechs = c(), cont = FALSE, variance = FALSE, legpos = "rigth", cols = 1){
  
  params <- data.frame(params)
  
  all <- data.frame(t(results))
  all <- cbind(all, params)
  all$Game <- rownames(all)
  all$Dffclt <- as.numeric(all$Dffclt)
  print(nrow(all))
  #all <- filter(all, Dffclt >= minDiff, Dffclt <= maxDiff, Dscrmn >=0)
  all <- filter(all, Dscrmn >=0)
  
  
  all.var <- data.frame(t(results.var))
  all.var <- cbind(all.var, params)
  all.var$Game <- rownames(all.var)
  all.var$Dffclt <- as.numeric(all.var$Dffclt)
  print(nrow(all.var))
  #all <- filter(all, Dffclt >= minDiff, Dffclt <= maxDiff, Dscrmn >=0)
  all.var <- filter(all.var, Dscrmn >=0)
  
  print(nrow(all))
  print(nrow(all.var))
  
  #View(all)
  
  if (length(cuts) == 0){
    all$cutDiff <- cut2(all$Dffclt, g = groups)
    all.var$cutDiff <- cut2(all.var$Dffclt, g = groups)
  }else{
    all$cutDiff <- cut(all$Dffclt, breaks = cuts)
    all.var$cutDiff <- cut(all.var$Dffclt, breaks = cuts)
  }
  print(table(all$cutDiff))
  print(table(all.var$cutDiff))
  
  
  #View(all)
  all <- all[,!colnames(all)%in%c("Gussng","Dscrmn","id","game")]
  all.var <- all.var[,!colnames(all.var)%in%c("Gussng","Dscrmn","id","game")]
  
  #Fix min max
  all$cutDiff <- factor(all$cutDiff, levels=c(levels(all$cutDiff), "Fix+6", "Fix-6"))
  all[nrow(all)+1,] <- c(rep(1,ncol(all)-3), as.numeric(-6),NA, "Fix-6")
  all[nrow(all)+1,] <- c(rep(1,ncol(all)-3), as.numeric(-6),NA, "Fix-6")
  all[nrow(all)+1,] <- c(rep(0,ncol(all)-3),  as.numeric(6),NA, "Fix+6")
  all[nrow(all)+1,] <- c(rep(0,ncol(all)-3),  as.numeric(6),NA, "Fix+6")
  all$Dffclt <- as.numeric(all$Dffclt)
  
  all.var$cutDiff <- factor(all.var$cutDiff, levels=c(levels(all.var$cutDiff), "Fix+6", "Fix-6"))
  all.var[nrow(all.var)+1,] <- c(rep(1,ncol(all.var)-3), as.numeric(-6),NA, "Fix-6")
  all.var[nrow(all.var)+1,] <- c(rep(1,ncol(all.var)-3), as.numeric(-6),NA, "Fix-6")
  all.var[nrow(all.var)+1,] <- c(rep(0,ncol(all.var)-3),  as.numeric(6),NA, "Fix+6")
  all.var[nrow(all.var)+1,] <- c(rep(0,ncol(all.var)-3),  as.numeric(6),NA, "Fix+6")
  all.var$Dffclt <- as.numeric(all.var$Dffclt)
  
  
  all.melt <- melt(all, id.vars = c("Game", "cutDiff", "Dffclt"))
  all.melt$value <- as.numeric(all.melt$value)
  
  all.var.melt <- melt(all.var, id.vars = c("Game", "cutDiff", "Dffclt"))
  all.var.melt$value <- as.numeric(all.var.melt$value)
  #View(all.melt)
  #all.melt$Result <- as.integer(all.melt$Result)
  colnames(all.melt) <- c("Game", "cutDiff", "Dffclt", "System", "Result")
  colnames(all.var.melt) <- c("Game", "cutDiff", "Dffclt", "System", "Result")
  
  #sapply(all.melt, class)
  
  by_bin <- group_by(all.melt, System, cutDiff)
  all.sum <- summarise(by_bin, Score = mean(Result), medianDiff = median(Dffclt), variance = myvar(Result))
  
  by_bin.var <- group_by(all.var.melt, System, cutDiff)
  all.var.sum <- summarise(by_bin.var, Score = mean(Result), medianDiff = median(Dffclt), variance = myvar(Result))
  #View(all.sum)
  
  all.sum$varOrig <- all.var.sum$variance
  if(length(filterTechs)>0){
    all.sum <- filter(all.sum, System%in%filterTechs)
  }
  
  # PLOT
  g <- ggplot(all.sum, aes(medianDiff, Score, colour = System))
  
  if(variance){
    g <- g + 
      geom_ribbon(aes(ymin = Score - (varOrig/2), ymax = Score + (varOrig/2), fill = System),  alpha = 0.1, linetype= "dashed") +
      scale_linetype(guide=FALSE)
  }
  if(cont){
     g <- g + 
      geom_line(size = 1.5, alpha = 0.7) +
      geom_vline(aes(xintercept = medianDiff), linetype = "dotted", size = 0.7, colour ="black") +
      xlab("Difficulty") + ylab("Mean Score")
  }else{
    g <- g + 
      geom_line(size = 1.5, alpha = 0.7) +
      xlab("Difficulty") + ylab("Mean Score")  
  }

  g <- g + theme_light() + 
    theme(legend.position = legpos,
          legend.text = element_text(size=13),#9
          legend.title = element_text(size=14),#10
          legend.key.size =  unit(0.15, "in"),
          axis.text=element_text(size=14),
          axis.text.x = element_text(angle = 0, hjust = 1),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()) +
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    #scale_color_discrete("Item [Dfclt, Dscrmn]") + 
    guides(colour=guide_legend(ncol=1)) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_colour_manual(values = ICCcolour3(n = length(unique(as.character(all.sum$System))))) +
    guides(colour=guide_legend(ncol=cols))  + xlim(minDiff,maxDiff)
  
  # if (length(filterTechs)==0){
  #   g <- g + scale_colour_manual(values = ICCcolour3(n = length(levels(all.sum$System))))
  # }else{
  #   g <- g + scale_colour_manual(values = ICCcolur.filter(n = length(levels(all.sum$System))))
  # }
  
  openPDFEPS(paste(filename, sep=""),height = 4, width = 8)
  plot(g)
  dev.off()
  return(all.sum)
  
  
}
#abil <- abil.Atari; params <-  params.Atari; results <-  Atari.Variability; filterTechs = filter.Atari
#abil <- abil.GVGP; params <-  params.GVGP; results <-  GVGP.Variability

plot.ACC.Emp.window <- function(abil, params, results, mod =2, minDiff = -3, maxDiff = 3,filename, window = 10,
                                filterTechs = c(), variance = FALSE){
  
  getScore <- function(b){
    temp <- filter(all, Dffclt>=(round(b,2)-0.5) & Dffclt<=(round(b,2)+0.5) & Dscrmn >= 0)
    if(nrow(temp)>0){
      temp <- colMeans(temp[,1:(ncol(temp)-5)])
      return(temp)
    }else{
      return(rep(NA,(length(colnames(temp))-5)))
      
    }
  }
  getScoreRows <- function(i){
    #temp <- all[(i-window+1):(i+window),]
    temp <- all[i:(i+window-1),]
    if(nrow(temp)>0){
      temp <- colMeans(temp[,1:(ncol(temp)-5)])
      return(temp)
    }else{
      return(rep(NA,(length(colnames(temp))-5)))
      
    }
  }
  
  getVarianceRows <- function(i){
    #temp <- all[(i-window+1):(i+window),]
    temp <- all[i:(i+window-1),]
    if(nrow(temp)>0){
      temp <- temp[,1:(ncol(temp)-5)]
      temp <- sapply(temp,myvar)
      return(temp)
    }else{
      return(rep(NA,(length(colnames(temp))-5)))
      
    }
  }
  
  params <- data.frame(params)
  params <- params[order(params$Dffclt),]
  
  all <- data.frame(t(results))
  all <- cbind(all, params)
  all$Game <- rownames(all)
  all <- filter(all, Dffclt >= minDiff, Dffclt <= maxDiff, Dscrmn >=0)

  #difficulty[window:length(difficulty)]
  #difficulty=seq(minDiff,maxDiff,0.1)
  difficulty = all$Dffclt 
  df <- data.frame()
  df.var <- data.frame()
  for(i in 1:(length(difficulty)-window)){
      df <- rbind(df, c(difficulty[i],getScoreRows(i)))
      df.var <- rbind(df.var, c(difficulty[i],getVarianceRows(i)))
  }
  colnames(df) <- c("difficulty", colnames(all)[1:(ncol(all)-5)])
  colnames(df.var) <- c("difficulty", colnames(all)[1:(ncol(all)-5)])
  
  #View(df)
  
  df.m <- melt(df, id.vars = c("difficulty"))
  df.var.m <- melt(df.var, id.vars = c("difficulty"))
  colnames(df.m) <- c("difficulty", "System", "Score")
  df.m$Var <- df.var.m$value

  if(length(filterTechs)>0){
    df.m <- filter(df.m, System%in%filterTechs)
  }
  
  g <- ggplot(df.m, aes(difficulty, Score, colour = System)) 
  if (variance){
    g <- g + 
      geom_ribbon(aes(ymin = Score - Var, ymax = Score + Var, fill = System),  alpha = 0.1, linetype= "dashed") 
  }
  g<- g + geom_line(size = 0.7, alpha = 0.8) + theme_bw() + xlim(c(minDiff,maxDiff)) +
    xlab("Difficulty") + ylab("Prob. Correct Response") +
    geom_rug(data= filter(params, Dscrmn >=0), aes(x = Dffclt, y = 0), sides="b", col="darkred") +
    theme_light() + 
    theme(#legend.position = c(0.15, 0.25),
      legend.text = element_text(size=9),#9
      legend.title = element_text(size=10),#10
      legend.key.size =  unit(0.1, "in"),
      axis.text=element_text(size=7),
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()) +
    #legend.direction = "vertical", 
    #legend.box = "vertical") +   
    #scale_color_discrete("Item [Dfclt, Dscrmn]") + 
    scale_colour_manual(values = ICCcolour3(n = length(unique(as.character(df.m$System)))))+ #scale_linetype(guide=FALSE) +  
    guides(colour=guide_legend(ncol=1)) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1), labels = c(0,0.25,0.5,0.75,1))
  
  

  
  openPDFEPS(paste(filename, sep=""),height = 6, width = 10)
  plot(g)
  dev.off()
  
  
}

closest<-function(xv,sv){
  xv[which(abs(xv-sv)==min(abs(xv-sv)))] }

# Scatter plot SLOPE (generality) & Regularity (1/variance)
genEmpACC <- function(cutData, Results, abil, filename, filter = c(), ylimit=65, text = F){

  slope <- function(x1,x2,y1,y2){
    return(as.numeric(abs(y2-y1)/abs(x2-x1)))
  }
  Slope = NULL
  df.sl = data.frame(System = NA, Slope = NA)
  for (i in levels(cutData$System)){
    df <- filter(cutData, System == i)
    #ind <- which(abs(df$Score-0.5)==min(abs(df$Score-0.5)))
    for(j in 1:3){
      if(df[j,"Score"]>0.5){
        if(df[j+1,"Score"]<0.5){
          sl <- slope(df[j,"medianDiff"],df[j+1,"medianDiff"],df[j,"Score"], df[j+1,"Score"])
          break
        }
      }else{
        sl <- NA
      }
    }
    
      Slope <-c(Slope,unlist(sl))
  }
  
  df.sl <- data.frame(System = levels(cutData$System), Slope = Slope, Generality = Results$var.Inv)
  
  print(df.sl)
  g <- ggplot(df.sl,aes(Slope, Generality, label = System)) + geom_point(size=8,alpha = 0.4, colour = "black") 
  
  if(length(filter)!=0){
    df.filter <- filter(df.sl, System%in%filter)
    g <- g + geom_point(data=df.filter, aes(Slope, Generality, colour = System), size=8)
    
    if(text){
      g <- g + geom_text_repel(data=df.filter, aes(Slope, Generality, label = System), 
                        point.padding = 0.3,
                          box.padding = 0.5,
                          nudge_x = 5,
                          size = 4,
                          segment.alpha = 0.2) 
    }
      
    g <- g +  scale_colour_manual(values = ICCcolur.filter(n = length(filter))) 
  }
    g <- g + theme_light() + ylim(10,ylimit) + ylab("Regulary (inverse of variance)") +
      xlab("Slope (proxy for generality)") +
    theme(legend.position = "none",
      axis.text=element_text(size=14),
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.border = element_blank()) 

  openPDFEPS(paste(filename, sep=""),height = 3, width = 4)
  plot(g)
  dev.off()
  
  return(df.sl)
  }


