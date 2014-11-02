#------------------------------------------------------------------------------#
#                                                                              #
# Analysis for the "Perceptual concordance of timbre similarity in electronic  #
# dance music" research project.                                               #
#                                                                              #
# University of Amsterdam, 2014                                                #
# David I. Lopez Mejia, October 2014                                           #
#                                                                              #
#------------------------------------------------------------------------------#


## Setup -------------------------------------------------------------------
# Load functions for analysis 
set.seed(0)
source("functions/clean.data.R")
source("functions/descriptives.R")
source("functions/fleiss.EDM.R")
source("functions/select.EDM.R")
source("functions/wilcoxon.loop.R")
require("psych")
require("irr")

# Load databases from csv files
timbre.raw <- read.csv("data/timbre.csv")  # Timbre Sim Experiment 1 data
timbre.WPC.raw <- read.csv("data/timbreWPC.csv")  # Timbre Sim Experiment 2 data

general.raw <- read.csv("data/general.csv")  # General Sim Experiment 3 data

rhythm.raw <- read.csv("data/rhythm.csv")  # Rhythm Sim Experiment 3 data

# Clean the data for 4-point and 2-point scales
timbre.clean.4 <-clean.data.subjectwise(timbre.raw)
timbre.clean.2 <-clean.data.subjectwise(timbre.raw, polarize = TRUE)

rhythm.clean.4 <-clean.data.subjectwise(rhythm.raw)
rhythm.clean.2 <-clean.data.subjectwise(rhythm.raw, polarize = TRUE)


## Experiment 1 -----------------------------------------------------------------
# Calculate the Fleiss' Kappa of the timbre similarity ratings for all the pairs.
# timbre.kappa.4.E1 <- fleiss.EDM(data = timbre.raw, polarize = FALSE)  # 4pt scale
# timbre.kappa.2.E1 <- fleiss.EDM(data = timbre.raw, polarize = TRUE)  # 2pt scale

# We need the 20 pairs with the lowest SD and the 20 pairs with the highest SD,
# so we describe the data...
timbre.desc.4 <- descriptives(data = timbre.raw)
timbre.desc.2 <- descriptives(data = timbre.raw, polarize = TRUE)

# ... and sort it by its SD.
timbre.desc.sorted.4 <- timbre.desc.4[order(timbre.desc.4$sd),]
timbre.desc.sorted.2 <- timbre.desc.2[order(timbre.desc.2$sd),]

# Now we get the top 20 lowest SDs...
timbre.topSD.4 <- timbre.desc.sorted.4[1:20,]
timbre.topSD.2 <- timbre.desc.sorted.2[1:20,]

# ... and the top 20 highest SDs.
timbre.bottomSD.4 <- timbre.desc.sorted.4[171:190,]
timbre.bottomSD.2 <- timbre.desc.sorted.2[171:190,]

# For the pairs in both extremes, we perform a Fleiss' Kappa analysis to know 
# how the agreement in each side affects the overall similarity result.
# timbre.fleiss.top20.4 <- fleiss.EDM(data = timbre.clean.4[,timbre.topSD.4$PairNumber], is.raw = F)
# timbre.fleiss.top20.2 <- fleiss.EDM(data = timbre.clean.2[,timbre.topSD.2$PairNumber], is.raw = F)
# 
# timbre.fleiss.bottom20.4 <- fleiss.EDM(data = timbre.clean.4[,timbre.bottomSD.4$PairNumber], is.raw = F)
# timbre.fleiss.bottom20.2 <- fleiss.EDM(data = timbre.clean.2[,timbre.bottomSD.2$PairNumber], is.raw = F)

# We want to compare these to the rhythm data, so we perform the same analysis to
# the rhythm database.

# First describe the data...
rhythm.desc.4 <- descriptives(data = rhythm.raw)
rhythm.desc.2 <- descriptives(data = rhythm.raw, polarize = TRUE)

# ... and get the ratrings for the relevant pairs.
rhythm.desc.subset.top.4 <- rhythm.desc.4[timbre.topSD.4$PairNumber,]  # Top 10
rhythm.desc.subset.top.2 <- rhythm.desc.2[timbre.topSD.2$PairNumber,]

rhythm.desc.subset.bottom.4 <- rhythm.desc.4[timbre.bottomSD.4$PairNumber,]  # Bottom 10
rhythm.desc.subset.bottom.2 <- rhythm.desc.2[timbre.bottomSD.2$PairNumber,]

# Now make a table with the pair, the timbre sim level (H/L), and the rhythm sim
# level (H/L).

# This function checks the mean rating and determined if it is above or below 
# a certain threshold.
HiLo <- function(data, threshold){
        levels <- {}
        for (i in 1:nrow(data)) {
                if (data$mean[i] >= threshold){
                        levels <- c(levels, "H")
                } else {
                        levels <- c(levels, "L")
                }
        }
        return(levels)
}

# We make a table with the similarity levels of the lowest SDs for both 4 and 2 pt
timbre.HL.top.4 <- HiLo(timbre.topSD.4, 2.5)
rhythm.HL.top.4 <- HiLo(rhythm.desc.subset.top.4, 2.5)
table.top.4 <- cbind(timbre.topSD.4[,1:3], timbre.HL.top.4, rhythm.HL.top.4)

timbre.HL.top.2 <- HiLo(timbre.topSD.2, 1.5)
rhythm.HL.top.2 <- HiLo(rhythm.desc.subset.top.2, 1.5)
table.top.2 <- cbind(timbre.topSD.2[,1:3], timbre.HL.top.2, rhythm.HL.top.2)

# Now we make the same with the bottom ones
timbre.HL.bottom.4 <- HiLo(timbre.bottomSD.4, 2.5)
rhythm.HL.bottom.4 <- HiLo(rhythm.desc.subset.bottom.4, 2.5)
table.bottom.4 <- cbind(timbre.bottomSD.4[,1:3], timbre.HL.bottom.4, rhythm.HL.bottom.4)

timbre.HL.bottom.2 <- HiLo(timbre.bottomSD.2, 1.5)
rhythm.HL.bottom.2 <- HiLo(rhythm.desc.subset.bottom.2, 1.5)
table.bottom.2 <- cbind(timbre.bottomSD.2[,1:3], timbre.HL.bottom.2, rhythm.HL.bottom.2)

## Experiment 2-----------------------------------------------------------------
timbre.WPC.desc.4 <- descriptives(data = timbre.WPC.raw, raw = T, selection = "SHORT")
timbre.WPC.desc.2 <- descriptives(data = timbre.WPC.raw, raw = T, selection = "SHORT", polarize = T)


## Experiment 3-----------------------------------------------------------------
# Clean up the data for general similarity
general.clean.4 <- clean.data.subjectwise(raw.data = general.raw)
general.clean.2 <- clean.data.subjectwise(raw.data = general.raw, polarize = T)

# Test with Fleiss Kappa
kappam.fleiss(ratings = t(general.clean.4))
kappam.fleiss(ratings = t(general.clean.2))

# Test with Kappa the 18 selected pairs of timbre similarity
fleiss.EDM(data = timbre.raw, is.raw = T, selection = "SHORT", polarize = F)
fleiss.EDM(data = timbre.raw, is.raw = T, selection = "SHORT", polarize = T)

# Check if timbre and general are from the same population
timbre.general.wilc.4 <- wilcoxon.loop(data.1 = timbre.sel.clean.4, data.2 = general.clean.4)
timbre.general.wilc.2 <- wilcoxon.loop(data.1 = timbre.sel.clean.2, data.2 = general.clean.2)

# Get the 18 selected pairs for timbre and rhythm
timbre.sel.clean.4 <- select.EDM(timbre.clean.4, dimension = "cols")
timbre.sel.clean.2 <- select.EDM(timbre.clean.2, dimension = "cols")

rhythm.sel.clean.4 <- select.EDM(rhythm.clean.4, dimension = "cols")
rhythm.sel.clean.2 <- select.EDM(rhythm.clean.2, dimension = "cols")

# Test selection of timbre and rhythm ratings with Fleiss Kappa
fleiss.EDM(data = timbre.sel.clean.4, is.raw = F)
fleiss.EDM(data = timbre.sel.clean.2, is.raw = F)

fleiss.EDM(data = rhythm.sel.clean.4, is.raw = F)
fleiss.EDM(data = rhythm.sel.clean.2, is.raw = F)

# Check if timbre and rhythm are from the same population
timbre.rhythm.wilc.4 <- wilcoxon.loop(data.1 = timbre.sel.clean.4, data.2 = rhythm.sel.clean.4)
timbre.rhythm.wilc.2 <- wilcoxon.loop(data.1 = timbre.sel.clean.2, data.2 = rhythm.sel.clean.2)

# Check if rhythm and general are from the same population
general.rhythm.wilc.4 <- wilcoxon.loop(data.1 = general.clean.4, data.2 = rhythm.sel.clean.4)
general.rhythm.wilc.2 <- wilcoxon.loop(data.1 = general.clean.2, data.2 = rhythm.sel.clean.2)
