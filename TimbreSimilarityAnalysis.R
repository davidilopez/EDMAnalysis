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


## Experiment 1 -----------------------------------------------------------------
# Calculate the Fleiss' Kappa of the timbre similarity ratings for all the pairs.
timbre.kappa.4.E1 <- fleiss.EDM(data = timbre.raw, polarize = FALSE)  # 4pt scale
timbre.kappa.2.E1 <- fleiss.EDM(data = timbre.raw, polarize = TRUE)  # 2pt scale

# We need the 20 pairs with the lowest SD and the 20 pairs with the highest SD, s
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



