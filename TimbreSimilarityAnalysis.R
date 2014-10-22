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
source("functions/reliability.check.R")
source("functions/fleiss.EDM.R")
source("functions/select.EDM.R")
require("psych")
require("irr")

# Load databases from csv files
timbre.raw <- read.csv("data/timbre.csv")  # Timbre Sim Experiment 1 data
timbre.WPC.raw <- read.csv("data/timbreWPC.csv")  # Timbre Sim Experiment 2 data

general.raw <- read.csv("data/general.csv")  # General Sim Experiment 3 data

rhythm.raw <- read.csv("data/rhythm.csv")  # Rhythm Sim Experiment 3 data


## Experiment 1 -----------------------------------------------------------------

