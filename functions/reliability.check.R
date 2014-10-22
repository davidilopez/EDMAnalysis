## The function "reliability.check" assesses the two identical pairs presented to 
# the participants by comparing them against each other. A case will be "reliable"
# when both have the same value (plus/minus 1). "Unreliable" cases are discarded
# from the database. The output is in the same format as the input, without said
# cases.
reliability.check <- function(database, threshold = "normal") {
        if (threshold == "normal") {      
                database[((database$Reliability.check == database$Reliability.check.1) | 
                                  (database$Reliability.check == 1 & database$Reliability.check.1 == 2) |
                                  (database$Reliability.check == 2 & database$Reliability.check.1 == 1) |
                                  (database$Reliability.check == 2 & database$Reliability.check.1 == 3) |
                                  (database$Reliability.check == 3 & database$Reliability.check.1 == 2) |
                                  (database$Reliability.check == 3 & database$Reliability.check.1 == 4) |
                                  (database$Reliability.check == 4 & database$Reliability.check.1 == 3) | 
                                  is.na(database$Reliability.check)),]
        } else if (threshold == "heavy"){
                database[database$Reliability.check == database$Reliability.check.1,]
        }
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014