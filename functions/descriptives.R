## The function "descriptives" takes a database (raw or otherwise) and outputs
# common descriptive statistics. The inputs are the data to be processed, and if
# said data is raw (default = TRUE).

descriptives <- function(data, raw=TRUE, conf.1.rm = FALSE, tracknames = TRUE, polarize=FALSE, selection = "LONG") {
        require("psych")
#         Load the pair list.
        source("tracklist.R")
        if (selection != "LONG") {
                tracklist <- tracklist[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
        }
#         Check if the data has been processed, or if it is raw. If raw is TRUE,
#         it cleans and processes the data. If raw is FALSE, it runs the
#         descriptive statistics to the unchanged data.
        if (raw == T) {
#                Load helper functions
                source("reliability.check.R")
                source("clean.data.R")
#                 Run the data through the functions
#                 Remove unreliable cases
                reliable.data <- reliability.check(data)
#                 Clean and format the remaining cases
                clean.data <- clean.data.subjectwise(raw.data=reliable.data, conf.1.rm=conf.1.rm, polarize=polarize)
                desc <- as.data.frame(describe(clean.data))
        } else {
                desc <- as.data.frame(describe(data))
        }
        if (tracknames == TRUE) {
#         Merge the pair list with the descriptive statistics
        descriptives <- cbind(tracklist, desc)
        } else {
                descriptives <- desc
        }
        descriptives
}

# ------------------------------------------------------------------------------
# David López Mejía, June 2014
