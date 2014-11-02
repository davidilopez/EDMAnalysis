# This function makes a selection of the pairs that were used for the General
# Similarity and Within Participant Concordance experiments.

# dataset is a data frame with the descriptive statistics as retrieved from the function
# "descriptives" from this repository. DO NOT USE RAW DATA.
select.EDM <- function(dataset, dimension = "rows"){
        if ( dimension == "rows") {
                dataset[c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190),]
        } else {
                dataset[,c(7, 17, 21, 39, 47, 53, 59, 62, 94, 111, 119, 149, 151, 176, 178, 184, 188, 190)]
        }
}