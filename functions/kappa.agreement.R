kappa.agreement <- function(kappa){
        if (kappa <= 0) {
                agreement <- "poor"
        } else if (kappa >= 0.01 & kappa <= 0.20) {
                agreement <- "slight"
        } else if (kappa >= 0.21 & kappa <= 0.40) {
                agreement <- "fair"
        } else if (kappa >= 0.41 & kappa <= 0.60) {
                agreement <- "moderate"
        } else if (kappa >= 0.61 & kappa <= 0.80) {
                agreement <- "substantial"
        } else if (kappa >= 0.81 & kappa <= 1) {
                agreement <- "almost perfect"
        }
        return(agreement)
}