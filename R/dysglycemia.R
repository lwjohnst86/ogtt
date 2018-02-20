#' @describeIn add_variables Add IFG, IGT, DM, and dysglycemia variables to the
#'   dataset.

IGT <- function(fasting, two_hour) {
    fasting < 7 & two_hour < 11.1 & two_hour >= 7.8
}

DM <- function(fasting, two_hour, hba1c = NULL) {
    dm <- fasting >= 7 | two_hour >= 11.1

    if (!is.null(hba1c))
        dm <- dm | hba1c >= 6.5

    dm
}

IFG <- function(fasting, two_hour) {
    fasting <= 6.9 & two_hour >= 6.1 & two_hour < 7.8
}

