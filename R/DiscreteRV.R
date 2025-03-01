#' Calculate expected value for a discreet random variable given a PDF
#'
#' NOTE - Length of x must equal lenght of prob, and prob must sum to 1
#'
#' @param x The set of all possilbe values
#' @param r The set of probabilities for all expected values
#' @return The expected value (mean)
#' @export
drvmean <- function (x, prob){
    if (length(x) != length(prob)){
        return("X set and Probability sets must have the same length")
    }

    if (sum(prob) != 1){
        return("Sum of Probabilty set must equal 1")
    }

    mean_val <- sum(x * prob)
    return(mean_val)
}


#' Calculate variance for a discreet random variable given a PDF
#'
#' NOTE - Length of x must equal lenght of prob, and prob must sum to 1
#'
#' @param x The set of all possilbe values
#' @param r The set of probabilities for all expected values
#' @return The variance for the sets
#' @export
drvvar <- function (x, prob){
    if (length(x) != length(prob)){
        return("X set and Probability sets must have the same length")
    }

    if (sum(prob) != 1){
        return("Sum of Probabilty set must equal 1")
    }

    mean_val <- drvmean(x,prob)
    var_val <- sum ((x^2) * prob)
    var_val = var_val - mean_val^2

    return(var_val)
}


#' Calculate standard deviation for a discreet random variable given a PDF
#'
#' NOTE - Length of x must equal lenght of prob, and prob must sum to 1
#'
#' @param x The set of all possilbe values
#' @param r The set of probabilities for all expected values
#' @return The standard deviation for the sets
#' @export
drvdev <- function (x,prob){
    dev <- drvvar(x,prob)
    dev <- sqrt(dev)

    return(dev)
}