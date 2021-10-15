#' generate radam numbers
#'
#' @param n A positive number, the number of items to generated.
#' @param mean mean
#' @param sd sd
#' @param digits To the decimal places.
#'
#' @return numbers.
#' @export
#'
#' @examples
#' rnorm_std(10)
rnorm_std <-
    function(n=5,
             mean=0,
             sd=1,
             digits = 4
    ) {
        r_num <- stats::rnorm(n, mean, sd)
        r_num <- round(r_num ,digits)
        i <- 1
        while (abs(sd(r_num)/sd-1) > 0.00001 ) {
            r_num <- stats::rnorm(n, mean, sd)
            r_num <- round(r_num ,digits)
            i <- i+1
        }
        cat(paste0("This is the ", i, " times."), "\n")
        r_num <- mean-mean(r_num)+r_num
        cat("The mean is : ",mean(r_num), "\n")
        cat("The sd is : ",sd(r_num), "\n\n")
        return(r_num)
    }
