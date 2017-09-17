#' Funtion for converting date to mon.yr
#'
#' Converts any date to format mon.yr (ex. Jul.17, Aug.14, etc), as an ordered factor.
#' @param
#' @keywords date conversion
#' @export
#' @examples
#' monyr(df$date)



#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# FUNCTION
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

monyr <- function(date_column) {

    datecol <-
        factor(
            paste(
                month.abb[as.POSIXlt(date_column)$mon + 1],
                as.POSIXlt(date_column)$year - 100,
                sep = "."
            ),
            levels =
                as.vector(
                    outer(
                        month.abb,
                        sort(unique(as.POSIXlt(date_column)$year - 100)),
                        paste,
                        sep = "."
                    )
                ),
            ordered = T
        )

    datecol <- droplevels(datecol)

    return(datecol)

}
