
#' Function for finding the denominator of a normalized vector
#'
#' #This function takes a vector that has been normalized and tries to find the denominator
#' used for normalizing. To un-normalize the vector multiply it by the result of this function,
#' like this UnNorm <- Normalized*FindDenom(Normalized).
#'
#' The function works by trying to find out how big a unit of 1 in the unnormalized set is in
#' this normalized set. We find this by sorting and checking the most common difference
#' (Norm[i]-Norm[i-1]). The most common difference probably represents the unit 1.
#' @param input (EXPLAIN!)
#' @keywords normalization, un-normalize, denominator
#' @export
#' @examples
#' FindDenom()


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# FUNCTION
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

FindDenom <- function(input){

    #Sort the input, round to 8 digits
    sorted <- round(sort(input),8)

    #Calculate the differences (Norm[i]-Norm[i-1])
    differences <- round(diff(sorted,lag=1,differences=1),10)

    #Keep only meaninful differences
    differences <- differences[differences>0.000001]

    #find the most frequent difference
    aggr <- sort(table(differences),decreasing = T)

    #return 1/[Most frequent difference] as the denominator
    return(1/as.numeric(names(aggr[1])))

}
