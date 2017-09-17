#' Missing variable plot
#'
#' Visualization of missing variables using VIM.
#' @param
#' @keywords VIM, missing variables, visualization
#' @export
#' @examples

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# VIM plot of missing variables
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

missingPlot <- 
    function(data, prop = FALSE, numbers = TRUE, sortVars = TRUE, 
             ylab = c("Missing data", "Pattern"), colors = c('lightgrey', 'red'), 
             gap = 3, cex.axis = .7
    ) {
        
        require(VIM)
        
        ## Plots missing varialbles
        aggr(data, col=c('lightgrey', 'red')
             , numbers = numbers
             , sortVars = sortVars
             , labels = names(data)
             , cex.axis = cex.axis
             , gap = gap
             , ylab = ylab
             , prop = prop
        )
    }
