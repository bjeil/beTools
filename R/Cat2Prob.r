
#' Function to calculate target-likelyhood for groups for a categorical value
#'
#' This function is used to return the target-likelyhood for groups for a categorical value.
#' It is used if the variable contains to many groups to be used on its own.
#' @param varname - variable name, target - target variable name, df - the data frame,
#' cutoff - the minimum N for a categorical value, roundprob - the number of groups to report
#' (rounded likelyhoods)
#' @keywords
#' @export
#' @examples
#' Cat2Prob()


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# DEPENDENCIES
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

library(sqldf)


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# FUNCTION
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

Cat2Prob <-  function(varname,target,df,cutoff,roundprob) {

    #Create a temporary dataframe with only the vars we need
    TempDF <- data.frame(df[,c(varname,target)])

    #If the target variable is a factor convert it to numeric, else let it be
    if (is.factor(TempDF[[target]])){
        TempDF[[target]] <- as.numeric(as.character(TempDF[[target]]))
    }

    #Count occurences of group, find likelyhood for group
    res1  <-  fn$sqldf('select $varname
                       ,count($varname) as N
                       ,sum($target) as sumtarget
                       from TempDF
                       group by $varname
                       order by  N desc
                       ;
                       quit')

    res1$likelyhood <- res1$sumtarget/res1$N

    #Return -1 if N in group is to low to be meaningful
    res2 <- fn$sqldf('select
                     $varname,N
                     ,case when N >= $cutoff then ((round(Likelyhood/$roundprob))*$roundprob) else -999 end as Aveprob from res1; quit')

    res2$Aveprob[res2$Aveprob==-999]  <- "Missing"
    res2$Aveprob <- as.factor(res2$Aveprob)

    return(res2)

}
