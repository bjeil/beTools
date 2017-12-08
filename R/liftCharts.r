#' Plots lift and gain
#'
#' Function for generating lift and gain charts, using actuals and predictions.
#' @param
#' actuals = the truth
#' predictions = the predictions
#' groups = number of groups (ex. deciles (10))
#' @keywords lift, gain, charts, plots, dplyr, ggplot
#' @export
#' @examples
#' df <- lift(truth, preds, groups = 10)
#' df$liftchart
#' df$gainchart
#' df$gaintable

### FUNCTION
################################################################################

liftCharts <- function(actuals, predictions, groups=10) {
    if(!require(dplyr)) {
        install.packages("dplyr")
        library(dplyr)
    }

    if(!require(ggplot2)) {
        install.packages("ggplot2")
        library(ggplot2)
    }

    if(is.factor(actuals)) actuals <- as.integer(as.character(actuals))
    if(is.factor(predictions)) predictions <- as.integer(as.character(predictions))

    helper = data.frame(cbind(actuals, predictions))
    helper[,"bucket"] = ntile(-helper[,"predictions"], groups)

    gaintable = helper %>% group_by(bucket)  %>%
        summarise_at(vars(actuals), funs(total = n(),
                                         totalresp = sum(., na.rm = TRUE))) %>%
        mutate(Cumresp = cumsum(totalresp),
               Gain = Cumresp / sum(totalresp) * 100,
               Cumlift = Gain / (bucket * (100 / groups))
        )

    # Lift chart
    p_lift <- ggplot(gaintable, aes(bucket, Cumlift)) +
        geom_line(colour = "blue", size = 1) +
        geom_point(colour = "blue", size = 3) +
        geom_hline(yintercept = 1, colour = "red", size = 1) +
        geom_point(aes(x = bucket, y = rep(1, groups)), colour = "red", size = 3) +
        scale_x_continuous(name = "Decile", breaks = 0:groups) +
        scale_y_continuous(name = "Cumulative lift", breaks = scales::pretty_breaks(8)) +
        coord_cartesian(xlim = c(0, groups))

    # Gain chart
    ## Add the ideal model curve to the gain chart
    p_gain <- ggplot(gaintable, aes(bucket, Gain)) +
        geom_line(colour = "blue", size = 1) +
        geom_point(colour = "blue", size = 3) +
        geom_line(aes(x = bucket, y = seq(10,100,10)), colour = "red", size = 1) +
        geom_point(aes(x = bucket, y = seq(10,100,10)), colour = "red", size = 3) +
        scale_x_continuous(name = "Decile", breaks = 1:10) +
        scale_y_continuous(name = "% of events", breaks = seq(10,100,10)) +
        coord_cartesian(xlim = c(1:10), ylim = c(10,100))

    return(
        list(
            gaintable = gaintable,
            liftchart = p_lift,
            gainchart = p_gain
        )
    )
}
