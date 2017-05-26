
#' A custom theme for ggplot2
#'
#' A simple custom theme setup, based on theme_bw, base size 15. The theme is quite minimal.
#' Use theme(axis.line.x = element_blank(), axis.line.y=element_blank()) to remove axis lines.
#' @param No parameters
#' @keywords ggplot, ggthemes, theme
#' @export
#' @examples
#' ggplot(df, aes(x = x)) +
#' geom_histogram() +
#' theme_be()
#' theme_be()


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# FUNCTION
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

theme_be <- function(base_size = 15,
                     axis.lines = FALSE,
                     light.grid = TRUE,
                     legend.position = "right",
                     legend.background = element_blank(),
                     legend.title = element_text()
                     ) {

    require(ggplot2)
    require(ggthemes)
    require(RColorBrewer)
    require(extrafont)

    ## Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Greys", n=9)
    color.background = palette[1]
    color.grid.major = ifelse(test = (light.grid == T), palette[2], palette[3])
    color.axis.text = palette[6]
    color.axis.title = palette[7]
    color.title = palette[9]

    ## Begin construction of chart
    theme_bw(base_size = base_size) +

        ## Set the entire chart region to a light gray or white color
        theme(panel.background=element_rect(fill=color.background, color=color.background)) +
        theme(plot.background=element_rect(fill=color.background, color=color.background)) +
        theme(panel.border=element_rect(color=color.background)) +

        ## Format the grid
        theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
        theme(panel.grid.minor=element_line(color=color.grid.major,size=.10)) +
        theme(axis.ticks=element_line(color=color.grid.major, size=.25)) +

        if (axis.lines == T) {
            theme(axis.line.x=element_line(color=color.grid.major, size=1.5)) +
            theme(axis.line.y=element_line(color=color.grid.major, size=1.5))
        } else theme() +

        ## Format the legend
        theme(legend.background=legend.background) +
        theme(legend.key=element_rect(fill=color.background)) +
        theme(legend.text=element_text(color=color.axis.title)) +
        theme(legend.position=legend.position) +
        theme(legend.title = legend.title) +

        ## Set title and axis labels, and format these and tick marks
        theme(plot.title=element_text(color=color.axis.title, vjust=1.25)) +
        theme(axis.text.x=element_text(color=color.axis.text)) +
        theme(axis.text.y=element_text(color=color.axis.text)) +
        theme(axis.title.x=element_text(color=color.axis.title, vjust=0)) +
        theme(axis.title.y=element_text(color=color.axis.title, vjust=1.25)) +

        ## Plot margins
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))

}
