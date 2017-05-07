
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

theme_be <- function() {

    ## Dependencies
    library(ggplot2)
    library(ggthemes)
    library(RColorBrewer)
    library(extrafont)

    ## Generate the colors for the chart procedurally with RColorBrewer
    palette <- brewer.pal("Greys", n=9)
    color.background = palette[1]
    color.grid.major = palette[3]
    color.axis.text = palette[6]
    color.axis.title = palette[7]
    color.title = palette[9]

    ## Begin construction of chart
    theme_bw(base_size=15) +

        ## Set the entire chart region to a light gray or white color
        theme(panel.background=element_rect(fill=color.background, color=color.background)) +
        theme(plot.background=element_rect(fill=color.background, color=color.background)) +
        theme(panel.border=element_rect(color=color.background)) +

        ## Format the grid
        theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
        theme(panel.grid.minor=element_line(color=color.grid.major,size=.10)) +
        theme(axis.ticks=element_line(color=color.grid.major, size=.25)) +
        theme(axis.line.x=element_line(color=color.grid.major, size=1.5),
              axis.line.y=element_line(color=color.grid.major, size=1.5)) +

        ## Format the legend
        theme(legend.background=element_rect(fill=color.background)) +
        theme(legend.key=element_rect(fill=color.background)) +
        theme(legend.text=element_text(color=color.axis.title)) +
        theme(legend.position='top') +

        ## Set title and axis labels, and format these and tick marks
        theme(plot.title=element_text(color=color.axis.title, vjust=1.25)) +
        theme(axis.text.x=element_text(color=color.axis.text)) +
        theme(axis.text.y=element_text(color=color.axis.text)) +
        theme(axis.title.x=element_text(color=color.axis.title, vjust=0)) +
        theme(axis.title.y=element_text(color=color.axis.title, vjust=1.25)) +

        ## Plot margins
        theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"))
}
