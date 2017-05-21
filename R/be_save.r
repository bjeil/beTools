
#' Custom settings for ggsave
#'
#' Custom preset settings for ggsave, fitting for PPT presentations.
#' @param No parameters
#' @keywords ggplot, ggthemes, theme
#' @export
#' @examples
#' be_save()


#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# FUNCTION
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

be_save <- function(plot = NULL,
                    filename = "",
                    dpi = 700,
                    size_ppt = "quarter",
                    height = 0,
                    width = 0
                    ) {
    
    ## Handling filename if function parameter empty
    filename <- ifelse(filename == "", paste0("gfx/", plot, ".png"), filename)
    
    ## Handling empty function parameter "plot"
    if (!exists(paste0(plot))) {  # Checking if plot input exists ## NEEEDS WORK!!
        return(print("Unrecognizable plot input."))
    } else if (height == 0 | width == 0) {  # Checking if height or width stated
            if (size_ppt == "quarter") {  # Checking for size input
                ggsave(plot, filename = filename, dpi = dpi, 
                       height = unit(3.15, "cm"), width = unit(6.3, "cm"))
            } else if (size_ppt == "half high") {  # Checking for size input
                ggsave(plot, filename = filename, dpi = dpi, 
                       height = unit(6.3, "cm"), width = unit(6.3, "cm"))
            } else if (size_ppt == "half wide") {  # Checking for size input
                ggsave(plot, filename = filename, dpi = dpi, 
                       height = unit(3.15, "cm"), width = unit(12.6, "cm"))
            } else if (size_ppt == "full") {  # Checking for size input
                ggsave(plot, filename = filename, dpi = dpi, 
                       height = unit(6.3, "cm"), width = unit(12.6, "cm"))
            } else return(print("Unrecognizable size_ppt. Use 'quarter', 'half high', 'half wide',
                                or 'full'. Sizes are of a PPT slide. Alternatively use the height
                                and width variables to specify size in cm."))
        } else {
            ggsave(plot, filename = filename, dpi = dpi, 
                   height = unit(height, "cm"), width = unit(width, "cm"))
        }   
}









