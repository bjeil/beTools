#' Custom settings for ggsave
#'
#' Custom preset settings for ggsave, fitting for PPT presentations.
#' @param
#' plot = plot object (without quotes)
#' filename = filename including folder structure from working directory
#' dpi = dpi setting
#' size_ppt = size parameter, in parts of a powerpoint slide ("quarter", "half high", "half wide", or "full")
#' height = custom height
#' width = custom width
#' @keywords ggplot, ggthemes, theme
#' @export
#' @examples
#' ## After creating plot, use be_save() to save the plot with preset options
#'
#' ## Filename created from the supplied plot object name, folder "./Gfx"
#' ## created in working directory, plot saved with default (quarter ppt) size.
#' be_save(plot = plt.test)
#'
#' ## Saved with custom file name, with size half heigh full width
#' be_save(plot = plt.test, filename = "Gfx/CUSTOM_NAME.png", size_ppt = "halfwide")
#'
#' ## Saved in custom folder, created if it doesn't already exist
#' be_save(plot = plt.test, folder = "CUSTOM_FOLDER")



#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# FUNCTION
#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------

be_save <- function(plot = NULL,
                    filename = "",
                    folder = "Gfx",
                    dpi = 700,
                    size_ppt = "quarter",
                    height = 0,
                    width = 0
) {

    require(ggplot2)

    if (!exists(paste0(deparse(substitute(plot)))) == T) {
        ## Handling empty function parameter "plot"

        stop("Unrecognizable plot input. Remember to use quotes when inputting plot")

        #return(print("Unrecognizable plot input. Remember to use quotes when inputting plot"))

    }

    ## Creating folder if it does not exist
    if(!dir.exists(file.path(paste0(getwd(), "/", folder)))) {

        dir.create(file.path(paste0(getwd(), "/", folder)))

        print(paste0("Folder created at ", file.path(paste0(getwd(), "/", folder))))

    }

    ## Handling filename if function parameter empty
    filename <- ifelse(filename == "", paste0(folder, "/", deparse(substitute(plot)), ".png"), filename)

    ## Handling height and width parameters
    if (height == 0 | width == 0) {  # Checking if height or width stated
        ## If no height or width is defined, check size_ppt parameter

        if (size_ppt == "quarter") {

            height = 3.15
            width = 6.3

        } else if (size_ppt == "half high") {

            height = 6.3
            width = 6.3

        } else if (size_ppt == "half wide") {

            height = 3.15
            width = 12.6

        } else if (size_ppt == "full") {

            height = 6.3
            width = 12.6

        } else return(print("Unrecognizable size_ppt. Use 'quarter', 'half high', 'half wide',
                                or 'full'. Sizes are of a PPT slide. Alternatively use the height
                            and width variables to specify size in cm."))

        ## Save the plot with size based on size_ppt
        return(
            ggsave(plot, filename = filename, dpi = dpi, device = "png",
                   height = unit(height, "cm"), width = unit(width, "cm"))
        )

    } else {

        ## Save plot with supplied height and width parameters
        return(
            ggsave(plot, filename = filename, dpi = dpi, device = "png",
                   height = unit(height, "cm"), width = unit(width, "cm"))
        )

    }
}

