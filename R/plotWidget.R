


###widget

#' Given a sea object containing layout information, draw the shark and dolphin plot
#' linked and interactive
#'
#' @param seaObject A sea object that contains layout information
#' @param shark Boolean - draw or not the shark plot
#' @param dolphin Boolean - draw or not the dolphin plot
#' @param shape The type of shape to construct the plot out of. The options are "spline" and "polygon" methods.
#' @param borderCol A color for the border line. If "NULL" then no border will be drawn.
#' @param vlines A vector of x positions at which to draw vertical lines
#' @param vlineCol A color value for the vertical lines
#' @param vlab A character vector containing labels for each of the vertical lines
#' @param vlabSize An integer value for the vertical labels size
#' @param separateIndependentClones Boolean - Should independently-arising clones (with parent 0) be separated by blank space in the plot?
#' @param showLegend Boolean - show or not a legend
#' @param markMeasuredTimepoints A vector of x positions at which to draw triangles on the bottom of the plot
#' @param mainDph A string for the title above the dolphin plot
#' @param mainPosDph A string for the dolphin plot title position. Options are 'left', 'middle' or 'right', always above the plot
#' @param mainSizeDph An integer value for the size of the dolphin plot title.
#' @param mainShk A string for the title above the shark plot
#' @param xlab String for the label on the dolphin plot x axis.
#' @param ylab String for the label on the dolphin plot y axis. Automatically vertical line showing 100% will be plotted.
#' @param pad.left The amount of "ramp-up" to the left of the first timepoint. Given as a fraction of the total plot width. Default 0.005
#' @param annotations A table with the columns x (x position), y (y position), lab (annotation text) and col (color of the text either black or white).
#' @param annotSize Integer value for the size of the annotations
#' @param width Integer value indicating the with of the output widget
#' @param height Integer value indicating the height of the output widget
#'
#' @return No return value, outputs on graphics device
#' @examples
#' \dontrun{
#' plotWidget(seaObject, borderCol = 'white', showLegend = T)
#'
#' plotWidget(seaObject, dolphin = F, showLegend = T)
#'
#' }
#' @export
#' @seealso sharkPlot, dolphinPlot


##Function to plot as interactive the plots you want
plotWidget <- function(seaObject, shark = TRUE, dolphin = TRUE,
                       shape = 'spline', borderCol = NULL, vlines = NULL,
                       vlineCol="#6E6E66", vlab=NULL, vlabSize = 3, pos = 'center',
                       separateIndependentClones=FALSE, showLegend = FALSE,
                       markMeasuredTimepoints = NULL, downloadWidget = NULL,
                       mainDph = NULL, mainPosDph = 'middle', mainSizeDph = 5,
                       mainShk = NULL,
                       xlab = NULL, ylab = NULL, pad.left = 0.005,
                       annotations = NULL, width = 12, height = 9){
  
  
  tooltip_css <- "color:black;padding:10px;border-radius:10px 20px 10px 20px;"
  
  if(shark & dolphin){
    
    splot <- sharkPlot(seaObject, showLegend = showLegend, main = mainShk)
    fplot <- dolphinPlot(seaObject, vlines = vlines, vlineCol = vlineCol,
                         vlab = vlab, shape = shape, borderCol = borderCol,
                         markMeasuredTimepoints = markMeasuredTimepoints,
                         main = mainDph, mainPos=mainPosDph, vlabSize = vlabSize,
                         mainSize = mainSizeDph, xlab = xlab,
                         ylab = ylab, pad.left = pad.left, pos=pos,
                         separateIndependentClones = separateIndependentClones,
                         annotations = annotations)
    
    ##interactive plot to html widget
    gPlot <- girafe(ggobj = fplot+splot, width_svg = width, height_svg = height) %>%
      girafe_options(opts_sizing(rescale = FALSE),
                     opts_hover_inv(css = "opacity:0.3;"),
                     opts_hover(css = "opacity:1;"),
                     opts_tooltip(css = tooltip_css, use_fill=TRUE))
  }
  
  else{
    if(shark & !dolphin){
      p <- sharkPlot(seaObject, showLegend = showLegend, main = mainShk)
    }
    if(dolphin & !shark){
      p <- dolphinPlot(seaObject, showLegend = showLegend, vlabSize = vlabSize,
                       vlines = vlines, vlineCol = vlineCol,
                       vlab = vlab, shape = shape, borderCol = borderCol,
                       markMeasuredTimepoints = markMeasuredTimepoints,
                       main = mainDph, mainPos=mainPosDph, pos = pos,
                       mainSize = mainSizeDph, xlab = xlab,
                       ylab = ylab, pad.left = pad.left,
                       separateIndependentClones = separateIndependentClones,
                       annotations = annotations)
    }
    
    gPlot <- girafe(ggobj = p, width_svg = width, height_svg = height) %>%
      girafe_options(opts_sizing(rescale = FALSE),
                     opts_hover_inv(css = "opacity:0.3;"),
                     opts_hover(css = "opacity:1;"),
                     opts_tooltip(css = tooltip_css, use_fill=TRUE))
  }
  
  if(!is.null(downloadWidget)){
    htmlwidgets::saveWidget(gPlot, downloadWidget,
                            selfcontained = TRUE)
  }
  
  
  return(gPlot)
}

