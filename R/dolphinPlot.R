


#' Given a sea object, draw the dolphin plot
#'
#' @param seaObject A sea object that contains layout information
#' @param shape The type of shape to construct the plot out of. The options are "spline" and "polygon" methods.
#' @param borderCol A color for the border line. If "NULL" then no border will be drawn.
#' @param pos Plotting position of the clones. Options are "center", "bottom" or "top".
#' @param vlines A vector of x positions at which to draw vertical lines
#' @param vlineCol A color value for the vertical lines
#' @param vlab A character vector containing labels for each of the vertical lines
#' @param vlabSize An integer value for the vertical labels size
#' @param separateIndependentClones Boolean - Should independently-arising clones (with parent 0) be separated by blank space in the plot?
#' @param showLegend Boolean - show legend at the left of the plot
#' @param markMeasuredTimepoints A vector of x positions at which to draw triangles on the bottom of the plot
#' @param main A string for the title above the plot
#' @param mainPos A string for the title position. Options are 'left', 'middle' or 'right', always above the plot
#' @param mainSize An integer value for the size of the title.
#' @param xlab String for the label on the x axis.
#' @param ylab String for the label on the y axis. Automatically vertical line showing 100% will be plotted.
#' @param pad.left The amount of "ramp-up" to the left of the first timepoint. Given as a fraction of the total plot width. Default 0.005
#' @param annotations A table with the columns x (x position), y (y position), lab (annotation text) and col (color of the text either black or white).
#' @param annotSize Integer value for the size of the annotations
#'
#' @return No return value, outputs on graphics device
#' @examples
#' \dontrun{
#' dolphinPlot(seaObject,shape="polygon",
#'            vlines=c(0,150), vlab=c("day 0","day 150"),
#'            main = 'Example plot')
#' }
#' @export
#'
dolphinPlot <- function(seaObject, shape = 'spline', borderCol = NULL, pos='center',
                        vlines=NULL, vlineCol="#6E6E66",
                        vlab=NULL, vlabSize = 3, separateIndependentClones = FALSE,
                        showLegend = FALSE, markMeasuredTimepoints = NULL,
                        main = NULL, mainPos = 'middle', mainSize = 5,
                        xlab = NULL, ylab = NULL, pad.left = 0.005,
                        annotations = NULL, annotSize = 3){
  x<-NULL
  y<-NULL
  name<-NULL
  ymin<-NULL
  ymax<-NULL
  tp<-NULL
  lab<-NULL
  
  ##get x and y values
  if (pos == 'center'){
    seaObject <- .layoutSeaObjectClones(seaObject,
                                        separateIndependentClones = separateIndependentClones)
  } else if(pos== 'bottom'){
    seaObject <- .bottomLayout(seaObject,
                               separateIndependentClones = separateIndependentClones)
  } else if (pos == 'top'){
    seaObject <-.topLayout(seaObject,
                           separateIndependentClones = separateIndependentClones)
  } else{
    stop(paste('Position (pos) must be either "center" or "bottom".',
               pos,'is not a valid position.'))
  }
  
  
  if (shape == 'polygon'){
    coordsTbl <- .getCoordsTable(seaObject)
    
  }else{
    if(shape == 'spline'){
      coordsTbl <- .getSmoothCoordsTbl(seaObject=seaObject, pad.left=pad.left)
    } else{
      stop('Shape must be wither "spline" or "polygon"')
    }
  }
  
  if(!is.null(borderCol)){
    dolphPlot <- ggplot(coordsTbl, aes(x = x, y = y)) +
      geom_polygon_interactive(aes(fill = name, group = name,
                                   tooltip = name,
                                   data_id = name),
                               show.legend = showLegend,
                               color = borderCol,
                               size = 0.25
      )
  }else{
    dolphPlot <- ggplot(coordsTbl, aes(x = x, y = y)) +
      geom_polygon_interactive(aes(fill = name, group = name,
                                   tooltip = name,
                                   data_id = name),
                               show.legend = showLegend
      )
  }
  
  dolphPlot <- dolphPlot +
    scale_fill_manual(values = seaObject@col, name = "Clone labels") +
    theme_graph() + theme(legend.position = "left")
  
  
  if(!is.null(vlines)){
    vlinesData <- data.frame(vlines = vlines,
                             ymin = rep(0,length(vlines)),
                             ymax = rep(100,length(vlines)))
    
    dolphPlot <- dolphPlot + geom_segment(mapping = aes(x = vlines,
                                                        xend=vlines,
                                                        y = ymin,
                                                        yend = ymax),
                                          data = vlinesData,
                                          color = vlineCol,
                                          show.legend = FALSE)
    
    if(!is.null(vlab)){
      vlinesData <- vlinesData %>% add_column(vlab = vlab)
      dolphPlot <- dolphPlot + geom_text(mapping = aes(x = vlines,
                                                       y = 100,
                                                       label = vlab,
                                                       vjust = -1),
                                         size = vlabSize,
                                         data = vlinesData) +
        expand_limits(y=100+vlabSize)
    }
  }
  
  if(!is.null(markMeasuredTimepoints)){
    measuredTpData <- data.frame(tp = markMeasuredTimepoints,
                                 y = rep(-2,length(markMeasuredTimepoints)))
    dolphPlot <- dolphPlot + geom_point(mapping = aes(x = tp, y = y),
                                        data = measuredTpData,
                                        shape = 'triangle',
                                        size = 2)
  }
  
  if(!is.null(main)){
    if(mainPos == 'middle'){
      mainCoords <- (min(seaObject@timepoints)+max(seaObject@timepoints))/2
    } else{
      if(mainPos == 'left'){
        mainCoords <- min(seaObject@timepoints)
        
      }else{
        if(mainPos == 'right'){
          mainCoords <- max(seaObject@timepoints)
        }else{
          stop('main position (mainPos) must be "left", "middle", or "right"')
        }
      }
    }
    
    mainY <- mainSize*2 + 100
    
    if(mainY < 110 & !is.null(vlab)){
      mainY  <- 110
    }
    
    mainLabel <- data.frame(lab = main,
                            x = mainCoords,
                            y = mainY)
    
    dolphPlot <- dolphPlot + geom_text(mapping = aes(x = x,
                                                     y = y,
                                                     label = lab),
                                       size = mainSize,
                                       data = mainLabel)
  }
  
  if(!is.null(xlab)){
    
    axisLabelX <- data.frame(lab = xlab,
                             x = ((min(seaObject@timepoints)+max(seaObject@timepoints))/2),
                             y = -6)
    
    dolphPlot <- dolphPlot + geom_text(mapping = aes(x = x,
                                                     y = y,
                                                     label = lab),
                                       data = axisLabelX)
  }
  
  if(!is.null(ylab)){
    pad <- max(coordsTbl$x)-min(coordsTbl$x)
    axisLabelY <- data.frame(lab = ylab,
                             x = min(coordsTbl$x) - pad*0.18,
                             y = 50)
    yLine <- data.frame(x = min(coordsTbl$x) - pad*0.05,
                        ymin = 0,
                        ymax =100)
    percentages <- data.frame(lab = '100%',
                              x = min(coordsTbl$x) - pad*0.08,
                              y = 50)
    
    dolphPlot <- dolphPlot +
      geom_text(mapping = aes(x = x,y = y,label = lab,angle = 90),
                data = axisLabelY) +
      geom_segment(mapping = aes(x = x, xend=x, y = ymin, yend = ymax),
                   data = yLine, color = 'black', show.legend = FALSE,
                   size = 0.2) +
      geom_text(mapping = aes(x = x, y = y, label = lab, angle = 90),
                data = percentages,
                size=3)
    
  }
  
  ##Annotations
  if(!is.null(annotations)){
    if(sum(annotations$col %in% c('black','Black')) == 0){
      colorsScale <- 'white'
    } else {
      colorsScale <- c('black','white')
    }
    dolphPlot <- dolphPlot + geom_text(mapping = aes(x = x,
                                                     y = y,
                                                     label = lab,
                                                     color = col),
                                       size = annotSize,
                                       data = annotations) +
      guides(color = 'none') +
      scale_color_discrete(type = colorsScale)
  }
  
  
  return(dolphPlot)
}

