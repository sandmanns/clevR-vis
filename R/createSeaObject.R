



#' Create a seaObject
#'
#' @param fracTable A numeric matrix containing tumor fraction estimates for all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between clones
#' @param timepoints An numeric vector specifying the timepoints for each column of the matrix
#' @param col A vector of colors to use when plotting each clone
#' @param cloneLabels A character vector of names to assign to each clone when plotting a legend

#' @return A fish object with the relevant slots filled
#' @export
#'
#' @examples
#' timepoints=c(0,30,75,150)
#' fracTable = matrix(
#'     c(100, 45, 00, 00,
#'        02, 00, 00, 00,
#'        02, 00, 02, 01,
#'        98, 00, 95, 40),
#'     ncol=length(timepoints))
#' parents = c(0,1,1,3)
#' seaObject = createSeaObject(fracTable,parents,timepointss)
#'
createSeaObject <- function(fracTable,parents,timepoints=NULL,col=NULL,
                            cloneLabels=NULL, originTimepoint=NULL,
                            timepointEstimation=TRUE, therapyEffect=NULL){
  
  .initSeaObjectClass()
  
  #nest levels
  nestLevels = .getAllNestLevels(parents)
  
  ##Change the name of the clones to numbers (in fracTable)
  rownames(fracTable)<-seq(1:dim(fracTable)[1])
  
  ##timepoints as column names
  colnames(fracTable) <- timepoints
  
  #default clone labels are just Clone: + 1:numClones
  if(is.null(cloneLabels)){
    cloneLabels <- paste('Clone:',as.character(1:nrow(fracTable)))
    defaultLabels = TRUE
  } else (defaultLabels = FALSE)
  
  ##Check for single timepoints that timepoint estimation is true
  if(!timepointEstimation & length(timepoints)==1){
    stop("ERROR: To visualize clonal evolution from single timepoint
             timepointEstimation mustbe set to TRUE")
  } else if (is.null(originTimepoint) & length(timepoints)==1){
    stop('ERROR: To visualize clonal evolution from a single timepoint
             originTimepoint must be manually specified.')
  }
  
  #check origin timepoint
  if(is.null(originTimepoint)){
    originTimepoint <- timepoints[1] - (timepoints[2] - timepoints[1])
  } else {
    if(!(is.numeric(originTimepoint))){
      stop("ERROR: originTimepoint must be a numeric value")
    }
    if(originTimepoint >= min(timepoints)){
      stop("ERROR: originTimepoint must be a timepoint before
                 the first timepoint in the timepoints vector")
    }
  }
  
  ##Values lower than 0.1 are considered 0
  fracTable[which(fracTable<0.1)] = 0
  
  ##sanity checks on input data
  .validateInputs(fracTable, parents, cloneLabels)
  
  ##Therapy effect estimation
  if(!is.null(therapyEffect)){
    fracTable <- .getTherapyEffect(fracTable = fracTable,
                                   timepoints = timepoints,
                                   parents=parents,
                                   therapyEffect=therapyEffect)
    
    timepoints <- as.numeric(colnames(fracTable))
  }
  
  
  ##Estimate new timepoints
  if(timepointEstimation){
    # fracTable <- .estimateTimepoints(nestLevels = nestLevels, fracTable=fracTable,
    #                                 parents = parents, timepoints=timepoints,
    #                                 originTimepoint = originTimepoint)
    fracTable <- .estimateTimepoints(nestLevels = nestLevels, fracTable=fracTable,
                                     parents = parents, timepoints=timepoints,
                                     originTimepoint = originTimepoint,
                                     therapyEffect = therapyEffect)
    timepoints <- as.numeric(colnames(fracTable))
  }
  
  ##Recalculate timepoints if both (new timepoints + therapy effect) are estimated
  if(!is.null(therapyEffect) & timepointEstimation){
    timepoints <- .recalculateTimepoints(therapyEffect,timepoints)
    colnames(fracTable) <- timepoints
  }
  
  
  
  
  ##Change the name of the clones to numbers (in fracTable)
  rownames(fracTable)=seq(1:dim(fracTable)[1])
  
  
  
  ##sanity checks on calculated data
  .validateInputs(fracTable, parents, cloneLabels)
  
  ##create the object
  seaObject <- new("seaObject", ytop=list(), ybtm=list(), col=c("NULL"),
                   timepoints=as.numeric(colnames(fracTable)),
                   fracTable=fracTable, parents=parents,
                   nestLevels = nestLevels,
                   cloneLabels=cloneLabels,
                   defaultLabels=defaultLabels,
                   originTimepoint=originTimepoint
  )
  
  ##vector with each clones family
  seaObject <- .cloneFamVec(seaObject)
  
  #set default colors to start
  seaObject <- .setColors(seaObject, col = col)
  
  
  return(seaObject)
}
