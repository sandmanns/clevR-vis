## Please check out our updated version 'clevRvis'

<p align="center">
    <img height="150" src="https://uni-muenster.sciebo.de/s/BMZPcx1ggQGvQOF/download">
</p>

# clevR-vis

clevR-vis provides an extensive set of visualization techniques for clonal evolution. Three types of plots are available: 1) shark plots (basic trees, showing the phylogeny and optionally the cancer cell fraction CCF); 2) dolphin plots (advanced visualization, showing the phylogeny and the development of CCFs over time); 3) plaice plots (novel visualization, shwoing the phylogeny, the development of CCFs and the development of remaining healthy alleles, influenced by bi-allelic events, over time). Moreover, the tool provides algorithms for fully automatic estimation of time points and therapy effect to approximate a tumor's development in the presence of few measured time points. 

## Requirements
To run clevR-vis, you need R (Version 4.1.0 or higher) and shiny.

##  Installation
To install clevR-vis, just download and execute the global.R script. All required packages will be installed, all functions loaded automatically.

## Running clevR-vis
clevR-vis is available as a shiny GUI. You can run the shiny version either by RStudio executing the function `clevRvisShiny()`. Additionally, all functions for classical use in R are available in the R folder:

1. `createSeaObject()`: Create a seaObject

2) `sharkPlot()`: Generate a basic graph visualization of clonal evolution

3) `extSharkplot()`: Generate an extended graph visualization of clonal evolution

4) `dolphinPlot()`: Generate a detailed visualization of clonal evolution

5) `plotWidget()`: Generate a combined basic graph and detailed visualization of clonal evolution

6) `plaicePlot()`: Generate an allele-aware visualization of clonal evolution


## Examplary use of clevR-vis

## createSeaObject()
clevR-vis needs a seaObject for the visualization of clonal evolution by means of any available plot. When generating the seaObject extra time points and/or therapy effect may be estimated.

### Usage
```
createSeaObject(fracTable,parents,timepoints,col=NULL,cloneLabels=NULL,
                originTimepoint=NULL,timepointEstimation=TRUE,
                therapyEffect=NULL)
```

* `fracTable` A numeric matrix containing tumor fraction estimates for all clones at all timepoints.
* `parents` An integer vector specifying parental relationships between clones.
* `timepoints` A numeric vector specifying the timepoints for each column of the matrix.
* `col` (optional) A vector of colors to use when plotting each clone.
* `cloneLabels` (optional) A character vector of names to assign to each clone when plotting a legend.
* `originTimepoint` (optional) Timepoint when the first clone emerges (must be before the first measured timepoint).
* `timepointEstimation` When set to true extra time points will be estimated between measured timepoints and before the first measure timepoint to improve the visualization (default: TRUE).
* `therapyEffect` (optional) A single numeric value indicating the time point when to estimate the effect of therapy or a numerc vector containing two consecutive measured time points, therapy effect timepoint will be in the middle.

### Details

The basis for all plotting functions included in clevRvis is a seaObject. It contains information on the CCFs for all clones, at all measured time points. Additionally, parental information on the clones is included.

Additional time points and therapy effect may be estimated when generating seaObjects.

Timepoint estimation is generally recommended to improve visualization of clonal evolution. When having less timepoints than clones, or many new clones emerging in one single measured timepoint, the extra timepoint estimation is strongly recommended to visualize the clonal evolution properly. If there is only one measured timepoint, timepoint estimation is needed and the timepoint of origin must be manually specified, as there is no way of calculating it.

To visualize the effect of therapy on the clones' CCFs in case of missing measured data, a fully automatic approach for therapy effect estimation is available. When creating the seaObject, a specific timepoint can be defined (between two measured timepoints) or two measured timepoints can be selected (new therapy effect timepoint will be in the middle) for the estimation of the therapy effect.

A seaObject with all relevant slots filled is returned.

#### Time point estimation
For an improved visualization of clonal evolution, enabling time point estimation is recommended.

#### Therapy effect estimation
When generating the seaObject, the effect of therapy can also be estimated. There’s two options to define the therapy effect timepoint:

- The therapyEffect is defined as a single numeric value in between measured timepoints
-> the therapy effect will be estimated at the given time point

- The therapyEffect is defined as a vector containing two measured timepoints
-> the therapy effect will be estimated at the midpoint between them       


```R
##Example data
timepoints <- c(0,50,100)
parents <- c(0,1,1,3,0,5,6)
fracTable <- matrix(c(20,10,0,0,0,0,0,
                      40,20,15,0,30,10,0,
                      50,25,15,10,40,20,15),
                    ncol = length(timepoints))
seaObject <- createSeaObject(fracTable, parents, timepoints,
                             timepointEstimation = FALSE)
```

```R
##seaObject with enabled time point estimation
seaObject_tp <- createSeaObject(fracTable, parents, timepoints,
                                timepointEstimation = TRUE)
```

```R
##seaObject with enabled time point estimation and therapy effect 
##estimation between timepoint 50 and 100
seaObject_te <- createSeaObject(fracTable, parents, timepoints,
                             timepointEstimation = TRUE,
                             therapyEffect = c(50,100))
```


## sharkPlot()
A shark plot shows the basic graph visualization of clonal evolution with nodes representing clones and edges indicating their evolutionary relations.

### Usage
```sharkPlot(seaObject, showLegend, main)```

* `seaObject` A seaObject.
* `showLegend` A boolean indicating whether to show the legend or not (default: FALSE).
* `main` A string corresponding to the plot''s main title.

### Details
A shark plot is the basic approach for visualization: common trees, with nodes representing clones and edges indicating their evolutionary relation. Phylogeny can be directly deduced from these plots.

Shark plots also offer an extension to visualize the changes in CCF along time for each clone. CCFs of each clone (rows) at each timepoint (columns) are shown as points next to the basic shark plot (see `extSharkPlot`).

### Examples
```R
#Basic shark plot showing legend and title
sharkPlot(seaObject_tp, showLegend = TRUE, main = 'Example Shark plot')
```

## extSharkPlot()               
An extended shark plot shows the basic graph visualization of clonal evolution with nodes representing clones and edges indicating their evolutionary relations and additional visualization of CCFs.

### Usage
```extSharkPlot(seaObject, showLegend, main, timepoints, width, interactivePlot)```

* `seaObject` A seaObject.
* `showLegend` A boolean indicating whether to show the legend or not (default: FALSE).
* `main` A string corresponding to the plot's main title (default: NULL).
* `timepoints` By default, all time points available in the seaObject are visualized. Optionally, a selected set of available time points can be chosen.
* `width` An integer value indicating the width of the widget plot (default: 10).
* `interactivePlot` A boolean defining whether the plot should be interactive (default: TRUE; if using this function to export the extended shark plot, e.g. by png(), define interactivePlot = FALSE).

### Details
An extended shark plots consists of two elements:

1. A basic shark plot: common trees, with nodes representing clones and edges indicating their evolutionary relation. Phylogeny can be directly deduced from these plots.

2. Additionally, CCFs of each clone (rows) at each timepoint (columns) are shown as points next to the basic shark plot. The size of each point correlates with the CCF at the corresponding clone and time point.

Both plots are linked in an interactive widget.

### Examples
```R
##extended shark plot, showing CCF as point size only for measured 
###timepoints, legend and title
extSharkPlot(seaObject_tp, timepoints = timepoints, showLegend = TRUE, 
             main = 'Example Extended Shark plot')
```

## dolphinPlot()
Dolphin plots provide a detailed visualization of clonal evolution. Plots show the development of all clones over time (x axis) and their clonal prevalences (y axis).

### Usage
```
dolphinPlot(seaObject, shape, borderCol, pos, vlines, vlineCol, vlab, 
            vlabSize, separateIndependentClones, showLegend, 
            markMeasuredTimepoints, main, mainPos, mainSize, xlab, 
            ylab, pad.left, annotations, annotSize)
```

* `seaObject` A seaObject.
* `shape` The type of shape to construct the plot out of. The options are "spline" and "polygon" (default: "spline"").
* `borderCol` A color for the border line. If "NULL" then no border will be drawn (default: NULL).
* `pos` Plotting position of the clones. Options are "center" or "bottom" (default: "center").
* `vlines` A vector of positions at which to draw vertical lines (default: NULL).
* `vlineCol` A color value for the vertical lines (default: "#6E6E66").
* `vlab` A character vector containing labels for each of the vertical lines (default: NULL).
* `vlabSize` An integer value for the vertical labels size (default: 3).
* `separateIndependentClones` Boolean defining whether independently-arising clones (with parent 0) should be separated by blank space in the plot (default: FALSE).
* `showLegend` A boolean indicating whether to show a legend at the left side of the plot (default: FALSE).
* `markMeasuredTimepoints` A vector of x positions at which to draw triangles on the bottom of the plot (default: NULL).
* `main` A string corresponding to the plot's main title (default: NULL).
* `mainPos` A string defining the title's position. Options are 'left', 'middle' or 'right', always above the plot (default: "middle").
* `mainSize` An integer value defining the size of the title (default: 5).
* `xlab` A string defining the label of the x axis (default: NULL).
* `ylab` A string defining the label of the y axis. Automatically, a vertical line showing 100\% will be plotted (default: NULL).
* `pad.left` The amount of "ramp-up" to the left of the first timepoint. Given as a fraction of the total plot width (default: 0.005).
* `annotations` A data.frame with: columns "x" (x position), "y" (y position), "lab" (annotation text) and "col" (color of the text either black or white) (default: NULL).
* `annotSize` An integer value defining the size of the annotations (default: 3).


### Details
Dolphin plots displays detailed information on clonal evolution, showing the development of all clones over time (x-axis) and their clonal prevalence (y-axis). Information on phylogeny, CCFs and time course characterizing a clonal evolution are jointly visualized in this single plot.

Several basic options for customizing dolphin plots are available, e.g. switching between spline and polygon shape, bottom or central visualization, annotations, separating independent clones, adding vertical lines and labels, changing border and vertical lines colors, etc.

Dolphin plots may be chosen to be plotted along with basic shark plots (see `plotWidget()`).

### Examples

```R
##Dolphin plot, with vertical lines showing all time points, custom y axis 
##label and triangles indicating the measured time points
dolphinPlot(seaObject_tp, showLegend = TRUE, vlines = seaObject@timepoints, 
            vlab = seaObject@timepoints, vlabSize = 2, 
            ylab = 'Cancer cell fractions (CCFs)',
            markMeasuredTimepoints = timepoints)
```

```R
##Dolphin plot polygon shape
dolphinPlot(seaObject_tp, showLegend = TRUE, vlines = timepoints,
            vlab = timepoints, vlabSize = 2, 
            ylab = 'Cancer cell fractions (CCFs)', shape = 'polygon')
```

```R
##Dolphin plot bottom layout and separated independent clones
dolphinPlot(seaObject_tp, showLegend = F,  vlines = timepoints,
            vlab = timepoints, vlabSize = 2, 
            ylab = 'Cancer cell fractions (CCFs)', 
            separateIndependentClones = TRUE, pos = 'bottom')
```

To add annotations to clones in the plot, a table must be created containing the following columns:
* `x` x position of the annotation text
* `y` y position of the annotation text
* `col` color of the text (options are ‘black’ or ‘white’)
* `lab` text of the annotation  

```R
##Dolphin plot with annotations
annotsTable <- data.frame(x = c(50,75), y = c(15,50), 
                          col = c('black', 'white'), 
                          lab = c('Annotation clone 5', 'Annotation clone 2'))
dolphinPlot(seaObject_tp, showLegend = TRUE, main = 'Example Dolphin Plot', 
            vlines = timepoints, vlab = timepoints, vlabSize = 2, 
            ylab = 'Cancer cell fraction', annotations = annotsTable, 
            pos = 'bottom', separateIndependentClones = TRUE)
```


```R
##Dolphin plot with enabled therapy effect estimation
##vertical lines show all time points, customized y axis label,
##triangles indicate the measured time points
dolphinPlot(seaObject_te, showLegend = TRUE, vlines = seaObject@timepoints, 
            vlab = seaObject@timepoints, vlabSize = 2, 
            ylab = 'Cancer cell fractions (CCFs)',
            markMeasuredTimepoints = timepoints)
```

## plotWidget()
Given a sea object containing layout information, a shark and dolphin plot can be plotted together - linked and interactive.

### Usage
```
plotWidget(seaObject_tp, shark, dolphin, shape, borderCol, vlines, 
           vlineCol, vlab, vlabSize, pos, separateIndependentClones,
           showLegend, markMeasuredTimepoints, downloadWidget, 
           mainDph, mainPosDph, mainSizeDph, mainShk, xlab, ylab, 
           pad.left, annotations, width, height)
```

* `seaObject` A seaObject.
* `shark` A boolean defining whether or not to draw a shark plot (default: TRUE).
* `dolphin` A boolean defining whether or not to draw a dolphin plot (default: TRUE).
* `shape` The type of shape to construct the plot out of. The options are "spline" and "polygon" (default: "spline"").
* `borderCol` A color for the border line. If "NULL" then no border will be drawn (default: NULL).
* `pos` Plotting position of the clones. Options are "center", "bottom" or "top" (default: "center").
* `vlines` A vector of positions at which to draw vertical lines (default: NULL).
* `vlineCol` A color value for the vertical lines (default: "#6E6E66").
* `vlab` A character vector containing labels for each of the vertical lines (default: NULL).
* `vlabSize` An integer value for the vertical labels size (default: 3).
* `separateIndependentClones` Boolean defining whether independently-arising clones (with parent 0) should be separated by blank space in the plot (default: FALSE).
* `showLegend` A boolean indicating whether to show a legend at the left side of the plot (default: FALSE).
* `markMeasuredTimepoints` A vector of x positions at which to draw triangles on the bottom of the plot (default: NULL).
* `downloadWidget` File to safe HTML to (default: NULL).
* `mainDph` A string corresponding to the dolphin plot's main title (default: NULL).
* `mainPosDph` A string defining the dolphin plot's title position. Options are 'left', 'middle' or 'right', always above the plot (default: "middle").
* `mainSizeDph` An integer value defining the size of the dolphin plot's title (default: 5).
* `mainShk` A string corresponding to the shark plot's main title (default: NULL).
* `xlab` A string defining the label of the x axis (default: NULL).
* `ylab` A string defining the label of the y axis. Automatically, a vertical line showing 100\% will be plotted (default: NULL).
* `pad.left` The amount of "ramp-up" to the left of the first timepoint. Given as a fraction of the total plot width (default: 0.005).
* `annotations` A data.frame with: columns "x" (x position), "y" (y position), "lab" (annotation text) and "col" (color of the text either black or white) (default: NULL).
* `width` An integer value indicating the with of the output widget (default: 12).
* `height` An integer value indicating the height of the output widget (default: 9).

### Details
Dolphin plots may be chosen to be plotted along with basic shark plots (for details see `dolphinPlot()` and `sharkPlot()`. Both plots are internally connected. By hovering on one of the clones, it is automatically highlighted in both, shark and dolphin plot.

Important note: extended shark plots and dolphin plots can NOT be visualized together.

### Examples

```R
##Basic shark plot linked to dolphin plot
plotWidget(seaObject_tp, showLegend = TRUE, vlines = timepoints,
            vlab = timepoints, vlabSize = 2, ylab = 'Cancer cell fraction',
            separateIndependentClones = TRUE)
```

## plaicePlot()
Plaice plots provide an allele-aware visualization of clonal evolution. Plots show the development of all clones over time (x axis) and their clonal prevalences (y axis), and the ratio of remaining healthy alleles (lower plaice).

### Usage
```
plaicePlot(seaObject, shape, borderCol, vlines, vlineCol, vlab, vlabSize,
            separateIndependentClones, clonesToFill, showLegend,
            markMeasuredTimepoints, main, mainPos, mainSize, xlab, ylab,
            pad.left, annotations, annotationsSize, interactivePlot)
```

* `seaObject` A seaObject.
* `shape` The type of shape to construct the plot out of. The options are "spline" and "polygon" (default: "spline"").
* `borderCol` A color for the border line. If "NULL" then no border will be drawn (default: "black").
* `vlines` A vector of positions at which to draw vertical lines (default: NULL).
* `vlineCol` A color value for the vertical lines (default: "#6E6E66").
* `vlab` A character vector containing labels for each of the vertical lines (default: NULL).
* `vlabSize` An integer value for the vertical labels size (default: 3).
* `separateIndependentClones` Boolean defining whether independently-arising clones (with parent 0) should be separated by blank space in the plot (default: FALSE).
* `clonesToFill` An integer vector with the index of the clone's color to fill each clone. For example: clonesToFill <- c(0,0,0,2,0,0) clone 4 (and its children) will be filled with clone 2 color (default: NULL).
* `showLegend` A boolean indicating whether to show a legend at the left side of the plot (default: FALSE).
* `markMeasuredTimepoints` A vector of x positions at which to draw triangles on the bottom of the plot (default: NULL).
* `main` A string corresponding to the plot's main title (default: NULL).
* `mainPos` A string defining the title's position. Options are 'left', 'middle' or 'right', always above the plot (default: "middle").
* `mainSize` An integer value defining the size of the title (default: 5).
* `xlab` A string defining the label of the x axis (default: NULL).
* `ylab` A boolean defining whether or not to show the default y axis labels (default: FALSE).
* `pad.left` The amount of "ramp-up" to the left of the first timepoint. Given as a fraction of the total plot width (default: 0.005).
* `annotations` A data.frame with: columns "x" (x position), "y" (y position), "lab" (annotation text) and "col" (color of the text either black or white) (default: NULL).
* `annotationsSize` An integer value defining the size of the annotations (default: 3).
* `interactivePlot` A boolean defining whether the plot should be interactive (default: TRUE; if using this function to export the plaice plot, e.g. by png(), define interactivePlot = FALSE).

### Details

Plaice plots represent an option for visualizing clonal evolution on allelic-level. The upper half of the plot (=plaice) shows a common dolphin plot in bottom visualization. The lower plaice shows the percentage of healthy alleles. Clones that are characterized by bi-allelic events affecting one or more genes may be colored accordingly.
Coloring of clones in the lower plaice is interpreted as follows:

- An uncolored clone indicates that at least one copy of a healthy allele is present for all genes.
- A colored clone indicates that a bi-allelic event took place. As a consequence, no healthy allele of the gene(s) affected by the bi-allelic event remains.
  - If clone 1 carries a point mutation in TP53 and clone 2 carries an additional del17p (affecting the remaining healthy allele), clone 2 in the lower plaice should be colored in the same hue as clone 1 in the upper plaice, which is originally charaterized by mutation in TP53, indicating that the double-hit event affected TP53.
  - If clone 1 carries a deletion of 17p and clone 2 carries an additional point mutation in TP53 (affecting the remaining healthy allele), clone 2 in the lower plaice should be colored in the same hue as clone 2 in the upper plaice, which is originally charaterized by mutation in TP53, indicating that the double-hit event affected TP53.
  - If clone 2 carries two point mutations in TP53 affecting different alleles, clone 2 in the lower plaice should be colored in the same hue as clone 2 in the upper plaice, indicating that the double-hit event affected TP53.
  - If clone 2 carries a variant affecting the X chromosome of a male subject, leading to a loss of the only available healthy allele, clone 2 in the lower plaice should be colored in the same hue as clone 2 in the upper plaice, indicating he hemizygous variant affecting a gene on the X chromosome.             

### Examples

```R
#Plaice plot when all genes have at least one healthy copy
plaicePlot(seaObject_tp, showLegend = T,  vlines = timepoints,
            vlab = timepoints, vlabSize = 4, ylab = T, 
            separateIndependentClones = TRUE)
```

```R
#Plaice plot showing bi-allelic events + annotations
annotsTable <- data.frame(x = c(24,55), y = c(-40,-5), 
                          col = c('black', 'white'), 
                          lab = c('TP53', 'UBA1'))
plaicePlot(seaObject_tp, showLegend = T,  vlines = timepoints,
            vlab = timepoints, vlabSize = 4, ylab = T, 
            separateIndependentClones = TRUE, clonesToFill = c(0,0,1,0,0,6,0),
           annotations = annotsTable)
```

```R
##Plaice plot with enabled therapy effect estimation, 
##all genes are assumed to have at least one healthy copy
plaicePlot(seaObject_te, showLegend = T,  vlines = timepoints,
            vlab = timepoints, vlabSize = 4, ylab = T, 
            separateIndependentClones = TRUE)
```


## clevRvisShiny()
clevR-vis provides an additional shiny GUI to perform all analyses interactively.

### Usage
```
clevRvisShiny()
```

### Examples
Two exemplary input files, containing the cancer cell fractions for simulated clonal evolution, can be found in the Example folder. 

For Example1.xlsx the suggested parental relations are:

|Clone|Parent|
|-----|------|
|clone A|normal cells|
|clone B|clone A|
|clone C|clone B|
|clone D|clone C|


For **Example2.xlsx** the suggested parental relations are: 

|Clone|Parent|
|-----|------|
|clone A|normal cells|
|clone B|clone A|
|clone C|clone B|
|clone D|clone C|
|clone E|clone B|


## Reporting errors / Requesting features

If you should experience any errors using clevR-vis, or you are looking for some additional features, feel free to open an issue or to contact Sarah Sandmann ( sarah.sandmann@uni-muenster.de ).
