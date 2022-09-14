#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(colourpicker)
library(DT)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    ##helpers
    observe_helpers(session = session)
    
    # First Pop Up ###############################
    observeEvent("", {
        showModal(modalDialog(
            includeHTML("intro_text.html"),
            easyClose = TRUE,
            footer = tagList(
                actionButton(inputId = "modalButton", 
                             label = "UNDERSTOOD",
                             icon = icon("thumbs-up"),
                             color = "success")
            )
        ))
    })
    
    observeEvent(input$modalButton,{
        removeModal()
    })
    ######
    
    
    ##Process ccf table
    fracTableR <- reactive({
        req(input$fileCCF)
        file <- input$fileCCF
        ext <- tools::file_ext(file$datapath)
        
        if (ext == 'xlsx' | ext == 'xls') {
            if (input$tpCols){
                fracTable <- as.matrix(read_excel(file$datapath,col_names = TRUE))
            } else {
                fracTable <- as.matrix(read_excel(file$datapath,col_names = FALSE))
            }
            
        }else if (ext == 'csv'){
            if (input$tpCols){
                fracTable <- as.matrix(read_csv(file$datapath))
            } else {
                fracTable <- as.matrix(read_csv(file$datapath, col_names = FALSE))
            }
            
        }else {
            stop(paste('File must be an excel (.xls / .xlsx) or a CSV (.csv)',
                       file$datapath, 'is not a valid file.'))
        }
        
        if (input$labRows){
            rown <- fracTable[,1]
            coln <- colnames(fracTable)[2:ncol(fracTable)]
            if(dim(fracTable)[1]==1){
                fracTable <- dplyr::as_data_frame(matrix(fracTable[,2:ncol(fracTable)], ncol = length(coln)))
                fracTable <- apply(fracTable, 2, as.numeric)
                fracTable <- dplyr::as_data_frame(matrix(fracTable, ncol = length(fracTable)))
                
            } else {
                fracTable <- dplyr::as_data_frame(fracTable[,2:ncol(fracTable)])
                fracTable <- apply(fracTable, 2, as.numeric)
                fracTable <- round(fracTable, 2)
                
            }
            suppressWarnings(rownames(fracTable) <- rown)
            colnames(fracTable) <- coln
        }
        
        return(fracTable)
        
    }) #end of fracTable processing
    
    ##Show ccf table OUTPUT
    output$OGccfTable <- renderDataTable({
        df <- fracTableR()
        if (!input$tpCols){
            tps <- getTimepoints()
            if (length(tps) < ncol(df)){
                colnames(df) <- c(NULL,tps)
            } else {
                colnames(df) <- tps
            }
        }
        if (!input$labRows){
            cloneLabs <- getCloneLabs()
            rownames(df) <- cloneLabs
        }
        
        datatable(round(df,2), options = list(scrollX=TRUE, scrollCollapse=TRUE))
    },
    extensions = 'Buttons', options = list(
        dom = 'Blfrtip',  buttons = list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel'),
            text = 'Download'
        )))
    )
    
    ##Ask for input table
    output$inTableAsk <- renderText({
        if (!is.null(input$fileCCF)){
            t <- NULL
        } else {
            t <- 'Please upload an input Cancer Cell Fraction table'
        }
    })
    
    ##Create inputs for specifying parental relations 
    output$parents <- renderUI({
        df <- fracTableR()
        nclones <- nrow(df) 
        cloneLabs <- getCloneLabs()
        
        lapply(1:(nclones), function(i) {
            list(pickerInput(paste0('p', i), label = cloneLabs[i], 
                             choices = c('Normal cell',cloneLabs)[-(i+1)], 
                             width = 'fit')
            )
        }) #end of lapply
    }) # end of renderUI
    
    ##Get clone labels
    getCloneLabs <- reactive({
        if (input$labRows){ #first col is clone labs
            cloneLabs <- rownames(fracTableR())
        } else if (input$cloneLabs){ # default clone labs
            cloneLabs <- paste('Clone:',as.character(1:nrow(fracTableR())))
        } else { #not default clone labs
            cloneLabs <- strsplit(input$cloneLabsValues, ",")[[1]]
            
        }
        
        return(cloneLabs)
    })
    
    ##Get Timepoints
    getTimepoints <- reactive({
        df <- fracTableR()
        if (input$tpCols){ #first row is tps
            if (colnames(df)[1]=='...1'){
                tp <- colnames(df)[2:ncol(df)]
            } else {
                tp <- colnames(df)
            }
        } else if (input$dfltTP){ #default tps
            tp <- seq(1,(ncol(df)))
            
        } else { #specified tps
            tp <- as.numeric(strsplit(input$timepoints, ",")[[1]])
            }
        return(as.numeric(tp))
    })
    

    
    ##Get parents vector
    getParents <- eventReactive(input$subInputs,{
        df <- fracTableR()
        cloneLabs <- getCloneLabs()
        p_vec <- c()
        for (j in seq_len(length(cloneLabs))) {
            p <- input[[paste0("p",j)]]
            if (p == 'Normal cell') {
                p_vec <- c(p_vec,0)
            } else {
                p_id <- which(cloneLabs == p)
                p_vec <- c(p_vec, p_id)
            }
        }
        return(p_vec)
    })
    
    #get reactive vector with warning messages
    validateInputs <- eventReactive(input$subInputs, {
        fracTable <- fracTableR()
        parents <- getParents()
        cloneLabels <- getCloneLabs()
        warns <- c()
        clones =  1:dim(fracTable)[1]
        timepts = 1:dim(fracTable)[2]
        
        ##no cluster can go from present to absent and then back
        ##difference in clusters cannot go from present to absent and then back
        for(clone in clones){
            startedClone <- FALSE
            endedClone <- FALSE
            startedDiff <- FALSE
            endedDiff <- FALSE
            
            for(timept in timepts){
                ##check individual clones
                if(fracTable[clone,timept] > 0){
                    if(startedClone & endedClone){
                        stop(paste("Clone",clone,"goes from present to absent (fraction=0) 
                     and then back to present."))
                    }
                    startedClone=TRUE
                } else {
                    if(startedClone){
                        endedClone=TRUE
                    }
                }
                
                ##check difference in clusters
                diffr <- fracTable[clone,timept] - sum(fracTable[which(parents==clone),timept])
                if( diffr > 0){
                    if(startedDiff & endedDiff){
                        stop(paste("The difference between",clone,"and its subclones 
                    goes from present to absent (difference=0) and then back to 
                    present. Subclones can't have the same CCF as the parent 
                    (difference = 0) and then have less CCF again."))
                    }
                    startedDiff=TRUE
                } else {
                    if(startedDiff){
                        endedDiff=TRUE
                    }
                }
            }
        }
        
        ##clusters of entirely zero get a warning
        if(length(which(rowSums(fracTable) == 0)) > 0){
            warns <- c(warns, "At least one cluster has fraction zero at all timepoints. 
            It will not be displayed")
        }
        
        ##make sure that each timepoint doesn't sum to more than the parental value 
        #at a given nest level (or 100% for level 0)
        for(timept in timepts){
            neighbors <- which(parents == 0)
            if(sum(fracTable[neighbors,timept]) > 100){
                warns <- c(warns, paste("clones with same nest level cannot have values that sum to 
                 more than 100%: Problem is in clusters ",
                                        paste(neighbors,collapse=",")))
            }
            
            for(i in unique(parents)){
                if(i > 0){
                    neighbors <- which(parents==i)
                    if(sum(fracTable[neighbors,timept]) > fracTable[parents[neighbors[1]],
                                                                    timept]){
                        warns <- c(warns, paste("clones with same parent cannot have values that sum to 
                     more than the percentage of the parent: Problem is in 
                     clusters ",paste(neighbors,collapse=","),"at timepoint",
                                                timept))
                        
                    }
                }
            }
        }
        
        ##ensure that the number of clone labels is equal to the number of clones
        if(length(cloneLabels) != nrow(fracTable)){
            warns <- c(warns, paste("number of cloneLabels provided must be equal to the 
               number of clones"))
        }
        return(unique(warns))
    })
    
    ##Success / warning messages
    observeEvent(input$subInputs,{
        warns <- validateInputs()
        if (length(warns) == 0){
            sendSweetAlert(
                session = session,
                title = "Success!!",
                text = "Parental relations have been successfully defined",
                type = "success"
            )
        } else {
            sendSweetAlert(
                session = session,
                title = "WARNING...",
                text = warns,
                type = "error"
            )
        }
    })
    
    ##Update default original timepoint (manual choice)
    observeEvent(input$subInputs,{
        timepoints <- getTimepoints()
        if (length(timepoints)==1){
            val <- -30
        } else {
            val <- timepoints[1] - (timepoints[2] - timepoints[1])
        }
        updateNumericInput(session = session,
                           inputId = 'ogTP',
                           max = min(timepoints)-1,
                           value = val)
    })
    
    ##Update slider to select measured timepoints for therapy estimation
    observeEvent(input$subInputs,{
        timepoints <- getTimepoints()
        updateSliderTextInput(session = session,
                              inputId = 'MtpThp',
                              choices = timepoints,
                              selected = c(timepoints[1],timepoints[2]))
    })
    
    ##Update slider to select specific timepoint for therapy estimation
    observeEvent(input$subInputs,{
        timepoints <- getTimepoints()
        updateSliderInput(session = session, inputId = 'tpThp',
                          min = min(timepoints),
                          max = max(timepoints),
                          value = 0,
                          step = 0.5)
    })

    #check thp eff
    output$thpEf <- renderText({
        if(input$therapyEffect){
            if (input$thpEf_opt == 1){
                therapyEffect <- input$tpThp
            } else if (input$thpEf_opt == 2){
                therapyEffect <- as.numeric(input$MtpThp)
            }
        }
           
        return(c(therapyEffect,class(therapyEffect)))
    })
    
    ##Validate sea Object
    validateSeaObjectFilters <- eventReactive(input$submitSea, {
       
        tps <- getTimepoints()
        warns <- c()
        if(input$therapyEffect){
            if (input$thpEf_opt == 1){ ##thp effect is specified timepoint
                if (input$tpThp %in% tps){
                    warns <- c(warns, 'Specific timepoint defined for therapy effect
                               can not be a measured timepoint')
                }
            } else { ##thp eff defined as two measured tps
                therapyEffectMtps <- as.numeric(input$MtpThp)
                if (which(tps==therapyEffectMtps[2])-which(tps==therapyEffectMtps[1])>1){
                    warns <- c(warns, 'Measured Timepoints selected for therapy
                           effect must be contiguous, no other measured timepoint
                           must be in between.')
                } else if (which(tps==therapyEffectMtps[2])-which(tps==therapyEffectMtps[1])==0){
                    warns <- c(warns, 'Measured Timepoints selected for therapy
                           effect can not be the same one. To select a specific
                           timepoint click on "Specific Timepoint" when choosing
                           the type of Therapy Effect Estimation.')
                }
            }
        }

        if(length(tps)==1 & !input$tpEstim){
            warns <- c(warns, 'To visualize clonal evolution from single timepoint
             extra timepoints estimation must be selected.')
        } 
        if (length(tps)==1 & !input$customOgTP){
            warns <- c(warns, "To visualize clonal evolution from a single timepoint
             a timepoint of origin must be manually specified.")
        }
        return(warns)
    })

    ##check inputs are there (and correct) before clicking submit sea object
    observeEvent(input$submitSea,{
        err <- validateSeaObjectFilters()
        if (!input$subInputs){
            sendSweetAlert(
                session = session,
                title = "ERROR!",
                text = 'Submit inputs before clicking submit on seaObject filters',
                type = "error"
            )
        } else if (length(err)>0){
            sendSweetAlert(
                session = session,
                title = "ERROR!",
                text = err,
                type = "error"
            )
        } else {
            sendSweetAlert(
                session = session,
                title = "SUCCESS!!",
                text = 'All in order',
                type = "success"
            )
        }
    })
    
    #reactive object SeaObject will update every time we click submit
    seaObjectR <- eventReactive(input$submitSea,{
        err <- validateSeaObjectFilters()
        fracTable <- as.matrix(fracTableR())
        timepoints <- getTimepoints()
        cloneLabels <- getCloneLabs()
        parents <- getParents()
        if(length(err)==0){
            if(input$therapyEffect){
                if (input$thpEf_opt == 1){
                    therapyEffect <- input$tpThp
                } else if (input$thpEf_opt == 2){
                    therapyEffect <- as.numeric(input$MtpThp)
                }

            }else{
                therapyEffect <- NULL}

            if(input$customOgTP){
                ogTimepoint <- input$ogTP
            }else{
                ogTimepoint <- NULL
            }


            seaObject <- createSeaObject(fracTable = fracTable, timepoints = timepoints,
                                         parents = parents,
                                         timepointEstimation = input$tpEstim,
                                         therapyEffect = therapyEffect,
                                         cloneLabels = cloneLabels,
                                         originTimepoint = ogTimepoint)
        }


        return(seaObject)

    }) #end of create SeaObject

    
    ##TABLE WITH CCF AFTER SEAOBJECT IS CREATED
    output$ccfTable <- renderDataTable({
        seaObject <- seaObjectR()
        df <- dplyr::as_data_frame(seaObject@fracTable)
        parents <- getParents()
        df <- df %>% add_column(parents)
        cloneLabs <- getCloneLabs()
        rownames(df) <- cloneLabs
        datatable(df, options = list(scrollX=TRUE, scrollCollapse=TRUE))
        
    },
    rownames = TRUE,
    extensions = 'Buttons', options = list(
        dom = 'Blfrtip',  buttons = list('copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel'),
            text = 'Download'
        )))
    )
    
    ####PLOTS####
    ##INTERACTIVE PLOTS
    output$intPlot <- renderGirafe({
        req(input$shark | input$dolphin )
        seaObject <- seaObjectR()
        timepoints <- getTimepoints()
        if(input$dolphin){
            ##Vertical Lines and Labels
            vlines <- c()
            vlabs <- c()
            symbs <- c()
            if('Measured timepoints' %in% input$vlines){
                vlines <- c(vlines,timepoints)
                if('Measured timepoints' %in% input$vlab){
                    vlabs <- c(vlabs, timepoints)
                }
            }
            if('Measured timepoints' %in% input$symbols){
                symbs <- c(symbs, timepoints)
            }
            if('Estimated timepoints' %in% input$vlines){
                estimatedTimepoints <- seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                vlines <- c(vlines, estimatedTimepoints)
                if('Estimated timepoints' %in% input$vlab){
                    vlabs <- c(vlabs, estimatedTimepoints)
                }
            }
            if('Estimated timepoints' %in% input$symbols){
                estimatedTimepoints <- seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                symbs <- c(symbs, estimatedTimepoints)
            }
            if('Custom' %in% input$vlines){
                customVlines <- as.numeric(strsplit(input$customVlines, ",")[[1]])
                vlines <- c(vlines,customVlines)
            }
            if('Custom' %in% input$vlab){
                customVlabs <- strsplit(input$customVlabs, ",")[[1]] 
                vlabs <- c(vlabs, customVlabs)
            }
            if('Custom' %in% input$symbols){
                customSymbs <- as.numeric(strsplit(input$customSymbols, ",")[[1]])
                symbs <- c(symbs,customSymbs)
            }
            if(input$axisLab){
                if(length(input$xlab)>0){
                    xlab <- input$xlab
                }else{ xlab <- NULL}
                if(length(input$ylab)>0){
                    ylab <- input$ylab
                }else{ ylab <- NULL}
            }else{
                xlab <- NULL
                ylab <- NULL
            }
        }
        if(input$legendDol | input$legendShk){
            legend <- TRUE
        }else {legend <- FALSE}
        
        if (input$addAnnot){
            annotsTbl <- getAnnots()
        } else {annotsTbl <- NULL}

        if (input$extendedShk & !input$dolphin){
            tps <- c()
            if('Measured timepoints' %in% input$showTps){
                tps <- c(tps,timepoints)
            }
            if('Estimated timepoints' %in% input$showTps){
                estimatedTimepoints <- seaObject@timepoints[!seaObject@timepoints %in% timepoints]
                tps <- c(tps, estimatedTimepoints)
            } 
            if('Custom' %in% input$showTps){
                customTps <- as.numeric(strsplit(input$customShowTps, ",")[[1]])
                tps <- c(tps,customTps)
            }
            suppressWarnings(extSharkPlot(seaObject, 
                                         showLegend = input$legendShk, 
                                         main = input$mainShk,
                                         timepoints = tps), 
                             classes = 'warnings')
        } else {
            plotWidget(seaObject, dolphin = input$dolphin, shark = input$shark, 
                   vlines = vlines, mainShk = input$mainShk, showLegend = legend, 
                   shape = input$shape, borderCol = input$borderCol,
                   vlineCol=input$vlineCol, vlab=vlabs, 
                   markMeasuredTimepoints = symbs, pos = input$pos, 
                   separateIndependentClones = input$sepIndCl,
                   mainDph = input$mainDol, vlabSize = input$vlabSize,
                   xlab = xlab, ylab = ylab, annotations = annotsTbl,
                   pad.left = input$pad)
        }
    }) 

    ##Create inputs for specifying clones to fill in plaiceplot 
    output$fillVec <- renderUI({
        cloneLabs <- getCloneLabs()
        nclones <- length(cloneLabs) 
        
        lapply(1:(nclones), function(i) {
            list(pickerInput(paste0('color', i), label = cloneLabs[i], 
                             choices = c('Empty',cloneLabs), 
                             width = 'fit')
            )
        }) #end of lapply
    }) # end of renderUI
    
    ##Get clones to fill vector
    getClonesToFill <- reactive({
        req(input$fillClones)
        cloneLabs <- getCloneLabs()
        f_vec <- c()
        for (j in seq_len(length(cloneLabs))) {
            c <- input[[paste0("color",j)]]
            if (c == 'Empty') {
                f_vec <- c(f_vec,0)
            } else {
                f_vec <- c(f_vec,which(cloneLabs == c))
            }
        }
        return(f_vec)
    })
    

    ##PLAICEPLOT 
    output$plaicePlot <- renderGirafe({
        req(input$plaice)
        seaObject <- seaObjectR()
        timepoints <- getTimepoints()
        estimatedTimepoints <- seaObject@timepoints[!seaObject@timepoints %in% timepoints]
        
        ##vector clones to fill
        if (input$fillClones){
            fillClones <- getClonesToFill()
        } else {
            fillClones <-rep(0,length(seaObject@col))
        }
        ##annotations
        if (input$addAnnot){
            annotsTbl <- getAnnots()
        } else {annotsTbl <- NULL}
        
        ##Vertical Lines, Labels and symbols
        vlines <- c()
        vlabs <- c()
        symbs <- c()
        if('Measured timepoints' %in% input$vlinesPlc){
            vlines <- c(vlines,timepoints)
        }
        if('Measured timepoints' %in% input$vlabPlc){
            vlabs <- c(vlabs, timepoints)
        }
        if('Measured timepoints' %in% input$symbolsPlc){
            symbs <- c(symbs, timepoints)
        }
        if('Estimated timepoints' %in% input$vlinesPlc){
            vlines <- c(vlines, estimatedTimepoints)
        }
        if('Estimated timepoints' %in% input$vlabPlc){
            vlabs <- c(vlabs, estimatedTimepoints)
        }
        if('Estimated timepoints' %in% input$symbolsPlc){
            symbs <- c(symbs, estimatedTimepoints)
        }
        if('Custom' %in% input$vlinesPlc){
            customVlines <- as.numeric(strsplit(input$customVlinesPlc, ",")[[1]])
            vlines <- c(vlines,customVlines)
        }
        if('Custom' %in% input$vlabPlc){
            customVlabs <- strsplit(input$customVlabsPlc, ",")[[1]] 
            vlabs <- c(vlabs, customVlabs)
        }
        if('Custom' %in% input$symbolsPlc){
            customSymbs <- as.numeric(strsplit(input$customSymbolsPlc, ",")[[1]])
            symbs <- c(symbs,customSymbs)
        }
        
        plaicePlot(seaObject, clonesToFill = fillClones, shape = input$shapePlc, 
                   borderCol = input$borderColPlc, vlines=vlines, 
                   vlineCol = input$vlineColPlc, vlab=vlabs, 
                   vlabSize = input$vlabSizePlc, annotations=annotsTbl,
                   separateIndependentClones = input$sepIndClPlc,
                   showLegend = input$legendPlc, markMeasuredTimepoints = symbs,
                   main = input$mainPlc, ylab = input$axisLabPlc)
    }) 
    
    
        
    ##Create inputs for annotations
    output$annot <- renderMenu({
        req(input$submitSea & req(input$addAnnot))
        tps <- seaObjectR()@timepoints
        cloneLabs <- getCloneLabs()
        nclones <- length(cloneLabs)
        sidebarMenu(
            lapply(1:(nclones), function(i) {
                list(menuItem(paste(cloneLabs[i]),
                    ##Annotations text
                     textInput(paste0('annot',i),  
                               paste('Annotations',cloneLabs[i])),
                    ##Color
                     switchInput(paste0("col",i), "Text color", labelWidth = "80px",
                                 onLabel = 'White', offLabel = 'Black',
                                 onStatus = 'default'
                     ),
                    ##X position
                     sliderInput(paste0('x', i), label = 'X position',
                                 max = round(max(tps),0)+((max(tps)-min(tps))/2),
                                 min = round(min(tps),0)-((max(tps)-min(tps))/2),
                                 value = 0, step=(max(tps)-min(tps))*0.05
                     ),
                    ##Y position
                     sliderInput(paste0('y', i), label = 'Y position',
                                 max = 110,
                                 min = -10,
                                 value = 50, step=1)

                     )

                )
            }) #end of lapply
        )#end of sidebarMenu
    }) # end of renderMenu
    
    ##Generate the annotations table
    getAnnots <- reactive({
        
        cloneLabs <- getCloneLabs()
        x_vec <- c()
        y_vec <- c()
        annot_vec <- c()
        col_vec <- c()
        for (j in seq_len(length(cloneLabs))) {
            x_vec <- c(x_vec, input[[paste0("x",j)]]) 
            y_vec <- c(y_vec, input[[paste0("y",j)]]) 
            annot_vec <- c(annot_vec, input[[paste0("annot",j)]]) 
            col_vec <- c(col_vec, input[[paste0('col',j)]])

        }
        annot_df <- data.frame(x = x_vec, y = y_vec, lab = annot_vec,
                               col = col_vec)
        
        return(annot_df)
    })
  
    
    session$onSessionEnded(function() {
        stopApp()
    })  
})

