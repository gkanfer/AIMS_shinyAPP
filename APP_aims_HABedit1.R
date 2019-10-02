rm(list=ls())
library("shiny")
library("EBImage")
library(outliers)
library("shinycssloaders")
library("yaImpute")
library("shinyDND")
library(shinyDirectoryInput)
library(e1071)
library("ROCR", lib.loc="~/Library/R/3.4/library")
library("ggbiplot", lib.loc="~/Library/R/3.5/library")
library("dragulaR")
library("raster", lib.loc="~/Library/R/3.5/library")
library("leaflet", lib.loc="~/Library/R/3.5/library")
library("shinyjs", lib.loc="~/Library/R/3.5/library")

#setwd("~/Desktop/Gil_LabWork/AIMS/Shiny/011619/")

ui <- navbarPage("AIMS platform",
                 tabPanel("Image Analysis",
                          sidebarLayout(
                            sidebarPanel(
                              h4("Channel 1"),
                              fileInput("images", "Select image"),
                              shinyjs::useShinyjs(),
                              id="side-panel",
                              actionButton("reset_input","Reset inputs"),
                              #textOutput(outputId = "file_name"),
                              #sliderInput("size","Image size:",1,1000,1,step=250),  ###choose width and heights
                              sliderInput("wh","threshold:",1,100,46,step=1), 
                              sliderInput("intensity", "Choose image intensity:",1,1000,21,step=1),
                              sliderInput("gm","Gamma DAPI:",0.0001,0.1,0.002,step=0.001),  
                              sliderInput("filter","Detect nuclei edges:",1,99,13,step=2), 
                              sliderInput("peri","Remove small objects:",1,500,30,step=1) 
                            ),
                            mainPanel(
                              tabsetPanel(
                                #tabPanel("Static raster dapi", plotOutput("raster")),
                                tabPanel("Nucleus channel", displayOutput("dapi")),
                                tabPanel("Nucleus outline", displayOutput("dapi_outline_seg")),
                                tabPanel("Nucleus segmentation browser", displayOutput("widget1")),
                                tabPanel("Nucleus segmentation static", withSpinner(displayOutput("final")))
                              ))),
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("GFP_intensity", "Choose image intensity:",1,1000,2,step=1),
                              sliderInput("filter_GFP", "Detect cell edges:",1,99,21,step=2),
                              sliderInput("wh_GFP","GFP threshold:",1,100,7,step=1),
                              sliderInput("gm_GFP","Gamma GFP:",0.0001,0.1,0.012,step=0.001), 
                              sliderInput("global","Detect global edges",0.1,20,0.2,step=0.1), 
                              sliderInput("peri_GFP","Clean small objects",1,1000,50,step=5),  
                              sliderInput("size_GFP","Clean large objects",5000,50000,5000,step=100),position="left"
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Static raster GFP", plotOutput("raster_GFP")),
                                tabPanel("Cell outline",withSpinner(displayOutput("outline_seg"))),
                                tabPanel("Cell outline final",withSpinner(displayOutput("outline_size")))
                              ),
                              div("Once the file has finished uploading, press the button below:",
                                  actionButton(inputId = "button", label="save the images per cell")),
                              div("Number of cells",textOutput(outputId = "cell_number_print"))
                            ))),
                 tabPanel("Classification of Postive Objects",
                          fluidRow(style = "margin: 15px;",
                                   column(12,
                                          h4("Choose cells"),
                                          sliderInput("intensity_classification", "Image intensity:",0.1,500,1,step=0.1),
                                          actionButton(inputId = "button_postive", label="Image update"),
                                          downloadButton("postive_training", "Download positive example cells")
                                   )),
                          fluidRow(style = "margin: 15px;",
                                   column(12,
                                          h4("Segmented image, Choose positive examples"),
                                          plotOutput("postive_image", width="1000px", height="1000px",click="plot_click"))
                                   #verbatimTextOutput("txt")
                          )
                 )
                 
                 ,
                 tabPanel("Classification of Negative objects",
                          fluidRow(style = "margin: 15px;",
                                   column(12,
                                          h4("Choose cells"),
                                          sliderInput("intensity_classification.negtive", "Image intensity:",0.1,500,1,step=0.1),
                                          actionButton(inputId = "button_negative", label="Image update"),
                                          downloadButton("negative_training", "Download negative example cells")
                                   )),
                          fluidRow(style = "margin: 15px;",
                                   column(12,
                                          h4("Segmented Image, Choose negative examples"),
                                          plotOutput("negative_image", width="1000px", height="1000px",click="plot_click_negative")
                                          #verbatimTextOutput("txt")
                                   )
                          ))
                 
                 ,
                 tabPanel("Create a Model",
                          directoryInput("directory", label = 'Select model directory'),
                          fluidRow(style = "margin: 15px;",
                                   column(6,
                                          h3("Positive cells #:"),
                                          div(id = "postive_number", style = "min-height: 600px;",textOutput("postive_number"))),
                                   column(6,
                                          h3("Negative cells #:"),
                                          div(id = "negative_number", style = "min-height: 600px;",textOutput("negative_number")))
                          ),
                          fluidRow(style = "margin: 15px;",
                                   column(12,
                                          h3("PCA Plot"),
                                          div(id = "postive_number", style = "min-height: 600px;",plotOutput("PCA")))),
                          fluidRow(style = "margin: 15px;",
                                   column(12,
                                          downloadButton("model", "Save model file")
                                   ))
                          
                 ),
                 tabPanel("Test Model",
                          sidebarLayout(
                            sidebarPanel(
                              fileInput("rds_file", label = h4("Load the rds file")),
                              fileInput("csv_file", label = h4("Load the paramter file")),
                              downloadButton("parameters", "Get image settings"),
                              fileInput("image_new", "Load test image"),
                              sliderInput("dv","SVM decision value",-5,5,0,step=0.1),
                              sliderInput("test_intensity", "Image intensity:",1,1000,1,step=1)),
                            mainPanel(withSpinner(displayOutput("image_test"))))),
                 tabPanel("test model re-classification",
                          sidebarLayout(
                            sidebarPanel(
                              # h4("Classification of postive objects"),
                              h4("Choose cells"),
                              sliderInput("intensity_classification_1", "Image intensity:",0.1,500,1,step=0.1),
                              actionButton(inputId = "button_postive_1", label="Image update"),
                              downloadButton("postive_training_1", "Download negative example cells")),
                            mainPanel(plotOutput("postive_image_1", width="1000px", height="1000px",click="plot_click"))),
                          sidebarLayout(
                            sidebarPanel(
                              # h4("Classification of negative objects"),
                              h4("Choose cells"),
                              sliderInput("intensity_classification.negtive_1", "choose segmented image intensity:",0.1,500,1,step=0.1),
                              actionButton(inputId = "button_negative_1", label="Image update"),
                              downloadButton("negative_training_1", "Download negative example cells")),
                            mainPanel(plotOutput("negative_image_1", width="1000px", height="1000px",click="plot_click_negative")))
                 )
                 
)

#####################################################
server <- function(input, output,session) {
  options(shiny.maxRequestSize=200*1024^2)
  imgg <- reactive({
    f <- input$images
    if (is.null(f))
      return(NULL)
    readImage(f$datapath,all = T)
  })
  
  observeEvent(input$reset_input,{
    shinyjs::reset("side-panel")
  })
  
  size<-reactive({
    req(imgg())
    size<-as.numeric(dim(imgg()))
  })
  
  dapi_normal <- reactive({
    req(imgg())
    GFP<-imgg()[1:size()[1],1:size()[2],1]
    dapi<-imgg()[1:size()[1],1:size()[2],2]
    # GFP<-imgg()[1:input$size,1:input$size,1]
    # dapi<-imgg()[1:input$size,1:input$size,2]
    minDapi<-min(as.vector(dapi))
    maxDapi<-max(as.vector(dapi))
    minGFP<-min(as.vector(GFP))
    maxGFP<-max(as.vector(GFP))
    dapin<-normalize(dapi, ft=c(0,1),c(minDapi,maxDapi))
    GFPn<-normalize(GFP, ft=c(0,1) ,c(minGFP,maxGFP))
    dapi_normal<- dapin*(input$intensity)
    
  })
  
  output$dapi <- renderDisplay({
    req(dapi_normal())
    display(dapi_normal())
  })
  

  nmask2 <- reactive({
    req(dapi_normal())
    nmask2 = thresh(dapi_normal(), input$wh, input$wh,input$gm)
    mk3 = makeBrush(input$filter, shape= "diamond")
    nmask2 = opening(nmask2, mk3)
  })
  nmask <- reactive({
    req(nmask2())
    nmask2 = fillHull(nmask2())
    nseg = bwlabel(nmask2())  #binery to object
    chackpoint<-computeFeatures.shape(nseg)
    nmask = watershed( distmap(nmask2()),1)
  })
  
  gsegg<-reactive({
    req(nmask())
    nf = computeFeatures.shape(nmask())
    nr = which(nf[,2] < input$peri)    ## now corresponds to slider in UI (previously = 50)
    nseg = rmObjects(nmask(), nr)
    #rm(nf,nmask)
    nn = max(nseg)
    chackpoint<-computeFeatures.shape(nseg)
    int.dapi<-computeFeatures.basic(nseg,dapi_normal())
    y<-which(scores(int.dapi[,1], type="z",prob = 0.95))
    tp<-as.numeric(attr(y,"names"))
    nseg<-rmObjects(nseg,tp)
    chackpoint<-computeFeatures.shape(nseg)
    df<-as.data.frame(chackpoint)
    xy<-computeFeatures.moment(nseg)[,c('m.cx','m.cy')]
    df<-cbind(df,xy)
    #display(seg_dapi, method="raster")
    # blank<-imfill(500,500)
    # display(blank, method="raster")
    df.combine<-as.data.frame(matrix(0,nrow(xy),5))
    colnames(df.combine)<-c("x","y","Area_real","Areal_roundess","ratio")
    df.combine$x<-xy[,1]
    df.combine$y<-xy[,2]
    df.combine$Area_real<-df[,1] #area of sample
    df.combine$Areal_roundess<-pi*(df[,3])^2
    df.combine$ratio<-df.combine[,4]/df[,1]
    nr = which(df.combine[,5] > 1 )
    gsegg = rmObjects(nseg, nr)
    #rm(nseg,df)
    nr = which(df.combine[,5] < 0.6 )
    #rm(df.combine)
    gsegg = rmObjects(gsegg, nr)
    #rm(nr)
    ######gsegg
    seg_dapi = paintObjects(gsegg,toRGB(dapi_normal()),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
    
  })
  
  output$dapi_outline_seg <- renderDisplay({
    req(seg_dapi())
    display(seg_dapi())
  })
  
  output$widget1 <- renderDisplay({
    req(nmask2())
    display(nmask2())
  })
  
  output$final <- renderDisplay({
    req(gsegg())
    display(gsegg())
  })
  
  output$raster <- renderPlot({
    req(imgg())
    plot(imgg()*5, all=FALSE)
  })
  
  
  cell_normal <- reactive({
    req(imgg())
    GFP<-imgg()[1:size()[1],1:size()[2],1]
    dapi<-imgg()[1:size()[1],1:size()[2],2]
    # GFP<-imgg()[1:input$size,1:input$size,1]
    # dapi<-imgg()[1:input$size,1:input$size,2]
    minDapi<-min(as.vector(dapi))
    maxDapi<-max(as.vector(dapi))
    minGFP<-min(as.vector(GFP))
    maxGFP<-max(as.vector(GFP))
    dapin<-normalize(dapi, ft=c(0,1),c(minDapi,maxDapi))
    GFPn<-normalize(GFP, ft=c(0,1) ,c(minGFP,maxGFP))
    cell_normal<- GFPn*input$GFP_intensity
  })
  
  GFP <- reactive({
    req(imgg())
    GFP<-imgg()[1:size()[1],1:size()[2],1]
  })
  
  output$raster_GFP <- renderPlot({
    req(cell_normal())
    plot(cell_normal(), all=FALSE)
  })
  
  seg_pink <- reactive({
    req(cell_normal(),GFP(),gsegg())
    smooth<-makeBrush(size=input$filter_GFP, shape = "disc")
    cell_normall<-filter2(cell_normal(),smooth, boundary = c("circular", "replicate"))
    # display(cell_normal/30,method="raster")
    thr_cell<-thresh(cell_normall, w=input$wh_GFP, h=input$wh_GFP, offset=input$gm_GFP)
    colorMode(thr_cell)<-Grayscale
    cmask = opening(thr_cell, kern=makeBrush(7,shape="disc"))
    open2<-opening(cell_normal()>input$global)
    colorMode(open2)<-Grayscale
    combine<-cmask
    combine[open2 > cmask]<-open2[open2 > cmask]
    combine[gsegg() > combine]<-gsegg()[gsegg() > combine]
    #display(combine,method="raster")
    # display(open_2/10,method="raster")
    #display(cmask,method="raster")
    csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=cmask)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    # display(seg_pink,method="raster")
    #display(csegpink,method="raster")
    csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=combine)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    seg_pink = paintObjects(csegpink,toRGB(cell_normal()),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
  })
  
  
  output$outline_seg <- renderDisplay({
    req(seg_pink())
    display(seg_pink())
  })
  
  csegpink <- reactive({
    req(cell_normal(),GFP(),gsegg())
    smooth<-makeBrush(size=input$filter_GFP, shape = "disc")
    cell_normall<-filter2(cell_normal(),smooth, boundary = c("circular", "replicate"))
    # display(cell_normal/30,method="raster")
    thr_cell<-thresh(cell_normall, w=input$wh_GFP, h=input$wh_GFP, offset=input$gm_GFP)
    colorMode(thr_cell)<-Grayscale
    cmask = opening(thr_cell, kern=makeBrush(7,shape="disc"))
    open2<-opening(cell_normal()>input$global)
    colorMode(open2)<-Grayscale
    combine<-cmask
    combine[open2 > cmask]<-open2[open2 > cmask]
    combine[gsegg() > combine]<-gsegg()[gsegg() > combine]
    #display(combine,method="raster")
    # display(open_2/10,method="raster")
    #display(cmask,method="raster")
    csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=cmask)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    # display(seg_pink,method="raster")
    #display(csegpink,method="raster")
    csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=combine)
    csegpink <- fillHull(csegpink)
  })
  
  seg_pink_size <- reactive({
    req(csegpink(),cell_normal())
    csegpink <- fillHull(csegpink())
    #colorMode(csegpink)<-Grayscale
    xy<-computeFeatures.moment(csegpink)[,c('m.cx','m.cy')]
    cf = computeFeatures.shape(csegpink)
    seg_pink = paintObjects(csegpink,toRGB(GFP()),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
    ##computes the geometric features of the cells outline
    cf = computeFeatures.shape(csegpink)
    ##print(cf)
    cr = which(cf[,2] < input$peri_GFP)
    csegpink = rmObjects(csegpink, cr)
    #display(seg_pink/10,method="raster")
    ##removing small obgects: remove all the object smaller (perimeter) than 250 pixel beased on the perimeter which is column 2
    cf = computeFeatures.shape(csegpink)
    cfErea<-data.frame(cf[,1])
    cfErea$num<-row.names(cfErea)
    ##print(cf)
    ##summary(cf[,1])
    ci = which(cf[,1] > input$size_GFP)
    csegpink = rmObjects(csegpink, ci)
    #rm(cf,ci,cfErea,cr)
    csegpink   = propagate(csegpink, gsegg(), lambda=1.0e-2, mask=combine)
    csegpink <- fillHull(csegpink)
    #colorMode(csegpink)<-Grayscale
    seg_pink = paintObjects(csegpink,toRGB(GFP()*10),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
    #display(seg_pink,method="raster")
    dims<-dim(csegpink)
    #dims
    border<-c(csegpink[1:dims[1],1], csegpink[1:dims[1],dims[2]], csegpink[1,1:dims[2]], csegpink[dims[1],1:dims[2]])
    ##idintify the objects at the bounder
    ids<-unique(border[which(border !=0)])
    csegpink<-rmObjects(csegpink, ids)
    #display(csegpink,"raster")
    #gsegg()<-rmObjects(gsegg(), ids)
    #display(nseg,"raster")
    #rm(border,ids)
    seg_pink_size = paintObjects(csegpink(),toRGB(cell_normal()),opac=c(1, 0.8),col=c("Green",NA),thick=TRUE,closed=T)
  })
  
  
  output$outline_size <- renderDisplay({
    req(seg_pink_size())
    display(seg_pink_size())
  })
  
  xx<-reactive(parmetrs.vector())
  
  output$parameters <- downloadHandler(
    
    filename <- function(){
      paste("table.csv",sep = "_")
    },
    content <- function(file) {
      write.csv(xx(), file, row.names = FALSE)
    }
  )
  
  ##################Classification###########
  DistCost<-reactive(NULL)
  
  observeEvent(input$button_postive,{
    
    initX <- 1
    initY <- 2
    
    ## Source Locations (Home Base)
    source_coords <- reactiveValues(xy=c(x=initX, y=initY) )
    
    ## Dest Coords
    dest_coords <- reactiveValues(x=initX, y=initY)
    observeEvent(plot_click_slow(), {
      dest_coords$x <- c(dest_coords$x, floor(plot_click_slow()$x))
      dest_coords$y <- c(dest_coords$y, floor(plot_click_slow()$y))
    })
    
    ## Don't fire off the plot click too often
    plot_click_slow <- debounce(reactive(input$plot_click), 300)
    
    # Calculate Manhattan Distance from Source to Destination
    DistCost <- reactive({
      num_points <- length(dest_coords$x)
      
      list( Lost= lapply(seq(num_points), function(n) {
        
        c(dest_coords$x[n], dest_coords$y[n])
        
      }) )
    })
    
    
    ## RenderPlot
    output$postive_image <- renderPlot({
      req(seg_pink)
      par(bg=NA)
      plot.new()
      plot.window(
        xlim=c(0,10), ylim=c(0,10),
        yaxs="i", xaxs="i")
      axis(1)
      axis(2)
      grid(10,10, col="black")
      box()
      
      
      plot(seg_pink()*input$intensity_classification)
      ## Source
      
      points(source_coords$xy[1], source_coords$xy[2], cex=3, pch=intToUtf8(8962))
      ## Destination
      text(dest_coords$x, dest_coords$y, paste0(DistCost()$Lost),col="yellow")
      
    })
    
    xy<-reactive({
      xy<-computeFeatures.moment(csegpink())[,c('m.cx','m.cy')]
    })
    
    
    postive_training<-reactive({
      df<-data.frame(matrix(unlist(DistCost()), nrow=length(DistCost()$Lost), byrow=T))
      knn.out <- ann(as.matrix(xy()), as.matrix(df[2:nrow(df),]), k=2)
      row_numb<-knn.out$knnIndexDist
      class(row_numb)
      row_numb<-as.data.frame(row_numb)
      Ts.training<-table_test_pink()
      Ts.training$predict<-0
      Ts.training[row_numb$V1,21]<-"p"
      postive_training<-Ts.training
    })
    
    output$postive_training <- downloadHandler(
      filename <- function(){
        paste(Sys.time(),"postive_training.rds",sep = "_")
      },
      content <- function(file) {
        saveRDS(postive_training(),file = file)
      }
    )
    
  })
  
  
  
  
  
  #####################################
  
  xy<-reactive({
    xy<-computeFeatures.moment(csegpink())[,c('m.cx','m.cy')]
  })
  
  seg_pink_for_negative <- reactive({
    req(cell_normal(),GFP(),gsegg())
    smooth<-makeBrush(size=input$filter_GFP, shape = "disc")
    cell_normall<-filter2(cell_normal(),smooth, boundary = c("circular", "replicate"))
    # display(cell_normal/30,method="raster")
    thr_cell<-thresh(cell_normall, w=input$wh_GFP, h=input$wh_GFP, offset=input$gm_GFP)
    colorMode(thr_cell)<-Grayscale
    cmask = opening(thr_cell, kern=makeBrush(7,shape="disc"))
    open2<-opening(cell_normal()>input$global)
    colorMode(open2)<-Grayscale
    combine<-cmask
    combine[open2 > cmask]<-open2[open2 > cmask]
    combine[gsegg() > combine]<-gsegg()[gsegg() > combine]
    #display(combine,method="raster")
    # display(open_2/10,method="raster")
    #display(cmask,method="raster")
    csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=cmask)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    # display(seg_pink,method="raster")
    #display(csegpink,method="raster")
    csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=combine)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    seg_pink_for_negative = paintObjects(csegpink,toRGB(cell_normal()),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
  })
  
  
  #########CLASSIFY the negative example
  
  DistCostn<-reactive(NULL)
  
  observeEvent(input$button_negative,{
    
    initXn <- 1
    initYn <- 2
    
    ## Source Locations (Home Base)
    
    source_coordsn <- reactiveValues(xyn=c(x=initXn, y=initYn) )
    
    
    
    ## Dest Coords
    
    dest_coordsn <- reactiveValues(x=initXn, y=initYn)
    
    observeEvent(plot_click_slown(), {
      dest_coordsn$x <- c(dest_coordsn$x, floor(plot_click_slown()$x))
      dest_coordsn$y <- c(dest_coordsn$y, floor(plot_click_slown()$y))
    })
    
    ## Don't fire off the plot click too often
    plot_click_slown <- debounce(reactive(input$plot_click_negative), 300)
    
    # Calculate Manhattan Distance from Source to Destination
    DistCostn <- reactive({
      num_points <- length(dest_coordsn$x)
      
      list( Lost= lapply(seq(num_points), function(n) {
        
        c(dest_coordsn$x[n], dest_coordsn$y[n])
        
      }) )
    })
    
    
    
    ## RenderPlot
    output$negative_image <- renderPlot({
      req(seg_pink_for_negative())
      par(bg=NA)
      plot.new()
      plot.window(
        xlim=c(0,10), ylim=c(0,10),
        yaxs="i", xaxs="i")
      axis(1)
      axis(2)
      grid(10,10, col="black")
      box()
      
      
      plot(seg_pink_for_negative()*input$intensity_classification.negtive)
      ## Source
      points( source_coordsn$xyn[1], source_coordsn$xyn[2], cex=3, pch=intToUtf8(8962))
      
      ## Destination
      
      text(dest_coordsn$x, dest_coordsn$y, paste0(DistCostn()$Lost ),col="red")
      
    })
    
    xy<-reactive({
      xy<-computeFeatures.moment(csegpink())[,c('m.cx','m.cy')]
    })
    
    
    postive_training_n<-reactive({
      df<-data.frame(matrix(unlist(DistCostn()), nrow=length(DistCostn()$Lost), byrow=T))
      knn.out <- ann(as.matrix(xy()), as.matrix(df[2:nrow(df),]), k=2)
      row_numb<-knn.out$knnIndexDist
      class(row_numb)
      row_numb<-as.data.frame(row_numb)
      Ts.training<-table_test_pink()
      Ts.training$predict<-0
      Ts.training[row_numb$V1,21]<-"N"
      postive_training<-Ts.training
    })
    
    output$negative_training <- downloadHandler(
      filename <- function(){
        paste(Sys.time(),"negative_training.rds",sep = "_")
      },
      content <- function(file) {
        saveRDS(postive_training_n(),file = file)
      }
    )
    
  })
  
  #####################################
  ####Intate cells selecting process###
  #####################################
  
  
  
  
  observeEvent(eventExpr = input$button,{
    stack_image_list <- reactive({
      req(csegpink(),cell_normal(),GFP())
      table_test_pink_shape = computeFeatures.shape(csegpink(),GFP())
      ##print(table_test_pink_shape)
      table_test_pink_moment = computeFeatures.moment(csegpink(),GFP())
      ##print(table_test_pink_moment)
      table_test_pink_basic = computeFeatures.basic(csegpink(),GFP())
      ##print(table_test_pink_basic)
      table_test_pink<-data.frame(cbind(table_test_pink_basic,table_test_pink_moment,table_test_pink_shape))
      rownameTable<-row.names(table_test_pink)
      table_test_pink<-data.frame(cbind(rownameTable,table_test_pink))
      stack_image_list<-stackObjects(csegpink(),GFP(),combine=T)
      stack_image_list<-channel(stack_image_list,"rgb")
      setwd("/Users/kanferg/Desktop/Gil_LabWork/AIMS/AIMS_090518/Pex_ER/Peroxisome_model/Shiny_app_analysis_012019/temp/")
      writeImage(stack_image_list,paste(Sys.time(),"new.tif"))
      
    })
    
    dim.obse<-reactive({dim(stack_image_list())})
    
    output$cell_number_print<- renderText({
      dim.obse()[3]
    })

  })
  
  
  table_test_pink <- reactive({
    req(csegpink(),cell_normal(),GFP())
    table_test_pink_shape = computeFeatures.shape(csegpink(),GFP())
    ##print(table_test_pink_shape)
    table_test_pink_moment = computeFeatures.moment(csegpink(),GFP())
    ##print(table_test_pink_moment)
    table_test_pink_basic = computeFeatures.basic(csegpink(),GFP())
    ##print(table_test_pink_basic)
    table_test_pink<-data.frame(cbind(table_test_pink_basic,table_test_pink_moment,table_test_pink_shape))
    rownameTable<-row.names(table_test_pink)
    table_test_pink<-data.frame(cbind(rownameTable,table_test_pink))
    
    
  })

  Ts.traning<-reactive({
    req(table_test_pink(),negative_data(),postive_data())
    negative.number<-as.numeric(negative_data())
    postive.number<-as.numeric(postive_data())
    numbers<-c(negative.number,negative.number)
    Ts.training<-table_test_pink()
    Ts.training$predict<-0
    ind.remove<-which(Ts.training$rownameTable %in% numbers)
    Ts.training<-Ts.training[ind.remove,]
    ind.post<-which(Ts.training$rownameTable %in% postive.number)
    Ts.training[ind.post,21]<-"p"
    ind.neg<-which(Ts.training$rownameTable %in% nunegative.number)
    Ts.training[ind.neg,21]<-"N"
  })
  
  
  
  output$training <- downloadHandler(
    filename <- function(){
      paste(Sys.time(),"training.rds",sep = "_")
    },
    content <- function(file) {
      saveRDS(Ts.traning,file = file)
    }
  )
  
  
  #############################################choose path  and save model ##########################################
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        
        # update the widget value
        #updateDirectoryInput(session, 'directory', value = path)
        #path1<-reactive(path)
        
        Ts.traning_sum<-reactive({
          rds<-dir(paste0(path)[grep(".rds",dir(paste0(path)))])
          Ts.traning<-readRDS(paste0(path,"/",rds[1]))
          Ts.traning_sum<-Ts.traning
          for (l in 2:length(rds)){
            Ts.traning<-readRDS(paste0(path,"/",rds[l]))
            Ts.traning_sum<-rbind(Ts.traning_sum,Ts.traning)
          }
          Ts.traning_sum<-as.data.frame(Ts.traning_sum)
        })
        
        output$postive_number<-renderText({
          req(Ts.traning_sum())
          lp<-length(which(Ts.traning_sum()$predict=="p"))
          print(lp)
          
        })
        
        output$negative_number<-renderText({
          req(Ts.traning_sum())
          lN<-length(which(Ts.traning_sum()$predict=="N"))
          print(lN)
          
        })
        
        Ts.traning<-reactive({
          req(Ts.traning_sum())
          lN<-length(which(Ts.traning_sum()$predict=="N"))
          lp<-length(which(Ts.traning_sum()$predict=="p"))
          
          
          Ts.traning_sum<-Ts.traning_sum()[!duplicated(Ts.traning_sum()[c("b.sd","b.mad")]),]
          ind<-which(!Ts.traning_sum$predict==0)
          Ts.training_sum<-Ts.traning_sum[ind,]
          
          ind.p<-which(Ts.traning_sum$predict=="p")
          
          ind.N<-which(Ts.traning_sum$predict=="N")
          
          if (lN > lp) {
            Ts.traning_sum_N<-Ts.traning_sum[sample(ind.N,lp),]
            Ts.traning_sum_p<-Ts.traning_sum[ind.p,]
          }
          else
          { Ts.traning_sum_N<-Ts.traning_sum[ind.n,]
          Ts.traning_sum_p<-Ts.traning_sum[sample(ind.p,lN),]
          }
          
          Ts.traning<-rbind(Ts.traning_sum_N,Ts.traning_sum_p)
          
        })
        
        
        Ts.traning<-reactive({
          req(Ts.traning_sum())
          lN<-length(which(Ts.traning_sum()$predict=="N"))
          lp<-length(which(Ts.traning_sum()$predict=="p"))
          
          
          Ts.traning_sum<-Ts.traning_sum()[!duplicated(Ts.traning_sum()[c("b.sd","b.mad")]),]
          ind<-which(!Ts.traning_sum$predict==0)
          Ts.training_sum<-Ts.traning_sum[ind,]
          
          ind.p<-which(Ts.traning_sum$predict=="p")
          
          ind.N<-which(Ts.traning_sum$predict=="N")
          
          if (lN > lp) {
            Ts.traning_sum_N<-Ts.traning_sum[sample(ind.N,lp),]
            Ts.traning_sum_p<-Ts.traning_sum[ind.p,]
          }
          else
          { Ts.traning_sum_N<-Ts.traning_sum[ind.N,]
          Ts.traning_sum_p<-Ts.traning_sum[sample(ind.p,lN),]
          }
          
          Ts.traning<-rbind(Ts.traning_sum_N,Ts.traning_sum_p)
          
        })
        
        output$PCA<-renderPlot({
          req(Ts.traning())
          Ts<-Ts.traning()
          colnames(Ts)<-c("rownameTable","mean intensity", "sd intensity", "mad intesity", "1% intensity", "5% intensity", "50% intensity","95% intensity","99 intensity","center of mass,x", "center of mass,y","major axis","eccentricity", "angle","area","perimeter","radius.mean","radius.sd","radius.min","radius.max","predict" )
          
          myPr <- prcomp(Ts[, 2:19], scale = TRUE)
          g1<-ggbiplot(myPr, obs.scale = 1, var.scale = 1, groups = F, ellipse = F, circle = F,alpha = 0.2)+
            theme(axis.title.x =element_text(size = 14))+
            theme(axis.title.y =element_text(size = 14))+
            theme(axis.text = element_text(size=12))+
            theme(axis.line = element_line(colour = "black",size=1), panel.border = element_blank(),panel.background = element_blank())+
            # theme(axis.text.x = element_text( hjust = 1))+
            labs(title="PCA - feature selection") +
            theme(plot.title = element_text(hjust = 0.5))+
            #scale_x_continuous(breaks = (seq(0, 15, by=1)))+
            # geom_vline(xintercept = vec_zscore, linetype="dotted", size=  0.5, color = "red")+
            #geom_vline(xintercept = non_target.log2.norm, linetype="dotted", size=  0.5, color = "black")+
            theme(panel.border = element_blank(),panel.background = element_blank())+
            theme(panel.grid.minor = element_line(colour = "white"))+
            theme(panel.border = element_blank(),panel.background = element_blank())+
            theme(panel.grid.minor = element_line(colour = "white"))+
            # theme(axis.text.x=element_blank())+
            # theme(axis.ticks.x=element_line(size=0.5))+
            #theme(axis.line.x=element_blank())+
            theme(legend.position="none")
          
          tryCatch(g1,message="Loading....")
          
        })
        
        mymodel <- reactive({
          req(Ts.traning())
          x<-(Ts.traning()[,2:12])
          y<-Ts.traning()[,21]
          
          dataTest<-data.frame(x,y)
          acc<-rep(0,10)
          
          TestIndex<-sample(nrow(x),round(nrow(x)/2))
          mymodel<-svm(y~., data = dataTest, kernel="linear",type = "C",cost= 5, degree = 45)
          
          #set.seed(50)
          #tmodel<- tune(svm, y~., data = dataTest, ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:4),probability = TRUE ))
          #mymodel<-tmodel$best.model
          
          return(mymodel)
        })
        
        # output$model_text<-renderText({
        #   req(mymodel())
        #   print(mymodel())
        # })
        
        output$model <- downloadHandler(
          
          filename <- function(){
            paste("mymodel.rds")
          },
          content <- function(file) {
            saveRDS(mymodel(),file = file)
          }
        )
        
        
        
      }
      
    }
  )
  
  
  
  
  
  #################################################test model##########################################################
  
  mymodel_new<-reactive({
    m<-input$rds_file
    if (is.null(m))
      return(NULL)
    mymodel_new<-readRDS(m$datapath)
  })
  
  imgg_new <- reactive({
    w <- input$image_new
    if (is.null(w))
      return(NULL)
    readImage(w$datapath,all = T)
  })
  
  size.new<-reactive({
    req(imgg_new())
    size.new<-as.numeric(dim(imgg_new()))
  })
  
  parmeter.set<-reactive({
    df.parmater<-input$csv_file
    if (is.null(df.parmater))
      return(NULL)
    parmeter.set<-read.csv(df.parmater$datapath,stringsAsFactors = F)
  })
  
  
  imageAnalysis.list<-reactive({
    req(imgg_new(),mymodel_new(),size.new(),parmeter.set())
    ll.temp<-list()
    parmeter.set.temp<-parmeter.set()
    GFP<-imgg_new()[1:size.new()[1],1:size.new()[2],1]
    dapi<-imgg_new()[1:size.new()[1],1:size.new()[2],2]
    minDapi<-min(as.vector(dapi))
    maxDapi<-max(as.vector(dapi))
    minGFP<-min(as.vector(GFP))
    maxGFP<-max(as.vector(GFP))
    dapin<-normalize(dapi, ft=c(0,1),c(minDapi,maxDapi))
    GFPn<-normalize(GFP, ft=c(0,1) ,c(minGFP,maxGFP))
    dapi_normal<- dapin*(parmeter.set.temp$dapi.intensity)
    nmask2 = thresh(dapi_normal, parmeter.set.temp$dapi.thershold, parmeter.set.temp$dapi.thershold,parmeter.set.temp$dapi.gama)
    mk3 = makeBrush(parmeter.set.temp$dapi.filter, shape= "diamond")
    nmask2 = opening(nmask2, mk3)
    nmask2 = fillHull(nmask2)
    nseg = bwlabel(nmask2)  #binery to object
    chackpoint<-computeFeatures.shape(nseg)
    nmask = watershed(distmap(nmask2),1)
    nf = computeFeatures.shape(nmask)
    nr = which(nf[,2] < 50)
    nseg = rmObjects(nmask, nr)
    #rm(nf,nmask)
    nn = max(nseg)
    chackpoint<-computeFeatures.shape(nseg)
    int.dapi<-computeFeatures.basic(nseg,dapi_normal)
    y<-which(scores(int.dapi[,1], type="z",prob = 0.95))
    tp<-as.numeric(attr(y,"names"))
    nseg<-rmObjects(nseg,tp)
    chackpoint<-computeFeatures.shape(nseg)
    df<-as.data.frame(chackpoint)
    xy<-computeFeatures.moment(nseg)[,c('m.cx','m.cy')]
    df<-cbind(df,xy)
    #display(seg_dapi, method="raster")
    # blank<-imfill(500,500)
    # display(blank, method="raster")
    df.combine<-as.data.frame(matrix(0,nrow(xy),5))
    colnames(df.combine)<-c("x","y","Area_real","Areal_roundess","ratio")
    df.combine$x<-xy[,1]
    df.combine$y<-xy[,2]
    df.combine$Area_real<-df[,1] #area of sample
    df.combine$Areal_roundess<-pi*(df[,3])^2
    df.combine$ratio<-df.combine[,4]/df[,1]
    nr = which(df.combine[,5] > 1 )
    gsegg = rmObjects(nseg, nr)
    #rm(nseg,df)
    nr = which(df.combine[,5] < 0.6 )
    #rm(df.combine)
    gsegg = rmObjects(gsegg, nr)
    cell_normal<- GFPn*parmeter.set.temp$GFP.intensity
    smooth<-makeBrush(parmeter.set.temp$filter.GFP, shape = "disc")
    cell_normall<-filter2(cell_normal,smooth, boundary = c("circular", "replicate"))
    # display(cell_normal/30,method="raster")
    thr_cell<-thresh(cell_normall, w=parmeter.set.temp$thershold.GFP, h=parmeter.set.temp$thershold.GFP, offset=parmeter.set.temp$gama.GFP)
    colorMode(thr_cell)<-Grayscale
    cmask = opening(thr_cell, kern=makeBrush(7,shape="disc"))
    open2<-opening(cell_normal>parmeter.set.temp$global.thershold.GFP)
    colorMode(open2)<-Grayscale
    combine<-cmask
    combine[open2 > cmask]<-open2[open2 > cmask]
    combine[gsegg > combine]<-gsegg[gsegg > combine]
    #display(combine,method="raster")
    # display(open_2/10,method="raster")
    #display(cmask,method="raster")
    csegpink = propagate(cell_normal, gsegg, lambda=1.0e-2, mask=cmask)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    # display(seg_pink,method="raster")
    #display(csegpink,method="raster")
    csegpink = propagate(cell_normal, gsegg, lambda=1.0e-2, mask=combine)
    csegpink <- fillHull(csegpink)
    colorMode(csegpink)<-Grayscale
    seg_pink = paintObjects(csegpink,toRGB(cell_normal),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
    xy<-computeFeatures.moment(csegpink)[,c('m.cx','m.cy')]
    cf = computeFeatures.shape(csegpink)
    seg_pink = paintObjects(csegpink,toRGB(GFP),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
    ##computes the geometric features of the cells outline
    cf = computeFeatures.shape(csegpink)
    ##print(cf)
    cr = which(cf[,2] < parmeter.set.temp$Perimeter.GFP)
    csegpink = rmObjects(csegpink, cr)
    #display(seg_pink/10,method="raster")
    ##removing small obgects: remove all the object smaller (perimeter) than 250 pixel beased on the perimeter which is column 2
    cf = computeFeatures.shape(csegpink)
    cfErea<-data.frame(cf[,1])
    cfErea$num<-row.names(cfErea)
    ##print(cf)
    ##summary(cf[,1])
    ci = which(cf[,1] > input$size.new_GFP)
    csegpink = rmObjects(csegpink, ci)
    #rm(cf,ci,cfErea,cr)
    csegpink   = propagate(csegpink, gsegg, lambda=1.0e-2, mask=combine)
    csegpink <- fillHull(csegpink)
    #colorMode(csegpink)<-Grayscale
    seg_pink = paintObjects(csegpink,toRGB(GFP*10),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
    #display(seg_pink,method="raster")
    dims<-dim(csegpink)
    #dims
    border<-c(csegpink[1:dims[1],1], csegpink[1:dims[1],dims[2]], csegpink[1,1:dims[2]], csegpink[dims[1],1:dims[2]])
    ##idintify the objects at the bounder
    ids<-unique(border[which(border !=0)])
    csegpink<-rmObjects(csegpink, ids)
    #display(csegpink,"raster")
    gsegg<-rmObjects(gsegg, ids)
    #display(nseg,"raster")
    #rm(border,ids)
    seg_pink_size.new = paintObjects(csegpink,toRGB(cell_normal),opac=c(1, 0.8),col=c("Green",NA),thick=TRUE,closed=T)
    table_test_pink_shape = computeFeatures.shape(csegpink,GFP)
    ##print(table_test_pink_shape)
    table_test_pink_moment = computeFeatures.moment(csegpink,GFP)
    ##print(table_test_pink_moment)
    table_test_pink_basic = computeFeatures.basic(csegpink,GFP)
    ##print(table_test_pink_basic)
    table_test_pink<-data.frame(cbind(table_test_pink_basic,table_test_pink_moment,table_test_pink_shape))
    rownameTable<-row.names(table_test_pink)
    table_test_pink<-data.frame(cbind(rownameTable,table_test_pink))
    Ts.mix<-table_test_pink[,2:12]
    rowNameTable<-table_test_pink[,1]
    ll.temp$Ts.mix<-Ts.mix
    ll.temp$table_test_pink<-table_test_pink
    ll.temp$rowNameTable<-rowNameTable
    ll.temp$gsegg<-gsegg
    ll.temp$GFPn<-GFPn
    ll.temp$GFP<-GFP
    ll.temp$cell_normal<-cell_normal
    ll.temp$csegpink<-csegpink
    ll.temp$xy<-xy
    imageAnalysis.list<-ll.temp
  })
  
  seg_pink_positive<-reactive({
    ll.temp<-list()
    req(imageAnalysis.list(),mymodel_new())
    Ts.mix<-imageAnalysis.list()$Ts.mix
    table_test_pink<-imageAnalysis.list()$table_test_pink
    rowNameTable<-imageAnalysis.list()$rowNameTable
    gsegg<-imageAnalysis.list()$gsegg
    GFPn<-imageAnalysis.list()$GFPn
    y.pred<-predict(mymodel_new(),Ts.mix, decision.values = T)
    length(y.pred)
    #acc=length(which(y.pred==y[-TestIndex]))/length(y.pred)
    d=attr(y.pred,"decision.values")
    #write.table(y.pred,"C:/Users/Demo/Desktop/Data/Gil/20171213_pakrincccp/table.txt")
    #c.pred<-predict(model,Ts.mix)
    #tabledc<-data.table(d,y.pred)
    new.y.pred=rep("p",length(y.pred))
    NewCutoff=input$dv
    new.y.pred[d>NewCutoff]="N"
    length(new.y.pred)
    #tabledc<-data.table(d,new.y.pred)
    d<-round(d,1)
    Ts.mix$pred<-as.array(new.y.pred)
    #Ts.mix$pred<-as.array(y.pred)
    Ts.mix<-Ts.mix[1:length(table_test_pink[,1]),]
    #rm(table_test_pink)
    Ts.mix$rowNameTable<-rowNameTable
    # ir = which(Ts.mix$pred %in% "N")
    # csegpink = rmObjects(csegpink, ir)
    #seg_pink_positive = paintObjects(csegpink,toRGB(GFP*2),opac=c(1,0.8),col=c("Green",NA),thick=TRUE,closed=FALSE)
    # display(seg_pink_positive,"raster")
    # display(csegpink*5,"raster")
    # pr = which(Ts.mix$pred %in% "N")
    # mseg<- rmObjects(gsegg, nr)
    nr = which(Ts.mix$pred %in% "p")
    gsegg = rmObjects(gsegg, nr)
    ll.temp$gsegg.selected<-gsegg
    seg_pink_positive<-ll.temp
  })
  
  output$image_test <- renderDisplay({
    req(seg_pink_positive(),imageAnalysis.list())
    gsegg.selected<-seg_pink_positive()$gsegg.selected
    GFPn<-imageAnalysis.list()$GFPn
    call_Image = paintObjects(gsegg.selected,toRGB(GFPn),opac=c(1,0.8),col=c("Green",NA),thick=TRUE,closed=FALSE)
    display(call_Image*input$test_intensity)
  })
  
  seg_finemodel <-reactive ({
    req(imageAnalysis.list(),seg_pink_positive())
    cseg<-imageAnalysis.list()$csegpink
    
  })
  
  xx<-reactive({
    paramters.df<-as.data.frame(matrix(0,1,13))
    colnames(paramters.df)<-c("dapi intensity", "dapi thershold", "dapi gama", "dapi filter", "GFP intensity", "filter GFP", "thershold GFP", "gama GFP", "global thershold GFP","Perimeter GFP","Erea GFP","decsion value GFP")
    paramters.df[1,]<-c(input$intensity, input$wh,input$gm,input$filter,input$GFP_intensity, input$filter_GFP,input$wh_GFP,input$gm_GFP,input$global, input$peri_GFP,input$size.new_GFP,input$dv)
    xx<-paramters.df
  })
  
  output$parameters <- downloadHandler(
    
    filename <- function(){
      paste("table.csv",sep = "_")
    },
    content <- function(file) {
      write.csv(xx(), file, row.names = FALSE)
    }
  )
  ######################################################################Corrected SVM
  ###postive example
  DistCost<-reactive(NULL)
  
  observeEvent(input$button_postive_1,{
    
    initX <- 1
    initY <- 2
    
    ## Source Locations (Home Base)
    source_coords <- reactiveValues(xy=c(x=initX, y=initY) )
    
    ## Dest Coords
    dest_coords <- reactiveValues(x=initX, y=initY)
    observeEvent(plot_click_slow(), {
      dest_coords$x <- c(dest_coords$x, floor(plot_click_slow()$x))
      dest_coords$y <- c(dest_coords$y, floor(plot_click_slow()$y))
    })
    
    ## Don't fire off the plot click too often
    plot_click_slow <- debounce(reactive(input$plot_click), 300)
    
    # Calculate Manhattan Distance from Source to Destination
    DistCost <- reactive({
      num_points <- length(dest_coords$x)
      
      list( Lost= lapply(seq(num_points), function(n) {
        
        c(dest_coords$x[n], dest_coords$y[n])
        
      }) )
    })
    
    
    ## RenderPlot
    output$postive_image_1 <- renderPlot({
      req(seg_pink_positive(),imageAnalysis.list())
      par(bg=NA)
      plot.new()
      plot.window(
        xlim=c(0,10), ylim=c(0,10),
        yaxs="i", xaxs="i")
      axis(1)
      axis(2)
      grid(10,10, col="black")
      box()
      
      
      gsegg.selected<-seg_pink_positive()$gsegg.selected
      GFPn<-imageAnalysis.list()$GFPn
      csegpink<-imageAnalysis.list()$csegpink
      call_Image_postive = paintObjects(gsegg.selected,paintObjects(csegpink,toRGB(GFPn),opac=c(1,0.8),col=c("Green",NA),thick=TRUE,closed=FALSE),opac=c(1,0.8),col=c("red",NA))
      
      plot(call_Image_postive*input$intensity_classification_1)
      ## Source
      
      points(source_coords$xy[1], source_coords$xy[2], cex=3, pch=intToUtf8(8962))
      ## Destination
      text(dest_coords$x, dest_coords$y, paste0(DistCost()$Lost),col="yellow")
      
    })
    
    
    
    postive_training_1<-reactive({
      req(imageAnalysis.list())
      xy<-imageAnalysis.list()$xy
      table_test_pink<-imageAnalysis.list()$table_test_pink
      df<-data.frame(matrix(unlist(DistCost()), nrow=length(DistCost()$Lost), byrow=T))
      knn.out <- ann(as.matrix(xy), as.matrix(df[2:nrow(df),]), k=2)
      row_numb<-knn.out$knnIndexDist
      class(row_numb)
      row_numb<-as.data.frame(row_numb)
      Ts.training<-table_test_pink
      Ts.training$predict<-0
      Ts.training[row_numb$V1,21]<-"p"
      postive_training_1<-Ts.training
    })
    
    output$postive_training_1 <- downloadHandler(
      filename <- function(){
        paste(Sys.time(),"postive_training.rds",sep = "_")
      },
      content <- function(file) {
        saveRDS(postive_training_1(),file = file)
      }
    )
    
  })
  
  
  
  
  
  #####################################
  #
  #   xy<-reactive({
  #     xy<-computeFeatures.moment(csegpink())[,c('m.cx','m.cy')]
  #   })
  #
  #   seg_pink_for_negative <- reactive({
  #     req(cell_normal(),GFP(),gsegg())
  #     smooth<-makeBrush(size=input$filter_GFP, shape = "disc")
  #     cell_normall<-filter2(cell_normal(),smooth, boundary = c("circular", "replicate"))
  #     # display(cell_normal/30,method="raster")
  #     thr_cell<-thresh(cell_normall, w=input$wh_GFP, h=input$wh_GFP, offset=input$gm_GFP)
  #     colorMode(thr_cell)<-Grayscale
  #     cmask = opening(thr_cell, kern=makeBrush(7,shape="disc"))
  #     open2<-opening(cell_normal()>input$global)
  #     colorMode(open2)<-Grayscale
  #     combine<-cmask
  #     combine[open2 > cmask]<-open2[open2 > cmask]
  #     combine[gsegg() > combine]<-gsegg()[gsegg() > combine]
  #     #display(combine,method="raster")
  #     # display(open_2/10,method="raster")
  #     #display(cmask,method="raster")
  #     csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=cmask)
  #     csegpink <- fillHull(csegpink)
  #     colorMode(csegpink)<-Grayscale
  #     # display(seg_pink,method="raster")
  #     #display(csegpink,method="raster")
  #     csegpink = propagate(cell_normal(), gsegg(), lambda=1.0e-2, mask=combine)
  #     csegpink <- fillHull(csegpink)
  #     colorMode(csegpink)<-Grayscale
  #     seg_pink_for_negative = paintObjects(csegpink,toRGB(cell_normal()),opac=c(1, 1),col=c("red",NA),thick=TRUE,closed=TRUE)
  #   })
  #
  
  
  #########CLASSIFY the negative example
  
  DistCostn<-reactive(NULL)
  
  observeEvent(input$button_negative_1,{
    
    initXn <- 1
    initYn <- 2
    
    ## Source Locations (Home Base)
    
    source_coordsn <- reactiveValues(xyn=c(x=initXn, y=initYn) )
    
    ## Dest Coords
    
    dest_coordsn <- reactiveValues(x=initXn, y=initYn)
    
    observeEvent(plot_click_slown(), {
      dest_coordsn$x <- c(dest_coordsn$x, floor(plot_click_slown()$x))
      dest_coordsn$y <- c(dest_coordsn$y, floor(plot_click_slown()$y))
    })
    
    ## Don't fire off the plot click too often
    plot_click_slown <- debounce(reactive(input$plot_click_negative), 300)
    
    # Calculate Manhattan Distance from Source to Destination
    DistCostn <- reactive({
      num_points <- length(dest_coordsn$x)
      
      list( Lost= lapply(seq(num_points), function(n) {
        
        c(dest_coordsn$x[n], dest_coordsn$y[n])
        
      }) )
    })
    
    
    
    ## RenderPlot
    output$negative_image_1 <- renderPlot({
      req(seg_pink_positive(),imageAnalysis.list())
      par(bg=NA)
      plot.new()
      plot.window(
        xlim=c(0,10), ylim=c(0,10),
        yaxs="i", xaxs="i")
      axis(1)
      axis(2)
      grid(10,10, col="black")
      box()
      
      
      gsegg.selected<-seg_pink_positive()$gsegg.selected
      GFPn<-imageAnalysis.list()$GFPn
      csegpink<-imageAnalysis.list()$csegpink
      seg_pink_for_negative = paintObjects(gsegg.selected,paintObjects(csegpink,toRGB(GFPn),opac=c(1,0.8),col=c("Green",NA),thick=TRUE,closed=FALSE),opac=c(1,0.8),col=c("red",NA))
      
      
      plot(seg_pink_for_negative*input$intensity_classification.negtive_1)
      ## Source
      points(source_coordsn$xyn[1], source_coordsn$xyn[2], cex=3, pch=intToUtf8(8962))
      
      ## Destination
      
      text(dest_coordsn$x, dest_coordsn$y, paste0(DistCostn()$Lost ),col="red")
      
    })
    
    xy<-reactive({
      xy<-computeFeatures.moment(csegpink())[,c('m.cx','m.cy')]
    })
    
    
    postive_training_n<-reactive({
      req(imageAnalysis.list())
      xy<-imageAnalysis.list()$xy
      table_test_pink<-imageAnalysis.list()$table_test_pink
      df<-data.frame(matrix(unlist(DistCostn()), nrow=length(DistCostn()$Lost), byrow=T))
      knn.out <- ann(as.matrix(xy), as.matrix(df[2:nrow(df),]), k=2)
      row_numb<-knn.out$knnIndexDist
      class(row_numb)
      row_numb<-as.data.frame(row_numb)
      Ts.training<-table_test_pink
      Ts.training$predict<-0
      Ts.training[row_numb$V1,21]<-"N"
      postive_training_n<-Ts.training
    })
    
    output$negative_training_1 <- downloadHandler(
      filename <- function(){
        paste(Sys.time(),"negative_training.rds",sep = "_")
      },
      content <- function(file) {
        saveRDS(postive_training_n(),file = file)
      }
    )
    
  })
  
  #####################################
  ####Intate cells selecting process###
  #####################################
  
  
  
  
  # observeEvent(eventExpr = input$button,{
  #   stack_image_list <- reactive({
  #     req(csegpink(),cell_normal(),GFP())
  #     table_test_pink_shape = computeFeatures.shape(csegpink(),GFP())
  #     ##print(table_test_pink_shape)
  #     table_test_pink_moment = computeFeatures.moment(csegpink(),GFP())
  #     ##print(table_test_pink_moment)
  #     table_test_pink_basic = computeFeatures.basic(csegpink(),GFP())
  #     ##print(table_test_pink_basic)
  #     table_test_pink<-data.frame(cbind(table_test_pink_basic,table_test_pink_moment,table_test_pink_shape))
  #     rownameTable<-row.names(table_test_pink)
  #     table_test_pink<-data.frame(cbind(rownameTable,table_test_pink))
  #     stack_image_list<-stackObjects(csegpink(),GFP(),combine=T)
  #     stack_image_list<-channel(stack_image_list,"rgb")
  #     setwd("/Users/kanferg/Desktop/Gil_LabWork/AIMS/AIMS_090518/Pex_ER/Peroxisome_model/Shiny_app_analysis_012019/temp/")
  #     writeImage(stack_image_list,paste(Sys.time(),"new.tif"))
  #
  #   })
  #
  #   dim.obse<-reactive({dim(stack_image_list())})
  #
  #   output$cell_number_print<- renderText({
  #     dim.obse()[3]
  #   })
  #
  #
  #
  # })
  
  
  table_test_pink <- reactive({
    req(csegpink(),cell_normal(),GFP())
    table_test_pink_shape = computeFeatures.shape(csegpink(),GFP())
    ##print(table_test_pink_shape)
    table_test_pink_moment = computeFeatures.moment(csegpink(),GFP())
    ##print(table_test_pink_moment)
    table_test_pink_basic = computeFeatures.basic(csegpink(),GFP())
    ##print(table_test_pink_basic)
    table_test_pink<-data.frame(cbind(table_test_pink_basic,table_test_pink_moment,table_test_pink_shape))
    rownameTable<-row.names(table_test_pink)
    table_test_pink<-data.frame(cbind(rownameTable,table_test_pink))
    
    
  })
  
  
  
  
  
  
  Ts.traning<-reactive({
    req(table_test_pink(),negative_data(),postive_data())
    negative.number<-as.numeric(negative_data())
    postive.number<-as.numeric(postive_data())
    numbers<-c(negative.number,negative.number)
    Ts.training<-table_test_pink()
    Ts.training$predict<-0
    ind.remove<-which(Ts.training$rownameTable %in% numbers)
    Ts.training<-Ts.training[ind.remove,]
    ind.post<-which(Ts.training$rownameTable %in% postive.number)
    Ts.training[ind.post,21]<-"p"
    ind.neg<-which(Ts.training$rownameTable %in% nunegative.number)
    Ts.training[ind.neg,21]<-"N"
  })
  
  
  
  output$training <- downloadHandler(
    filename <- function(){
      paste(Sys.time(),"training.rds",sep = "_")
    },
    content <- function(file) {
      saveRDS(Ts.traning,file = file)
    }
  )
  
  
  
  
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server,options = list(display.mode = 'showcase'))