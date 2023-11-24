library(shiny)
library(shinythemes)
library(colorRamps)
library(dplyr)
library(plotly)
library(DT)

library(ggplot2)
library(reshape2)


availableYears<- list("2023", "2022")

ui <- fluidPage(
  tags$head(includeScript("google-analytics.js")),
  theme = shinytheme("cerulean"), ## journal # united #cerulean # yeti
  titlePanel("The Stock Book"),
  # textInput("pdfurl", "PDF URL")
  # htmlOutput('pdfviewer')
  selectInput("year", h3("Select Stock Book Year"),
              choices = availableYears), 
  navlistPanel(id="mainpanel", widths=c(2,10), 
               tabPanel("Introduction", 
                        tabsetPanel(type="tabs",
                                    tabPanel("About the Stockbook",
                                             htmlOutput("Introtext"),
                                             HTML("<br><br>")),
                                    tabPanel("Organisation of the Stock Book",htmlOutput("Introtext2"),
                                             HTML("<br><br>")),
                                    tabPanel("Ireland's TAC", 
                                             textOutput("TACsubheading"),
                                             #"Ireland's Share of the EU TAC",
                                             HTML("<br><br>"),
                                             tags$head(tags$style(type="text/css", ".test_type {font-size: 11px;}")),
                                             div(class="test_type", tableOutput("IntroTable")),
                                             textOutput("TACtext1"),
                                             plotOutput("Introplot",height="800px"),
                                             "1 Only TAC areas where Ireland has share of the TAC>0t are included",p(),
                                             textOutput("TACtext2"),
                                             HTML("<br><br>")),
                                    #Nov 2023 moved the Data Quality set-up to a uiOutput() to avoid 'Navigation Containers' warning
                                    tabPanel("Data Quality", uiOutput("DataQuality"),
                                             HTML("<br><br>")),
                                    tabPanel("ICES Rationale",htmlOutput("Rationaletext"),
                                             HTML("<br><br>")))),
               tabPanel("Long Term Management Plans", htmlOutput("LongTermManagementtext"),p(),
                        imageOutput("MgtPlanFlow", height="30%"),p(),
                        imageOutput("MgtPlan", height="100%"),
                        fluidRow(column(width = 5, imageOutput("MgtPlan2", height="100%")),
                                 column(width = 5, imageOutput("MgtPlan3", height="100%"))),
                        HTML("<br><br>")),
               tabPanel("Advice Summary", textOutput("AdviceSummtext"),p(),
                        imageOutput("AdviceSummtable1", height="100%"),p(),
                        imageOutput("AdviceSummtable2", height="100%"),
                        HTML("<br><br>")),
               #Nov 2022 moved the Sustainability Ass. set-up to a uiOutput() to avoid 'Navigation Containers' warning
               tabPanel("Sustainability Assessment", uiOutput("SustainAss"),
                        HTML("<br><br>")),
#              tabPanel("Mixed Fisheries", uiOutput("MixFish"),
#                        HTML("<br><br>")),
               tabPanel("Mixed Fisheries", 
                        tabsetPanel(type="tabs",
                                    tabPanel("Celtic Sea (pg.1)",uiOutput("CelticSea_1")),
                                    tabPanel("Celtic Sea (pg.2)",uiOutput("CelticSea_2")),
                                    tabPanel("Irish Sea (pg.1)",uiOutput("IrishSea_1")),
                                    tabPanel("Irish Sea (pg.2)",uiOutput("IrishSea_2"))
                                    )),
               tabPanel("Recent Ecosystem Advice", uiOutput("RecentAdvice"),
                        HTML("<br><br>")),
               tabPanel("MI At-Sea Sampling", htmlOutput("AtSea_Text")),
               tabPanel("Stock Advice", value="StockAdvice_tab",
                        sidebarLayout(fluidRow(column(3,uiOutput("speciesSelector")),
                                               column(5,uiOutput("DescSelector"))),
                                      mainPanel(uiOutput("tabstest"),width = 12))),
               tabPanel("Definitions", htmlOutput("Defns"), htmlOutput("Defns2"),
                        HTML("<br><br>"))),
  hr(),
  fluidRow(width =12,#style = "margin-top:-4em",
           column(3,img(src="Logos/Irelands_EU_ESIF_2014_2020_en.jpg", width = "300px", height = "90px",style="margin-top: 2em; margin-left: 5px; margin-right: 5px;")),
           column(3,img(src="Logos/dafm_logo.png", width = "300px", height = "100px",style="margin-top: 1.5em;")),
           column(3,img(src="Logos/EU logo with text.jpg", width = "280px", height = "90px",style="margin-top: 32px;")),
           column(3,img(src="Logos/Marine_logo_rgb.jpg", width = "320px", height = "90px",style="margin-left: -0px;margin-top: 2em;"))
           #img(src="Niamh.png", width = "1250px", height = "100px", style="display: block; margin-left: auto; margin-right: auto;margin-top:0em")
  )
)

############################################################################################
############################################################################################
server <- function(input, output, session) {
  
  # This code has been moved here because we need the data for handling URL parameters
  ################
  # Stock Advice #
  ################
  #Species Table for mapping different codes
  #In previous years, 'StockAdvice.csv' was called 'ICES-New-Old - extra species.csv'
  ICEStable=read.csv('StockAdvice.csv', header=TRUE)
  ICEStable$Fish=as.character(ICEStable$Fish)
  ICEStable$Fish <- trimws(ICEStable$Fish)
  ICEStable$SpeciesByDiv=as.character(ICEStable$SpeciesByDiv)
  ICEStable$SpeciesByDiv <- trimws(ICEStable$SpeciesByDiv)
  Speciesfilter <- unique(ICEStable$Fish)
  
  ##SM Nov2023: Warning: Error in data.frame: arguments imply differing number of rows: 1, 0  - this shows when the three cod sub-stocks are chosen.....
  ## Read parameter strings from the URL and change the selection appropriately
  observe({
    urlParameters <- parseQueryString(session$clientData$url_search)
    
    ## If we have a stock parameter in the URL we will try and use it to
    ## choose our default stock
    if (!is.null(urlParameters[['stock']])) {
      
      stockURLParameter <- urlParameters[['stock']]
      
      stockParameterFrame <- ICEStable[as.character(ICEStable$New)==stockURLParameter,]
      stockParameterFish <- stockParameterFrame$Fish
      
      # If we didn't get a match just use the first species in the data frame as the default species
      if (length(stockParameterFish) == 0){
        stockParameterFish <- ICEStable[1,"Fish"]
      }
      
      #print(stockParameterFish)
      
      # create the speciesfilter input
      output$speciesSelector <- renderUI({
        selectInput("speciesfilter", h3("Select Species"), as.list(Speciesfilter), selected = stockParameterFish) 
      })
      
      stockParameterDiv <- stockParameterFrame$SpeciesByDiv
      
      # If we didn't get a match just use the first area for that species in the data frame as the default species
      if (length(stockParameterDiv) == 0){
        stockParameterDiv <- ICEStable[1,"SpeciesByDiv"]
      }
      
      #print(stockParameterDiv)
      
      # create the DescSelector input~~~~~~~~~~
      output$DescSelector <- renderUI({
        SpeciesbyDiv=filter(ICEStable, Fish %in% c(input$speciesfilter))
        Descriptions <- unique(SpeciesbyDiv$SpeciesByDiv)
        selectInput("speciesbydiv", h3("Select Stock Area"), as.list(Descriptions), selected = stockParameterDiv)
      })
      
      #Show the Stock Advice tab using SELECT - this is a bit of hack to make sure the
      # user is taken to the Stock page first if a stock parameter is provided
      showTab("mainpanel","StockAdvice_tab",select= TRUE, session)
      
    } 
    ## If there's no stock parameter we'll just default to Cod, and use the first area as the selected stock
    else 
    {
      # create the speciesfilter input with a default selection
      output$speciesSelector <- renderUI({
        selectInput("speciesfilter", h3("Select Species"), as.list(Speciesfilter), selected = "Cod") 
      }) 
      
      # create the DescSelector input with a default selection
      output$DescSelector <- renderUI({
        SpeciesbyDiv=filter(ICEStable, Fish %in% c(input$speciesfilter))
        Descriptions <- unique(SpeciesbyDiv$SpeciesByDiv)
        selectInput("speciesbydiv", h3("Select Stock Area"), as.list(Descriptions), selected = Descriptions[1])
      })
      
    }
    
    # YEAR 
    # ~~~~
    # If we have a valid year parameter - change the year input selection to the relevent value
    if (!is.null(urlParameters[['year']])) {
      
      yearURLParameter <- urlParameters[['year']]
      
      if(yearURLParameter %in% availableYears){
        updateSelectInput(session, 
                          "year",
                          choices= availableYears,
                          selected= yearURLParameter )
      }
    }
  })
  
  ################
  # Introduction # 
  ################
  # The 'Introduction.csv' contains the text for the tabs:
  #      1. About the Stockbook, 2. Organisation of the Stockbook (called 'Structure' in the csv), (Irelands TAC is a table)
  #      3. Data Quality, 4. Ices Rationale
  #      also - Sustainability Ass, LongTermManagement plans and Definitions
  
  #About the Stockbook
  #~~~~~~~~~~~~~~~~~~~
  Introduction=read.csv('Introduction.csv', header=TRUE)
  output$Introtext <- renderText({
    paste0(Introduction[1, which(colnames(Introduction)==paste0("X", input$year))])
  })
  output$Introtext2 <- renderText({
    paste0(Introduction[2, which(colnames(Introduction)==paste0("X", input$year))])
  })
  
  #Irelands TAC  (aka Introduction Table)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ###SM ADDED 2021--> Due to Brexit, the plot is deleted### For plot code see Git_2022 or previous years
  ###From 2021 onwards --> Due to Brexit, the final two columns are deleted### #else IntroTable1[,2:6]  
  #SM Nov23: Since Brexit, the TAC Tab is only using 4 columns (year is hidden) "Year", "Species", "TAC Area", "Irl. Tonnes"
  # This code deals with 2023+
  # If anything changes in future years please see the code in 2022_Git app.R. Previous years requirements have been deleted here
  
  IntroductionTable=read.csv('IntroductionTable.csv', header=TRUE)
  #IntroductionTable$Est..Value.of.Irl.Quota=paste("\u20ac", IntroductionTable$Est..Value.of.Irl.Quota)
  #IntroductionTable$Est..Value.of.EU.TAC=paste("\u20ac", IntroductionTable$Est..Value.of.EU.TAC)
  IntroductionTable$Stock= paste(IntroductionTable$Species, IntroductionTable$TAC.Area, sep=' ')
  names(IntroductionTable)=c("Year", "Species", "TAC Area", "Irl. Tonnes",
                             "Est. Value of Irl. Quota", "Est. Value of EU TAC", "Ireland", "Other", "Stock")
  output$IntroTable = renderTable({
    IntroTable1=filter(IntroductionTable, Year==input$year)
    #IntroTable1[,2:6] # now replaced with lines 225 to 229 (SM)
    
    
    if(input$year>=2022){
      IntroTable1[,2:4]
    }
  }, colnames = TRUE, bordered = TRUE,height=1600)#,width = '100%')#, height="50%", width = "60%")
  
  
  # djc 19/11/2021 Change the TAC sub-heading based on the year selected
  output$TACsubheading=renderText({
       paste0(input$year," Irish quota in tonnes")
  })
  
  
  #Data Quality
  #~~~~~~~~~~~~
    output$DataQualitytext1 <- renderText({
      paste0(Introduction[7, which(colnames(Introduction)==paste0("X", input$year))])
      })
  
    output$DataQualitytext2 <- renderText({
        paste0(Introduction[8, which(colnames(Introduction)==paste0("X", input$year))])
      })
  
    output$DataQualityimage1 <- renderImage({
      image_file <- paste0("www/Introduction/DataQuality",input$year,".png")
      return(list(src = image_file, filetype = "image/png", height = 500))
    }, deleteFile = FALSE) 
    
    output$DataQualityimage2 <- renderImage({
      image_file <- paste0("www/Introduction/DataQualityPrinciples",input$year,".png")
      return(list(src = image_file, filetype = "image/png", height = 700))
    }, deleteFile = FALSE) 
    
    
    
  #ICES Rationale
  #~~~~~~~~~~~~~~
  output$Rationaletext <- renderText({
    paste0(Introduction[3, which(colnames(Introduction)==paste0("X", input$year))])
  })
  
  
  ##############################
  # Long Term Management Plans #
  ##############################
  output$LongTermManagementtext <- renderText({
    
    paste0(Introduction[9, which(colnames(Introduction)==paste0("X", input$year))])
    
  })
  output$MgtPlan <- renderImage({
    image_file <- paste0("www/Introduction/ManagementPlan",input$year,".PNG")
    return(list(src = image_file, filetype = "image/png", height = 850))
  }, deleteFile = FALSE)
  
  output$MgtPlanFlow <- renderImage({
    image_file <- paste0("www/Introduction/ManPlanFlow",input$year,".png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  
  output$MgtPlan2 <- renderImage({
    image_file <- paste0("www/Introduction/ManPlanAppIX_Stocks_1_",input$year,".png")
    return(list(src = image_file, filetype = "image/png", height = 950))
  }, deleteFile = FALSE)
  
  output$MgtPlan3 <- renderImage({
    image_file <- paste0("www/Introduction/ManPlanAppIX_Stocks_2_",input$year,".png")
    return(list(src = image_file, filetype = "image/png", height = 950))
  }, deleteFile = FALSE)
  

  ##################
  # Advice Summary #   #original width (too big)--> # width = 1100
  ##################
  output$AdviceSummtable1 <- renderImage({
    image_file <- paste0("www/Introduction/AdviceSumm",input$year,"table1.PNG")
    return(list(src = image_file, filetype = "image/png", width = 750))
  }, deleteFile = FALSE)
  output$AdviceSummtable2 <- renderImage({
    image_file <- paste0("www/Introduction/AdviceSumm",input$year,"table2.PNG")
    return(list(src = image_file, filetype = "image/png", width = 750))
  }, deleteFile = FALSE)
  output$AdviceSummtable3 <- renderImage({
    image_file <- paste0("www/Introduction/AdviceSumm",input$year,"table3.PNG")
    return(list(src = image_file, filetype = "image/png", width = 750))
  }, deleteFile = FALSE)
  output$AdviceSummtext<- renderText({
    paste0("Marine Institute Summary on the Status, Scientific Advice for ", as.numeric(input$year)+1,
           " for those Stocks of Interest to Ireland")
  })
  
  #############################
  # Sustainability Assessment #  #table 3 was width=800 
  #############################
  output$Sustainabilitytext <- renderText({
    paste0(Introduction[4, which(colnames(Introduction)==paste0("X", input$year))])
  })
  output$SustainabilityTabletext <- renderText({
    paste0("Table 4: Stocks with a status change between the ", as.numeric(input$year)-1, 
           " and ", input$year, " assessments.") # SM Nov: changed in 2021 from 'Stock Books'
  })
  # Figure 1 text added in 2022
  output$SustainabilityFigText <- renderText({
    paste0("Figure 1: Number of stocks assessed to be fished below Fmsy and where SSB is above MSY Btrigger in the Stock Book each year, with trends indicated.") 
  })
  # SM Nov: changed in 2021 from 'Stock Books'
  output$Sustainabilitytable1 <- renderImage({
    image_file <- paste0("www/Introduction/Sustain",input$year,"Table1.png")
    return(list(src = image_file, filetype = "image/png", width = 700))
  }, deleteFile = FALSE)
  output$Sustainabilitytable2 <- renderImage({
    image_file <- paste0("www/Introduction/Sustain",input$year,"Table2.png")
    return(list(src = image_file, filetype = "image/png", width = 700))
  }, deleteFile = FALSE)
  output$Sustainabilitytable3 <- renderImage({
    image_file <- paste0("www/Introduction/Sustain",input$year,"Table3.PNG")
    return(list(src = image_file, filetype = "image/png", width = 800))
  }, deleteFile = FALSE)
  output$Sustainabilitytable4 <- renderImage({
    image_file <- paste0("www/Introduction/Sustain",input$year,"Table4.PNG")
    return(list(src = image_file, filetype = "image/png", width = 800))
  }, deleteFile = FALSE)
  #Added in 2022
  output$SustainabilityFig <- renderImage({
    image_file <- paste0("www/Introduction/Sustain",input$year,"Fig1.png")
    return(list(src = image_file, filetype = "image/png", width = 800))
  }, deleteFile = FALSE)
  
  ###############
  # Definitions #
  ###############
  # 2022: SM Oct. The definition list was updated by HG. Additions, Deletions and updates have been applied
  # 2021: Changed the column from '3' to '2' so that the second cell is also displayed ) 
  output$Defns <- renderText({
    #   #print(Introduction[5, 2])
    paste0(Introduction[5, 2])
  })
  output$Defns2 <- renderText({
    #print(Introduction[6, 2])
    paste0(Introduction[6, 2])
  })
  
  #################################
  # Mixed Fisheries #
  #################################
  #MIX FISH TEXT (in 2023 this was for the Celtic Sea)
  MixedFish=read.csv('MixedFish.csv', header=TRUE)
  output$SummaryText <- renderText({
    paste0(MixedFish[1, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$Considerations <- renderText({
    paste0(MixedFish[2, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$PressureState<- renderText({
    paste0(MixedFish[3, which(colnames(MixedFish)==paste0("X", input$year))])
  })  
  output$D3summ<- renderText({
    paste0(MixedFish[4, which(colnames(MixedFish)==paste0("X", input$year))])
  }) 
  output$D3text<- renderText({
    paste0(MixedFish[5, which(colnames(MixedFish)==paste0("X", input$year))])
  }) 
  #2023 CELTIC SEAS TEXT
  output$MixedFish_1 <-renderText({
    paste0(MixedFish[6, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$MixedFish_2 <-renderText({
    paste0(MixedFish[7, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$MixedFish_3 <-renderText({
    paste0(MixedFish[8, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  #(Text in 3 was split)
  # output$MixedFish_3A <-renderText({
  #   paste0(MixedFish[9, which(colnames(MixedFish)==paste0("X", input$year))])
  # })
  output$MixedFish_4 <-renderText({
    paste0(MixedFish[10, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$MixedFish_5 <-renderText({
    paste0(MixedFish[11, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$MixedFish_6 <-renderText({
    paste0(MixedFish[12, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  
  ## 2021 'Fisheries Overview' Text: tab in Ecosystem Overview section. ## SM
  output$Fisheries_1 <-renderText({
    paste0(MixedFish[13, which(colnames(MixedFish)==paste0("X", input$year))])
  })
 
  #IRISH SEA TEXT (This was added in 2023)
  #IrishSea
  output$MixedFish_7 <-renderText({
    paste0(MixedFish[14, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  #IS_2
  output$MixedFish_8 <-renderText({
    paste0(MixedFish[15, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  #IS_3
  output$MixedFish_9 <-renderText({
    paste0(MixedFish[16, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  # #IS_3A (Text in 3 was split)
  # output$MixedFish_9A <-renderText({
  #   paste0(MixedFish[17, which(colnames(MixedFish)==paste0("X", input$year))])
  # })
  #IS_4
  output$MixedFish_10 <-renderText({
    paste0(MixedFish[18, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  #IS_5
  output$MixedFish_11 <-renderText({
    paste0(MixedFish[19, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  #IS_6
  output$MixedFish_12 <-renderText({
    paste0(MixedFish[20, which(colnames(MixedFish)==paste0("X", input$year))])
  }) 
  
  #IMAGES
  ##Ecosystem Overview IMAGES ## (in 2023 this was for the Celtic Sea)
  output$MF_Fig1_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Fig1_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 600))
  }, deleteFile = FALSE)
  output$MF_Fig2_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Fig2_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 1000))
  }, deleteFile = FALSE)
  output$MF_Fig3_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Fig3_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 475))
  }, deleteFile = FALSE)
  output$MF_Fig4_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Fig4_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  output$MF_Tbl1 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl1.png")
    return(list(src = image_file, filetype = "image/png", height = 700))
  }, deleteFile = FALSE)
  output$MF_Tbl2 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl2.png")
    return(list(src = image_file, filetype = "image/png", height = 200))#
  }, deleteFile = FALSE)
  output$MF_Tbl3 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl3.png")
    return(list(src = image_file, filetype = "image/png", height = 400))
  }, deleteFile = FALSE)
  output$MF_Tbl4 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl4.png")
    return(list(src = image_file, filetype = "image/png", height = 500)) #2021 height = 400, 
  }, deleteFile = FALSE)
  output$MF_Tbl5 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl5.png")
    return(list(src = image_file, filetype = "image/png", height = 400)) #2021 height = 300
  }, deleteFile = FALSE)
  output$MF_Tbl6 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl6.png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  output$MF_Tbl7 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl7.png")
    return(list(src = image_file, filetype = "image/png", height = 225))
  }, deleteFile = FALSE)
  output$MF_Tbl8 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/MF_Tbl8.png")
    return(list(src = image_file, filetype = "image/png", height = 400))
  }, deleteFile = FALSE)
  
  #2023 - IRISH SEA
  output$IS_Fig1_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Fig1_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 550))
  }, deleteFile = FALSE)
  output$IS_Fig2_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Fig2_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 900))
  }, deleteFile = FALSE)
  output$IS_Fig3_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Fig3_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 550))
  }, deleteFile = FALSE)
  output$IS_Fig4_caption <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Fig4_caption.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$IS_Tbl1 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl1.png")
    return(list(src = image_file, filetype = "image/png", height = 550))
  }, deleteFile = FALSE)
  output$IS_Tbl2 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl2.png")
    return(list(src = image_file, filetype = "image/png", height = 200))#
  }, deleteFile = FALSE)
  output$IS_Tbl3 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl3.png")
    return(list(src = image_file, filetype = "image/png", height = 275))
  }, deleteFile = FALSE)
  output$IS_Tbl4 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl4.png")
    return(list(src = image_file, filetype = "image/png", height = 375)) #2021 height = 400, 
  }, deleteFile = FALSE)
  output$IS_Tbl5 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl5.png")
    return(list(src = image_file, filetype = "image/png", height = 400)) #2021 height = 300
  }, deleteFile = FALSE)
  output$IS_Tbl6 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl6.png")
    return(list(src = image_file, filetype = "image/png", height = 175))
  }, deleteFile = FALSE)
  output$IS_Tbl7 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl7.png")
    return(list(src = image_file, filetype = "image/png", height = 200))
  }, deleteFile = FALSE)
  output$IS_Tbl8 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IS_Tbl8.png")
    return(list(src = image_file, filetype = "image/png", height = 325))
  }, deleteFile = FALSE)
  

  ############################
  # Recent Ecosystems Advice #
  ############################
  #In 2023 'Recent_Middle2' and 'Recent_End2' was added
  ExtraChapters=read.csv('ExtraChapters.csv', header=TRUE)
  output$Recent_Beginning <- renderText({
    paste0(ExtraChapters[1, which(colnames(ExtraChapters)==paste0("X", input$year))])
  })
  output$Recent_Middle <- renderText({
    paste0(ExtraChapters[2, which(colnames(ExtraChapters)==paste0("X", input$year))])
  })
  output$Recent_Middle2 <- renderText({
    paste0(ExtraChapters[3, which(colnames(ExtraChapters)==paste0("X", input$year))])
  })
  output$Recent_End<- renderText({
    paste0(ExtraChapters[4, which(colnames(ExtraChapters)==paste0("X", input$year))])
  })  
  output$Recent_End2<- renderText({
    paste0(ExtraChapters[5, which(colnames(ExtraChapters)==paste0("X", input$year))])
  })
  #SMNov2023: if more than text is used in future years, then see the 2022 app.R "CovidResponse"
   output$AtSea_Text<- renderText({
     paste0(ExtraChapters[7, which(colnames(ExtraChapters)==paste0("X", input$year))])
   })
  output$AtSeaFootnote<- renderText({
    paste0(ExtraChapters[8, which(colnames(ExtraChapters)==paste0("X", input$year))])
  }) 
  
  #2023 text for Cod sub-stocks
  output$NoAssessment_text<- renderText({
    paste0(ExtraChapters[9, which(colnames(ExtraChapters)==paste0("X", input$year))])
  }) 
  
  
  #2021 and 2022 Specific - IMAGES added for the three extra chapters in www/MixedFisheries
  #2023 - RecentAdvice image 4 was added. 
  #       Images 1, 2, 3, 4 were 350, 350, 400, 350 for previous years.
  #       Changed to 500 for 2023
  output$RecentAdvice1 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/RecentAdvice1.png")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  output$RecentAdvice2 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/RecentAdvice2.png")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  output$RecentAdvice3 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/RecentAdvice3.png")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  output$RecentAdvice4 <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/RecentAdvice4.png")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  
  
  ################
  # Stock Advice #    
  ################
  
  #Species Summary 
  #~~~~~~~~~~~~~~~
  output$biology_text <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    #paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Description"])
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Description" ])
  })
  
  #Fish sketch
  #~~~~~~~~~~~
  output$display.fish <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    #image_file <- paste0("www/FishSketches/", 
    #       ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], ".png")
    image_file <- paste0("www/FishSketches/", 
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], ".png")
    return(list(src = image_file, filetype = "image/png",width = 200))
  }, deleteFile = FALSE)
  
  
  #International Landings
  #~~~~~~~~~~~~~~~~~~~~~~
  output$display.InternationalLandings <- renderImage({
    image_file <- paste0("www/Internationallandings/", input$year, "/Rect",
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"FAQCode"], 
                         ".png", sep="")
    return(list(src = image_file, width = 350))
  }, deleteFile = FALSE)

    
  output$display.IntlLdgs_x2Seabass <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    image_file <- paste0("www/Internationallandings/", input$year, "/Satellite",
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"FAQCode"], 
                         ".png", sep="")
    return(list(src = image_file, width = 750))#
  }, deleteFile = FALSE)
  
  
  
  output$text.InternationalLandings <- renderText({
    
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    ## 2023 uses a different year range (SM Nov2023)
    if(input$year>2022){
      
      if(input$speciesfilter=="Nephrops"){      
        paste0("The distribution of international landings of <em>",
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], "</em>  between 2017 - 2021", sep="")
      }
      #hom_mac_whb
      else if(input$speciesfilter=="Mackerel" | input$speciesfilter=="Blue Whiting" | input$speciesfilter=="Horse Mackerel"){      
        paste0("The distribution of international landings of ", 
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
               " in 2022", sep="")
      }
      # #HOM incl. with above in 2023
      # else if(input$speciesfilter=="Horse Mackerel"){      
      #   paste0("The distribution of international landings of ", 
      #          ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
      #          " in 2021", sep="")
      # }
      #boc_
      else if(input$speciesfilter=="Boarfish"){      
        paste0("The distribution of Irish ", 
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
               " landings in 2022", sep="")
      }
        # SM added in Nov 2021
        else if(input$speciesfilter=="Anglerfish"){
          paste0("The distribution of EU landings of ",
                 ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],
                 " between 2017 - 2021 (both species combined)", sep="")
        }
          
        else if(input$speciesfilter=="Megrim"){
          paste0("The distribution of EU landings of ",
                  ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],
                  " (mostly <em>L. whiffiagonis</em>) between 2017 - 2021", sep="")
        }
            
        else if(input$speciesfilter=="Saithe"){
           paste0("The distribution of EU landings of ",
                   ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],
                  " between 2017 - 2021", sep="")
        }
      
      #in 2023 Albacore & Bluefin Tuna had different year ranges
      else if(input$speciesfilter=="Albacore Tuna" | input$speciesfilter=="Bluefin Tuna"){      
        paste0("The distribution of international landings of ", 
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
               " between 2017 - 2022", sep="")
      }

        else{
          paste0("The distribution of international landings of ",
                 ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], " between 2017 - 2021 ", sep="")
        }
     }
    })
    
  
  
  #Irish Landings
  #~~~~~~~~~~~~~~
  output$display.IrishLandings <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    image_file <- paste0("www/Irishlandings/", input$year, "/Land",
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"FAQCode"], 
                         ".png", sep="")
    return(list(src = image_file, width = 350))
  }, deleteFile = FALSE)
  
  output$text.IrishLandings <- renderText({
    
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    ## 2023 uses a different year range (SM Nov2023)
    if(input$year > 2022){
      if(input$speciesfilter=="Nephrops"){      
        paste0("The distribution of <em>", 
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
               "</em> landings by Irish Vessels between 2017 - 2021", sep="")
      } 
      # #hom_mac_whb
      else if(input$speciesfilter=="Mackerel" | input$speciesfilter=="Horse Mackerel" | input$speciesfilter=="Blue Whiting"){      
        paste0("The distribution of Irish landings of ", 
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
               " in 2022", sep="")
      }
      
      #SM added Nov 24th 2021 and updated in 2022/2023
      else if(input$speciesfilter=="Boarfish"){      
        paste0("The distribution of Irish ", 
               ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
               " landings between 2020-2022", sep="")
      }
      
          # SM added in Nov 2021 and updated in 2022/2023
          else if(input$speciesfilter=="Anglerfish"){
            paste0("The distribution of Irish ",
                   ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],
                   " landings between 2017 - 2021 (both species combined)", sep="")
          }
      
          #in 2023 Albacore Tuna had a different year range
          else if(input$speciesfilter=="Albacore Tuna"){      
            paste0("The distribution of ", 
                   ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
                   " landings by Irish Vessels between 2017 - 2022", sep="")
          }
      
          #in 2023 Bluefin Tuna had a different sentence
          else if(input$speciesfilter=="Bluefin Tuna"){      
            paste0("The distribution of ", 
                   ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
                   " bycatches by Irish commercial vessels (mainly pelagic otter trawls), between 2017-2022.", sep="")
          }      

          else if(input$speciesfilter=="Megrim"){
            paste0("The distribution of ",
                   ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],
                   " landings by Irish vessels (nearly exclusively <em>L. whiffiagonis</em>) between 2017 - 2021", sep="")
          }
      
          else{
            paste0("The distribution of ",
                   ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],
                   " landings by Irish Vessels between 2017 - 2021", sep="")
          }
    }
    
  })
  
  
  #Landings by division & Value Time Series Text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$LandingsText <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area

    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                     paste0("TAC", as.numeric(as.character(input$year)), sep="")])
  })
  output$DivisionsText <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    if(input$speciesfilter=="Nephrops"){      
      paste0("The distribution of <em>", 
             ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
             "</em> landings by Irish vessels by division for the last three years.", sep="")
    }else{
      paste0("The distribution of ", 
             ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
             " landings by Irish vessels by division for the last three years.", sep="")
    }
  })
  
  output$ValueText <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    ## 2022 onwards uses landings instead of values (SM Oct2022) ## See 2022_Git if 'value needs to be re-used
    if (input$speciesfilter=="Nephrops"){      
      paste0("A historical view of <em>", 
             ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
             "</em> landings", sep="")
    }else{
      paste0("A historical view of ", 
             ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"], 
             " landings.", sep="")
    }
  })
  
  #Landings by division & Value Time Series Images
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$display.LandingsbyDiv <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    image_file <- paste0("www/LandingsByDivision/", input$year, "/",
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],".png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  
  #Value of TAC
  output$display.TACValue <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    image_file <- paste0("www/ValueTimeSeries/", input$year, "/", 
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"Fish"],".png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  
  
  #####################
  # Adding MI Summary #  
  #####################
  #SM Oct 2022: In 2022 the layout of the Summary page changed, to allow for a pdf page to be called when a link is clicked 
  output$display.SummaryPage <- renderImage({
    #Call the thumbnail png of the Summary page here. (Lines below calls the pdf)
    req(input$year>=2022)
    image_file <- paste0("www/SummaryPage/", input$year, "/",
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".png") #, sep=""
    #print(image_file)
    return(list(src = image_file, filetype = "image/png", height = 400))
    
  }, deleteFile = FALSE)
  
  
  output$display.SummaryPDF <- renderImage({
    #Call the pdf of the Summary page here. 
    req(input$year >=2022)
    image_file <- paste0("www/SummaryPDF/", input$year, "/",
                         ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".pdf") #, sep=""
    #print(image_file)
    return(list(src = image_file, filetype = "image/pdf", height = 400))

  }, deleteFile = FALSE)
  
  
  
  #Links
  #~~~~~  
  output$Stockbooklink <-renderUI({
     if(input$year==2023){
    #   a(href=paste0("The Stock Book 2023.pdf"), #CONNECT TO A PDF UNTIL THE 2023 LINK IS READY
    #     "The Stock Book 2023",target="_blank")
      
      #The OAR link became live on 24/11/2023      (#The OAR link became live on 23/11/2022)
      a(href=paste0("http://hdl.handle.net/10793/1873"), 
        "The Stock Book 2023",target="_blank")
    }
    else{paste0("No Link Available")
    }
  })
  #In 2023 CL removed this link from the digital stockbook 
  # output$ICESlink <-renderUI({
  #   # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
  #   
  #   if(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,
  #                paste0("ICESCode",input$year, sep="")]=="Not Available"){
  #     paste0("No Link Available")
  #   }else{
  #     a(href=paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,
  #                             paste0("ICESCode",input$year, sep="")]),
  #       "ICES Advice ",target="_blank")}
  # })
  
  output$ICESlinkpdf <-renderUI({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    if(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,
                 paste0("ICESLink",input$year, sep="")]=="Not Available"){
      paste0("No Link Available")
    }else{
      a(href=paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,
                              paste0("ICESLink",input$year, sep="")]),
        "ICES Advisory Sheet ",target="_blank")}
  })
  
  
  #Nephrops
  #~~~~~~~~
  output$nephrops <- renderImage({
    image_file <- paste0("www/Nephrops/advicechangeplot",input$year,".png", sep="")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  
  
  ######################
  # Adding ICES output 
  ######################
  #ICCATlist=c("North Atlantic", "East Atlantic and Mediterranean Sea")
  #output$advicelabel <-renderText ({
  #  if(input$speciesbydiv %in% ICCATlist){
  #    paste0("ICCAT Advice")
  #  }else{
  #    paste0("ICES Advice")
  #  }
  #})
  
  output$ICESAdviceTextMI <- renderText({
    paste0("Advice for ", as.numeric(as.character(input$year))+1)
  })
  output$ICESAdviceTextMI2 <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
  
    {
      paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                       paste0("FEASAdvice", as.numeric(as.character(input$year)), sep="")])
    }
  })
  output$ICESAdviceText <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                     paste0("ICESAdvice", as.numeric(as.character(input$year)), sep="")])
  })
  output$ices_ref <- renderText({
    paste0("Reference: ICES Stock Database, Extraction date: November of STOCK(S)/ASSESSMENT ", 
           input$year,". ICES, Copenhagen" )
  })
  
  output$ICESCategory <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                     paste0("Category", as.numeric(as.character(input$year)), sep="")])
  })
  output$ICESFrequency <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                     "Frequency"])
  })
  output$ManagementPlan <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                     "ManagementPlan"])
  })
  output$ICESAdviceBasis <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,  
                     "ICESAdviceBasis"])
  })
  output$Text.Stock <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"SpeciesByDivOrig"])
  })
  
  output$ICESinfo <- renderUI(
    actionButton("click", "Additional ICES Information")
  )
  
  observeEvent(input$click, {
    showModal(modalDialog(
      title = textOutput("Text.Stock"),
      h4("ICES Stock Data Category:"),
      textOutput("ICESCategory"),
      HTML("<hr>"),
      h4("ICES Advice Basis:"),
      textOutput("ICESAdviceBasis"),
      #SM updated on Nov 22nd 2021 with latest document found. 
      #a(href=paste0("https://www.ices.dk/sites/pub/Publication%20Reports/Advice/2021/2021/Advice_on_fishing_opportunities.pdf"),
      #  "Link to description of ICES Advice Basis",target="_blank"),
      #the link is protected so a doi is used but opens on a higher level page
      #In 2023 opened on the page
      a(href=paste0("https://ices-library.figshare.com/articles/report/Advice_on_fishing_opportunities_2023_/22240624"),
      #2022: a(href=paste0("https://doi.org/10.17895/ices.advice.19928060"),
        "Link to description of ICES Advice Basis",target="_blank"),
      #SM updated on Oct 28th 2021 with latest document found. (None for 2020 or 2021)
      #a(href=paste0("http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2019/2019/Introduction_to_advice_2019.pdf"),
      # "Link to description of ICES Advice Basis",target="_blank"),
      #a(href=paste0("http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2016/2016/Introduction_to_advice_2016.pdf"),
      #  "Link to description of ICES Advice Basis",target="_blank"),
      HTML("<hr>"),
      h4("ICES Advice Frequency:"),
      textOutput("ICESFrequency"),
      HTML("<hr>"),
      h4("Management Plan:"),
      textOutput("ManagementPlan"),
      easyClose = TRUE))####fluidRow(column(width=12)))
    
  })
  
  

  #Stock Development Over Time
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$display.Stock_Dev <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area

    #OCT 2023 SM: In 2023 Hans copied 'summary_' (SAG) and 'status_' images to one folder. Initially SM wrote code to remove those appendages
    #SM then decided to add the appendages to the file path in case HG repeats the extraction in following years.
      {
      image_file <- paste0("www/ICES/SAG/", input$year, "/summary_", 
                           ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".png")
      }
    return(list(src = image_file, filetype = "image/png", height = 400))
  }, deleteFile = FALSE)
  
  output$Text.Stock_Dev <- renderText({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    paste0(ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,
                     paste0("StockDev",input$year, sep="")])
  })
  
  #Stock and Exploitation status
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$display.ICESStatus <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    {
      #OCT 2023 SM: In 2023 Hans copied 'summary_' (SAG) and 'status_' images to one folder. Initially SM wrote code to remove those appendages
      #SM then decided to add the appendages to the file path in case HG repeats the extraction in folowing years.
      image_file <- paste0("www/ICES/Status/", input$year, "/status_",
                           ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".png")
    }
    return(list(src = image_file, filetype = "image/png", width = 750))
  }, deleteFile = FALSE)
  
  #SM Nov 2021: Nephrops cannot be italicised 
  output$Text.ICESStatus <- renderText({
    paste0(input$speciesfilter, " in ", input$speciesbydiv,
           ". State of Stock and fishery relative to reference points.")
  })
  
    
  
  #Quality of Assessment
  #~~~~~~~~~~~~~~~~~~~~~
  output$Text.Quality <- renderText({
    paste0(input$speciesfilter, " in ", input$speciesbydiv,
           ". Historical assessment results.")
  })
  
  output$display.SSB_Hist <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    {
      image_file <- paste0("www/ICES/SSB/", input$year, "/", 
                           ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".png")}
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  output$display.Fish_Mort <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    {
      image_file <- paste0("www/ICES/Fishmort/", input$year, "/", 
                           ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],
                           ".png")}
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  output$display.Recruit_Hist <- renderImage({
    # djc 10/11/21 - Filtering was previously only done by area description! - Fixed to filter by species and area
    
    {
      image_file <- paste0("www/ICES/RecruitHist/", input$year, "/", 
                           ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".png")}
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
 
  
  ###############
  # Forecasting #
  ###############
  #setwd("H:/Stockbook/shiny/WIP")
  Forecasting=read.csv('ForecastingData.csv', header=TRUE, stringsAsFactors=FALSE)
  Forecasting$value <- as.numeric(as.character(Forecasting$value))
  # Djc Basis was brought in as a factor
  Forecasting$Basis <- as.character(Forecasting$Basis)
  # djc Forecasting$value <- as.numeric(Forecasting$value)
  Forecasting$Year <- as.numeric(Forecasting$Year)
  Forecasting = Forecasting[,2:6]
  output$ForecastOptionsSelector <- renderUI({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    #sbl <- filter(Forecasting, 
    #              FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    #print(input$speciesbydiv)
    #print(paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    #print(unique(Forecasting$FishStock))
    ## djc
    #sbl <- filter(Forecasting, FishStock=='cod.27.6a')
    Options <- unique(sbl$Basis)
    Options <- Options[Options!= "Assessment"]
    Options <- Options[Options!= "ICES Advice"]
    Options <- Options[Options!= "TAC"]
    # djc 23/11/21 Sort the options alphabetically - should match forecast table then
    Options <- sort(Options)
    checkboxGroupInput("forecastoptionselection", h3("Select Forecast Options"), as.list(Options) ,
                       inline = TRUE) #, selected = "F = F2017"
  })

  
  # djc 23/11/21
  #mypalette<-primary.colors(length(factor(Forecasting$Basis)))
  mypalette<-primary.colors(length(unique(Forecasting$Basis)))
  #print(mypalette)
  
  
  #output$plotforecasting <- renderPlotly({
  output$plotSSB <- renderPlotly({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    # sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    ## djc
    #sbl <- filter(Forecasting, FishStock=='cod.27.6a')
    ssb <- filter(sbl, var=="SSB")
    Blim <- filter(sbl, var=="Blim")[1,5]
    Bpa <- filter(sbl, var=="Bpa")[1,5]
    ssb1 <- filter(ssb, Basis %in% c("Assessment", "ICES Advice"))
    #print(ssb1)
    #ssb2018= data.frame(FishStock=ssb1[1,1], Year=2018, Basis="Assessment", 
    #                    var="SSB", value=ssb1[ssb1$Year==2018 & ssb1$Basis=="ICES Advice",][,5])
    ssb2 <- filter(ssb, Basis %in% c(input$forecastoptionselection))
    ## djc
    #ssb2 <- filter(ssb, Basis %in% c('F=FMSY'))
    ssb3 <- rbind(ssb1, ssb2)#, ssb2018
    
    if (length(ssb3[ssb3$Year==2023 & ssb3$Basis=="Assessment",][,5])>0){
      ssb3[ssb3$Year==2023 & ssb3$Basis=="Assessment",][,5] <- head(ssb3[ssb3$Year==2023 & ssb3$Basis=="ICES Advice",][,5],1)
    }
    # SM Oct2023: Changed 2022 to 2023
    
    # djc 23/11/21 Use a defined ordering for the plots to stop the lines changing color
    myCustomOrder <- unlist(lapply(sbl$Basis, FUN = function(x) switch(x, "Assessment" = 1, "ICES Advice" = 2, "TAC" = 3, 4)))
    myPlotFactorOrder <- unique(sbl[order(myCustomOrder,sbl$Basis),"Basis"])
    ssb3$Basis <- factor(ssb3$Basis, levels = myPlotFactorOrder)
    
    p1 <- plot_ly(ssb3, x = ~Year, y = ~value, type = 'scatter', mode = 'lines', showlegend = F, #linetype = ~Basis,
                  color = ~Basis, colors=mypalette, height=375) %>% 
      layout(hovermode="FALSE", #showlegend = FALSE,
             xaxis = list(title = 'Year', range= c(min(ssb3$Year), max(ssb3$Year)+1)),
             yaxis = list (title = 'SSB', range = c(0, max(ssb3$value, na.rm = T)*1.05)),
             shapes = list(
               list(type = "rect", fillcolor = "green", opacity = 0.2, 
                    line = list(color = "green", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Bpa, y1 = max(ssb$value, na.rm=TRUE)*1.05, yref = "value"),
               list(type = "rect", fillcolor = "orange", opacity = 0.2, 
                    line = list(color = "orange", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Blim, y1 = Bpa, yref = "value"),
               list(type = "rect", fillcolor = "red", opacity = 0.2, 
                    line = list(color = "red", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = 0, y1 = Blim, yref = "value")))
    p1$elementId <- NULL
    p1
  })
  
  output$plotF <- renderPlotly({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    #sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    f <- filter(sbl, var=="F")
    
    if (length(f[f$Year==2023 & f$Basis=="Assessment",][,5])>0){
      f[f$Year==2023 & f$Basis=="Assessment",][,5] <- head(f[f$Year==2023 & f$Basis=="ICES Advice",][,5],1)
    }
    # SM Oct2023: Changed 2022 to 2023
    Fmsy <- filter(sbl, var=="Fmsy")[1,5]
    Fpa <- filter(sbl, var=="Fpa")[1,5]
    f1 <- filter(f, Basis %in% c("Assessment", "ICES Advice"))
    f2 <- filter(f, Basis %in% c(input$forecastoptionselection))
    f3 <- rbind(f1,f2) 
    
    # djc 23/11/21 Use a defined ordering for the plots to stop the lines changing color
    myCustomOrder <- unlist(lapply(sbl$Basis, FUN = function(x) switch(x, "Assessment" = 1, "ICES Advice" = 2, "TAC" = 3, 4)))
    myPlotFactorOrder <- unique(sbl[order(myCustomOrder,sbl$Basis),"Basis"])
    f3$Basis <- factor(f3$Basis, levels = myPlotFactorOrder)
    
    p2 <- plot_ly(f3, x = ~Year, y = ~value, type = 'scatter', mode = 'lines', showlegend = F,# linetype = ~Basis,
                  color = ~Basis, colors=mypalette, height=375) %>% 
      layout(hovermode="FALSE", #showlegend = TRUE,
             xaxis = list(title = 'Year', range= c(min(f3$Year), max(f3$Year)+1)),
             yaxis = list (title = 'F', range = c(0, max(f3$value, na.rm = T)*1.05)),
             #margin = list(l = 25, r = 25, b = 25, t = 25, pad = 4),
             shapes = list(
               list(type = "rect", fillcolor = "green", opacity = 0.2, 
                    line = list(color = "green", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = 0, y1 = Fmsy, yref = "value"),
               list(type = "rect", fillcolor = "orange", opacity = 0.2, 
                    line = list(color = "orange", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Fmsy, y1 = Fpa, yref = "value"),
               list(type = "rect", fillcolor = "red", opacity = 0.2, 
                    line = list(color = "red", opacity=0.2), x0 = min(sbl$Year), x1 = max(sbl$Year)+1, 
                    xref = "Year", y0 = Fpa, y1 =max(f3$value, na.rm=TRUE)*1.05, yref = "value")))
    p2$elementId <- NULL
    p2
  })
  
  output$plotLandings <- renderPlotly({
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    #sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    sbl <- filter(Forecasting, FishStock==stockToFilter)
    la <- filter(sbl, var %in% c("Landings", "TAC"))
    #print(la)
    yaxislabel="Landings"
    if(dim(la[la$Year==2023 & la$Basis=="Assessment",])[1]==0){
      # DJC if(dim(la[la$Year==2018 & la$Basis=="Assessment",])[1]==0){
      la2023= data.frame(FishStock=la[1,1], Year=2023, Basis="Assessment", 
                         var="Landings", value=la[la$Year==2023 & la$Basis=="ICES Advice",][,5])
      la=rbind(la, la2023)
    }else{
      la[la$Year==2023 & la$Basis=="Assessment" & la$var =="Landings",][,5] <- la[la$Year==2023 & la$Basis=="F = F2023" & la$var =="Landings",][,5]
      # DJC la[la$Year==2018 & la$Basis=="Assessment" & la$var =="Landings",][,5] <- la[la$Year==2018 & la$Basis=="F = F2018" & la$var =="Landings",][,5]
    }
    if(is.na(la[which(la$Basis=="ICES Advice"),"value"])[1]){
      la <- filter(sbl, var %in% c("Catch", "TAC"))
      yaxislabel="Total Catch"
      la[la$Year==2023 & la$Basis=="Assessment" & la$var =="Catch",][,5] <- la[la$Year==2023 & la$Basis=="F = F2023" & la$var =="Catch",][,5]
      # DJC la[la$Year==2018 & la$Basis=="Assessment" & la$var =="Catch",][,5] <- la[la$Year==2018 & la$Basis=="F = F2018" & la$var =="Catch",][,5]
    }
    la1 <- filter(la, Basis %in% c("Assessment", "ICES Advice", "TAC"))
    la2 <- filter(la, Basis %in% c(input$forecastoptionselection))
    la3 <- rbind(la1, la2)
    
    # djc 23/11/21 Use a defined ordering for the plots to stop the lines changing color
    myCustomOrder <- unlist(lapply(sbl$Basis, FUN = function(x) switch(x, "Assessment" = 1, "ICES Advice" = 2, "TAC" = 3, 4)))
    myPlotFactorOrder <- unique(sbl[order(myCustomOrder,sbl$Basis),"Basis"])
    la3$Basis <- factor(la3$Basis, levels = myPlotFactorOrder)
    
    p3 <- plot_ly(la3, x = ~Year, y = ~value, type = 'scatter', mode = 'lines', showlegend = T, #linetype = ~factor(var), 
                  color = ~Basis, colors=mypalette, height=375) %>% 
      layout(hovermode="FALSE", #showlegend = TRUE,
             xaxis = list(title = 'Year', range= c(min(la3$Year), max(la3$Year)+1)),
             yaxis = list (title = yaxislabel, range = c(0, max(la3$value, na.rm = T)*1.05)))
    p3$elementId <- NULL
    p3
    #subplot(p1, p2, p3)
  })
  
  #Forecasting table
  #~~~~~~~~~~~~~~~~~
  # djc 15/11/21 - don't convert strings to factors
  ForecastingTable=read.csv('ForecastOptionsV2.csv', header=TRUE, stringsAsFactors=FALSE)
  
  # djc 23/11/21 Sort to ensure ICES advice is always first for each stock
  ForecastingTable <- ForecastingTable[order(ForecastingTable$FishStock,ForecastingTable$Options,ForecastingTable$Basis),]
  
  # DJC Get rid of the X column - we don't need it
  # SM Oct2023: Updated year dates by +1
  ForecastingTable$X <- NULL
  ForecastingTable=ForecastingTable[,c(1,3,4,5,6,7,10,11,13)]#3 missing)]#  was col 12 but that is '%TAC change'. Col 13 '% Advice Change' is needed
  ForecastingTable$Catch...2024=formatC(as.numeric(as.character(ForecastingTable$Catch...2024)), format="d", big.mark=",")
  ForecastingTable$Landings...2024=formatC(as.numeric(as.character(ForecastingTable$Landings...2024)), format="d", big.mark=",")
  ForecastingTable$Discards...2024=formatC(as.numeric(as.character(ForecastingTable$Discards...2024)), format="d", big.mark=",")
  ###~~~~~
  #SM Nov2023: format="d" gives an integer. 
  #In 2023 the column "Ftotal (2024)" was changed from 2 to 3 decimal places. format was first changed to 'f' - floating
  #In 2023 the column "% SSB Change" and "% Advice Change" were changed to 1 decimal place
  #ForecastingTable$F...2024=formatC(as.numeric(as.character(ForecastingTable$F...2024)), format="f", digits = 3)
  ForecastingTable$F...2024=formatC(as.numeric(as.character(ForecastingTable$F...2024)), format="f", digits = 3)
  #ForecastingTable[,7]=formatC(ForecastingTable[,7], format="f", digits = 3)
  #ForecastingTable[11]=formatC(as.numeric(as.character(ForecastingTable[11])), format="f", digits = 1)
  #ForecastingTable[13]=formatC(as.numeric(as.character(ForecastingTable[13])), format="f", digits = 1)
  ###~~~~~
  #ForecastingTable$SSB...2025=formatC(as.numeric(as.character(ForecastingTable[,"SSB.2025"])), format="d", big.mark=",") #ForecastingTable$SSB...2025
  ForecastingTable$SSB...2025=formatC(as.numeric(as.character(ForecastingTable$SSB...2025)), format="d", big.mark=",")
  colnames(ForecastingTable)=c("FishStock", "Basis", 
                               "Total Catch (2024)", 
                               "Projected Landings (2024)", "Projected Discards (2024)", 
                               "F total (2024)", "SSB (2025)",
                               "% SSB change*", "% Advice change**")
  
  # SM Nov2021: Changed "% TAC change**" to "% Advice change**", also changed 'Wanted Catch' and 'Unwanted Catch' to 'Projected Landings','Projected Discards'
  # DJC ForecastingTable$Catch...2019=formatC(as.numeric(as.character(ForecastingTable$Catch...2019)), format="d", big.mark=",")
  # DJC ForecastingTable$Landings...2019=formatC(as.numeric(as.character(ForecastingTable$Landings...2019)), format="d", big.mark=",")
  # DJC ForecastingTable$Discards...2019=formatC(as.numeric(as.character(ForecastingTable$Discards...2019)), format="d", big.mark=",")
  # DJC ForecastingTable$SSB...2020=formatC(as.numeric(as.character(ForecastingTable$SSB...2020)), format="d", big.mark=",")
  # DJC colnames(ForecastingTable)=c("FishStock", "Basis", 
  # DJC                             "Catch (2019)", 
  # DJC                             "Landings (2019)", "Discards (2019)", 
  # DJC                             "F (2019)", "SSB (2020)",
  # DJC                             "% SSB change*", "% TAC change**")
  output$Forecasting_Table <- renderTable({
    #print("test1")
    # djc 9/11/21 Filtering was previously only done by area description! - Fixed to filter by species and area
    # ForecastingFilter=filter(ForecastingTable, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    stockToFilter <- ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New" ]
    if (length(stockToFilter) == 0){
      # djc 9/11/21 Leave the function if we can't find a match - stops error messages
      # when you move between species with different valid areas
      return(NULL)
    }
    #print("test2")
    ForecastingFilter <- filter(ForecastingTable, FishStock==stockToFilter)
    #print("test3")
    ForecastingFilter[,c(-1)]
  }, options = list(autoWidth = TRUE,
                    columnDefs = list(list(width = '200px', targets = c(1)))), 
  colnames = TRUE, bordered = TRUE)
  
  # djc 15/11/21 - Fixed some issues with spaces
  #SM Nov23: This list needs to be updated annually
  ForecastingStocks= c("Seabass Divisions 4.b-c 7.a and 7.d-h (central and southern North Sea Irish Sea English Channel Bristol Channel and Celtic Sea)",
                       ##"Cod Subareas 1 and 2 (Northeast Arctic)",
                       ##"Cod Division 6.a (West of Scotland)",
                       "Cod Division 7.a (Irish Sea)",
                       "Cod Divisions 7.e-k & 7b,c (eastern English Channel and southern Celtic Seas)",
                       ##"Spurdog Sub-areas 1-14",
                       "Haddock Subarea 4 Division 6.a and Subdivision 20 (North Sea West of Scotland Skagerrak)",
                       
                       "Haddock Division7.a (Irish Sea)",
                       "Haddock Divisions 7.b-k (southern Celtic Seas and English Channel)",
                       "Herring Subareas 1 2 5 and Divisions 4.a and 14.a (the Northeast Atlantic and Arctic Ocean)",
                       "Herring Divisions 7.a South of 52 30N 7.g-h and 7.j-k (Irish Sea Celtic Sea and southwest of Ireland)",
                       "Herring Division 7.a North of 52 30N (Irish Sea)",
                       "Hake Subareas 4 6 and 7 and Divisions 3.a 8.a-b and 8.d Northern stock (Greater North Sea Celtic Seas and the northern Bay of Biscay)",
                       "Horse Mackerel Subarea 8 and Divisions 2.a 4.a 5.b 6.a 7.a-c e-k (the Northeast Atlantic)",
                       "Megrim Divisions 4.a and 6.a (northern North Sea West of Scotland)",
                       "Mackerel Subareas 1-8 and 14 and Division 9.a (the Northeast Atlantic and adjacent waters)",
                       "Megrim Divisions 7.b-k 8.a-b and 8.d (west and southwest of Ireland Bay of Biscay)",
                       "Anglerfish Lophius piscatorius in Divisions 7.b-k, 8.a-b, and 8.d (southern Celtic Seas and Bay of Biscay)",
                       "Anglerfish Lophius budegassa Divisions 7.b-k, 8.a-b and 8.d (west and southwest of Ireland and Bay of Biscay)",
                       "Plaice Division 7.a (Irish Sea)",
                       "Saithe Subareas 4 6 and Division 3.a (North Sea Rockall and West of Scotland Skagerrak and Kattegat)",  
                       "Sole Division 7.a (Irish Sea)",
                       "Sole Divisions 7.f and 7.g (Bristol Channel and Celtic Sea)", 
                       "Blue Whiting Subareas 1-9 12 and 14 (Northeast Atlantic and adjacent waters)",
                       "Whiting Division 7.a (Irish Sea)",
                       "Whiting Divisions 7.b -c and 7.e-k (southern Celtic Seas and eastern English Channel)",
                       "Cod Subarea 4, divisions 6.a and 7.d, and Subdivision 20 (North Sea, West of Scotland, eastern English Channel, and Skagerrak). Southern substock",
                       "Cod Subarea 4, divisions 6.a and 7.d, and Subdivision 20 (North Sea, West of Scotland, eastern English Channel, and Skagerrak). Northwestern substock",
                       "Cod Subarea 4, divisions 6.a and 7.d, and Subdivision 20 (North Sea, West of Scotland, eastern English Channel, and Skagerrak). Viking substock")
  
  NoAssessmentStks=c(
                       "Cod Subarea 4, divisions 6.a and 7.d, and Subdivision 20 (North Sea, West of Scotland, eastern English Channel, and Skagerrak). Southern substock",
                       "Cod Subarea 4, divisions 6.a and 7.d, and Subdivision 20 (North Sea, West of Scotland, eastern English Channel, and Skagerrak). Northwestern substock",
                       "Cod Subarea 4, divisions 6.a and 7.d, and Subdivision 20 (North Sea, West of Scotland, eastern English Channel, and Skagerrak). Viking substock")
  
  
  
  
  #SM Note Nov 29th 2021: Updated the following list. (Extra spaces removed in 2021)
  NephropsStock=c("Division 7.a Functional Unit 14 (Irish Sea East)",
                  "Division 7.a Functional Unit 15 (Irish Sea West)",
                  "Divisions 7.b-c and 7.j-k Functional Unit 16 (west and southwest of Ireland Porcupine Bank)",
                  "Division 7.b Functional Unit 17 (west of Ireland Aran grounds)",
                  "Divisions 7.a 7.g and 7.j Functional Unit 19 (Irish Sea Celtic Sea eastern part of southwest of Ireland)",
                  "Divisions 7.g and 7.h Functional Units 20 and 21 (Celtic Sea)",
                  "Divisions 7.g and 7.f Functional Unit 22 (Celtic Sea Bristol Channel)",
                  "Subarea 7, outside the Functional Units (southern Celtic Seas, southwest of Ireland)")
  
  output$tabstest <- renderUI({
    panels= if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
      list(tabPanel("Stockbook Summary"))
      
    } 
    #Spurdog and Ling have no Species Summary Page
    else if(input$speciesfilter=="Spurdog" || input$speciesfilter=="Ling"){
      list(tabPanel("Stockbook Summary", value="stockbook_summ",
                    h3(htmlOutput("ICESAdviceTextMI")),
                    htmlOutput("ICESAdviceTextMI2"),p(),
                    HTML("<br><br>"),#Adding white space             

                    #SM Oct 2022: In 2022 the layout of the Summary page changed, to allow for a pdf page to be called when a link is clicked (see lines ~1015)
                    #Click here and open the summary page in pdf format
                    #if(input$year==2022){
                    if(input$year>2022){

                      list(
                        fluidRow (column(width = 3,
                                         HTML("<br><br>"),
                                         HTML("<br><br>"),
                                         a(h4("To see the stock advice page for ", as.numeric(input$year)+1, " click here"),target="_blank",href=paste0("SummaryPage/", input$year, "/",
                                                                                                                              ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".pdf")),
                                         HTML("<br><br>")#Adding white space
                        ),
                        column(width = 6, imageOutput("display.SummaryPage")),  #, height = "50%"
                        )#end of two columns
                      )#end of list
                    }#end of +2022 layout


                    ,h3("Links"),
                    h5("Link to the Stock Book PDF:"),
                    uiOutput("Stockbooklink"),
                    #h5("Link to the ICES Species Advice page:"), (removed in 2023 by CL)
                    #uiOutput("ICESlink"),
                    h5("Link to the ICES Advisory Sheet pdf:"),
                    uiOutput("ICESlinkpdf"),
                    HTML("<br><br>")
      ))
    }
    else{
      list(tabPanel("Species Summary", value="species_summ",
                    h3("Biology"), 
                    fluidRow(column(width = 9, htmlOutput("biology_text")), 
                             column(width = 3, imageOutput("display.fish", height = "50%"))),   
                    h3("Distribution in Irish Waters"),
                    if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
                    }
                    
                    ##***The Seabass page can change from year to year. This is the 2023 page layout:
                    else if(input$speciesfilter=="Seabass"){
                      list("While found along all Irish coasts, the primary concentration of sea bass can be found in divisions 7.j, 7.g. and 7.a. Annual Marine Institute research surveys have found a clustered distribution of sea bass in the eastern Celtic Sea, particularly during the late autumn/early winter period, which are believed to be pre-spawning aggregations foraging opportunistically.",
                           fluidRow(column(width= 4, 
                                           imageOutput("display.InternationalLandings",height = "100%")),#
                                    #SM Nov 2022: the year range was added in 2022 but will show for all previous years
                                    column(width= 4,
                                           "The distribution of sea bass sampled during
                                                 research surveys in offshore waters shows
                                                 clustering in the eastern Celtic Sea and Bristol
                                                 Channel region between 2003 and 2022.")),
                           h3("Irish Landings and Fishery"),
                           #there are extra headings and text for Seabass in the TAC2023 cell in 'StockAdvice.csv'
                           htmlOutput("LandingsText"),
                          imageOutput("display.IntlLdgs_x2Seabass",height = "100%"))
                    }
                    
                    else{
                      list(fluidRow(column(width = 6, htmlOutput("text.InternationalLandings"), HTML("<br>"),
                                           imageOutput("display.InternationalLandings",height = "100%")),#
                                    column(width = 6, htmlOutput("text.IrishLandings"), HTML("<br>"),
                                           imageOutput("display.IrishLandings",height = "100%"))),
                           HTML("<br><br>"),#Adding white space
                           h3("Irish Landings and Value of TAC"),
                           htmlOutput("LandingsText"),HTML("<br>"),
                           fluidRow(column(width = 6, htmlOutput("DivisionsText"), 
                                           imageOutput("display.LandingsbyDiv", height="100%")), 
                                    column(width = 6, htmlOutput("ValueText"),
                                           imageOutput("display.TACValue", height="100%"))))},
                    HTML("<br><br>")) , 
           tabPanel("Stockbook Summary", value="stockbook_summ",
                    h3(htmlOutput("ICESAdviceTextMI")),
                    htmlOutput("ICESAdviceTextMI2"),p(),
                    HTML("<br><br>"),#Adding white space
                    
                    #SM Oct 2022: In 2022 the layout of the Summary page changed, to allow for a pdf page to be called when a link is clicked (see lines ~1015)
                    #Click here and open the summary page in pdf format
                   # if(input$year==2022){
                    if(input$year>=2022){  
                      
                      list(
                        fluidRow (column(width = 3,
                                         HTML("<br><br>"),
                                         HTML("<br><br>"),
                                         a(h4("To see the stock advice page for ", as.numeric(input$year)+1, "click here"),target="_blank",href=paste0("SummaryPage/", input$year, "/",
                                                                                                                              ICEStable[ICEStable$Fish == input$speciesfilter & ICEStable$SpeciesByDiv == input$speciesbydiv,"New"],".pdf")),
                                         HTML("<br><br>")#Adding white space
                        ),
                        column(width = 6, imageOutput("display.SummaryPage")),  #, height = "50%"
                        )#end of two columns
                      )#end of list
                    }#end of +2022 layout
                   
                    
                    ,h3("Links"),
                    h5("Link to the Stock Book PDF:"), 
                    uiOutput("Stockbooklink"),
                    #h5("Link to the ICES Species Advice page:"), (removed in 2023 by CL)
                    #uiOutput("ICESlink"), 
                    h5("Link to the ICES Advisory Sheet pdf:"), 
                    uiOutput("ICESlinkpdf"), 
                    HTML("<br><br>")
                ))}
    
    if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
    }
    
 
    else if(!(input$speciesfilter %in% c("Albacore Tuna", "Bluefin Tuna"))){
      panels[[length(panels)+1]]=tabPanel("ICES Advice", value="ices_summ",
                                          h3("ICES Stock Advice"),
                                          htmlOutput("ICESAdviceText"),
                                          HTML("<br>"),
                                          uiOutput("ICESinfo"),
                                          HTML("<br>"),
                                          tabsetPanel(id="test", type = "pills",
                                                      tabPanel("Stock Development", 
                                                               h3("Stock Development Over Time"),
                                                               htmlOutput("Text.Stock_Dev"),HTML("<br>"),
                                                               imageOutput("display.Stock_Dev")) ,
                                                      tabPanel("Stock Status", 
                                                               h3("Stock & Exploitation Status"), 
                                                               textOutput("Text.ICESStatus"),HTML("<br>"),
                                                               imageOutput("display.ICESStatus")),
                                                      #In 2023 the three Cod46a7d20 sub-stocks had no Quality of Assessment 
                                                      #an explanation was added
                                                      if(paste(input$speciesfilter, input$speciesbydiv, sep=" ") %in% NoAssessmentStks & input$year == 2023)
                                                      {
                                                      tabPanel("Quality of Assessment", 
                                                               h3("Quality of Assessment"),
                                                               tagList(htmlOutput("NoAssessment_text")))}
                                                               else{
                                                                 tabPanel("Quality of Assessment", 
                                                                 h3("Quality of Assessment"),
                                                                 textOutput("Text.Quality"), HTML("<br>"),
                                                                 fluidRow(column(width = 4, imageOutput("display.SSB_Hist")),
                                                                          column(width = 4, imageOutput("display.Fish_Mort")),
                                                                          column(width = 4, imageOutput("display.Recruit_Hist"))),
                                          
                                          "*Images may be missing due to ICES Stock Data Category or Frequency of Advice.Some tables/graphs may be missing because they are not available for all stocks",p(),
                                          textOutput("ices_ref"))}),
                                          HTML("<br><br>"))}
    
    
    
    if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
    }else 
      #print (paste(input$speciesfilter, input$speciesbydiv, sep=" "))
      if(paste(input$speciesfilter, input$speciesbydiv, sep=" ") %in% ForecastingStocks & input$year == 2023){
        #print("TEST")
        # DJC}else if(paste(input$speciesfilter, input$speciesbydiv, sep=" ") %in% ForecastingStocks){
        # DJC panels[[4]]=tabPanel("Forecasting 2019", value="ForecastingTab",
        # SM Oct2023 Changed input$year from 2022 to 2023 and changed "Forecasting 2023" to "Forecasting 2024"
        
        # In 2022 in Spurdog, a 'ghost' panel appeared between tabs number 2 and 3 when test deployed. (The issue did not show when run locally in the browser.)
        # This was because Spurdog has no biology page, but '4' was hard-coded: panels[[4]] 
        # The number '4' was replaced with 'length(panels)+1' to fix the issue
        panels[[length(panels)+1]]=tabPanel("Forecasting 2024", value="ForecastingTab",
                                            uiOutput("ForecastOptionsSelector"),
                                            fluidRow(column(width = 3 ,plotlyOutput("plotSSB", width = "100%")), 
                                                     column(width = 3 ,plotlyOutput("plotF", width = "100%")), 
                                                     column(width = 5 ,plotlyOutput("plotLandings", width = "100%"))), HTML("<br><br>"),
                                            h3("Annual Catch Options"),
                                            "The first row in the table corresponds to the ICES Advice",p(),
                                            tags$head(
                                              tags$style("td:nth-child(1) {background: #f2f2f2;}")),
                                            tableOutput("Forecasting_Table"),
                                            "* SSB 2025 relative to SSB 2024",p(),
                                            "** Advice value for 2024 relative to Advice value for 2023", HTML("<br><br>") 
                                            
                                            # SM Oct2023: Updated year dates by +1
                                            # SM Nov2021: Changed "** Landings in 2022 relative to TAC in 2021" to the above line 
                                            # DJC"* SSB 2020 relative to SSB 2019",p(),
                                            # DJC"** Landings in 2019 relative to TAC in 2018", HTML("<br><br>")
        )
      }
    
    
    
    if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
    }else if(input$year>2022){
      if(input$speciesbydiv %in% NephropsStock){
        panels[[4]]=tabPanel("% Change in Landings", value="NephropsTab",
                             h5("Figure showing the % change in landings advice for each Functional Unit
                            compared with last year."),
                             fluidRow(column(width = 6 , imageOutput("nephrops")),
                                      column(width = 3 ,
                                             h4("Areas"),
                                             "FU14 Eastern Irish Sea",p(),
                                             "FU15 Western Irish Sea",p(), 
                                             "FU16 Porcupine Bank",p(),
                                             "FU17 Aran Grounds",p(),
                                             "FU18 & 7 Other",p(),
                                             "FU19 SE and SW Coasts of Ireland",p(),
                                             "FU20-21 Labadie, Jones & Cockburn",p(),
                                             "FU22 The Smalls"),
                                      column(width = 3 ,
                                             h4("Links to the UWTV Surveys"),
                                             "No Link Available",p(), #FU14
                                             "No Link Available",p(), #FU15
                                             
                                             #FU 16 and FU17 are the only ones to have a report published in 2016
                                             if (input$year==2023){a(href="https://oar.marine.ie/handle/10793/1185","Link to UWTV for FU16",target="_blank")},p(),

                                             if (input$year==2023){a(href="https://oar.marine.ie/handle/10793/1184","Link to UWTV for FU17",target="_blank")},p(),

                                             "No Link Available",p(), #this lists 'No Link' for 'FU18 & 7 Other'
                                             
                                             if (input$year==2023){a(href="http://hdl.handle.net/10793/1332","Link to UWTV for FU19",target="_blank")},p(),
                                             
                                             if(input$year==2023){a(href="http://hdl.handle.net/10793/1330","Link to UWTV for FU2021",target="_blank")},p(),
                                             
                                             if(input$year==2023){a(href="http://oar.marine.ie/handle/10793/1331","Link to UWTV for FU22",target="_blank")},p(),

                                      )),
                             HTML("<br><br>"))}}
    do.call(tabsetPanel, panels)
    #tabsetPanel(panels)
  })
  
  #MIXED FISHERIES 2023
  #~~~~~~~~~~~~~~~~~~~
  #CELTIC SEA
  output$CelticSea_1 <-renderUI({
                  tagList(htmlOutput("MixedFish_1"),
                          htmlOutput("MixedFish_2"),
                          HTML("<br>"),
                         #  # fluidRow( column(width = 7, imageOutput("MF_Fig1_caption")), #,height = "100%"    ,height = "50%"
                         #  #           column(width = 5, imageOutput("MF_Tbl1"))),  #, height="50%"
                          imageOutput("MF_Fig1_caption"),
                          HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
                          imageOutput("MF_Tbl1"),
                          HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
                          HTML("<br><br><br><br><br><br>"),
                          htmlOutput("MixedFish_3"),
                          HTML("<br><br>"),
                          imageOutput("MF_Tbl2"),
                          #htmlOutput("MixedFish_3A"),
                          #HTML("<br><br>"),
                          imageOutput("MF_Tbl3"),
                          HTML("<br><br><br>"),
                          imageOutput("MF_Tbl4"),
                          HTML("<br><br><br><br><br><br><br><br>"),
                          imageOutput("MF_Tbl5"),
                          HTML("<br><br><br><br>"),
                         
                  )}) #end of uiOutput 'CelticSea_1'   
                         
  output$CelticSea_2 <-renderUI({
                                        
                tagList(  htmlOutput("MixedFish_4"),
                          imageOutput("MF_Fig2_caption"),
                          HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
                          HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
                          HTML("<br><br><br><br><br><br><br><br><br>"),
                          imageOutput("MF_Fig3_caption"),
                          HTML("<br><br><br><br><br><br><br><br><br>"),
                          imageOutput("MF_Tbl6"),
                          #HTML("<br><br>"),
                          htmlOutput("MixedFish_5"),
                          HTML("<br><br><br>"),
                          imageOutput("MF_Fig4_caption"),
                          HTML("<br><br><br><br><br><br><br><br>"),
                          imageOutput("MF_Tbl7"),
                          HTML("<br>"),
                          imageOutput("MF_Tbl8"),
                          HTML("<br>"),
                          htmlOutput("MixedFish_6"),
                          HTML("<br><br>"),

                   )}) #end of uiOutput 'CelticSea_2'

  #IRISH SEA
  output$IrishSea_1 <-renderUI({
    tagList(htmlOutput("MixedFish_7"),
            htmlOutput("MixedFish_8"),
            HTML("<br>"),
            imageOutput("IS_Fig1_caption"),
            HTML("<br><br><br><br><br><br><br><br><br><br>"),
            imageOutput("IS_Tbl1"),
            HTML("<br><br><br><br><br><br><br><br><br><br>"),
            imageOutput("IS_Tbl2"),
            htmlOutput("MixedFish_9"),
            HTML("<br><br>"),
            imageOutput("IS_Tbl3"),
            # HTML("<br>"),
            imageOutput("IS_Tbl4"),
            # HTML("<br><br><br>"),
            imageOutput("IS_Tbl5"),
            # HTML("<br><br><br>"),
            
    )}) #end of uiOutput 'CelticSea_1'   
  
  output$IrishSea_2 <-renderUI({
    
    tagList(  htmlOutput("MixedFish_10"),
              HTML("<br><br>"),
              imageOutput("IS_Fig2_caption"),
              HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
              HTML("<br><br><br><br><br><br><br><br><br><br><br><br>"),
              HTML("<br><br><br><br><br>"),
              imageOutput("IS_Fig3_caption"),
              HTML("<br><br><br><br><br><br><br><br><br><br><br>"),
              imageOutput("IS_Tbl6"),
              #HTML("<br><br>"),
              htmlOutput("MixedFish_11"),
              HTML("<br><br><br>"),
              imageOutput("IS_Fig4_caption"),
              HTML("<br><br><br>"),
              imageOutput("IS_Tbl7"),
              #HTML("<br>"),
              imageOutput("IS_Tbl8"),
              #HTML("<br>"),
              htmlOutput("MixedFish_12"),
              HTML("<br><br>"),
              
    )}) #end of uiOutput 'CelticSea_2'
  
  
  #SUSTAINABILITY ASSESSMENT 2022 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #(output code removed from 'tabpanel' (line ~54) to here)
  # to avoid 'Navigation Containers' warning
 
  output$SustainAss <-renderUI({
    
    tagList(
      
      htmlOutput("Sustainabilitytext"),
      "Table 1: Summary of FEAS evaluation of fishing mortality in relation 
                       to FMSY for Stocks of interest to Ireland", 
      imageOutput("Sustainabilitytable1", height="100%"),
      HTML("<br>"),
      "Table 2: Summary of FEAS evaluation of SSB in 
                       relation to biomass reference points for stocks of interest to Ireland.",
      imageOutput("Sustainabilitytable2", height="100%"),
      HTML("<br>"),
      # SM added Fig 1 image and caption in Nov 2022
      imageOutput("SustainabilityFig", height="100%"),
      textOutput("SustainabilityFigText"),
      HTML("<br>"),
      "Table 3: Details of FEAS evaluation of fishing mortality in relation to FMSY and SSB 
                       in relation to biomass reference points for stocks of interest to Ireland.",
      imageOutput("Sustainabilitytable3", height="100%"),HTML("<br>"),
      textOutput("SustainabilityTabletext"),
      imageOutput("Sustainabilitytable4", height="100%"), HTML("<br>"),  
      
      
    )                             # end of taglist
  })                                  # end of output$SustainAss
  
  ##DATA QUALITY
  ##~~~~~~~~~~~~~~~~~~~~~~~
  output$DataQuality <-renderUI({
    
    tagList(
      
      htmlOutput("DataQualitytext1"),
      imageOutput("DataQualityimage1", height="100%"),
      "Figure 1: Data Management Quality Management Framework Model", 
      HTML("<br>"),
      p(),p(),p(),
      htmlOutput("DataQualitytext2"),
      imageOutput("DataQualityimage2", height="100%"),
      "10 principles of the ICES advice",
      HTML("<br>"),
      
    )                             # end of taglist
  })                                  # end of output$DataQuality
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  #EXTRA CHAPTERS ADDED IN 2021 and continued in 2022, 2023
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ##RECENT ECOSYSTEM ADVICE
  ##~~~~~~~~~~~~~~~~~~~~~~~
  output$RecentAdvice <-renderUI({
    if(input$year==2023){
      tagList(
        fluidRow(column(width = 10, htmlOutput("Recent_Beginning")),
                 column(width = 10, imageOutput("RecentAdvice1",height = "100%")),
                 HTML("<br><br>")),
        fluidRow(column(width = 10, htmlOutput("Recent_Middle")),
                 column(width = 10, imageOutput("RecentAdvice2",height = "100%")),
                 column(width = 10, htmlOutput("Recent_Middle2")),
                 column(width = 10, imageOutput("RecentAdvice3",height = "100%"))),
        fluidRow(column(width = 10, htmlOutput("Recent_End")),
                 column(width = 10, imageOutput("RecentAdvice4",height = "100%")),
                 column(width = 10, htmlOutput("Recent_End2"))),
                 

      )                             # end of taglist
    }                                  #end of 2023 content 
    
    
  })                                  # end of output$RecentAdvice
  
    
} #closing bracket of server <- function(input, output, session) {     (line 87)

shinyApp(ui, server)
