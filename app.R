library(shiny)
library(shinythemes)
library(colorRamps)
library(dplyr)
library(plotly)
library(DT)

library(ggplot2)
library(reshape2)

availableYears<- list("2019","2018", "2017", "2016", "2015")

ui <- fluidPage(
  tags$head(includeScript("google-analytics.js")),
  theme = shinytheme("journal"),
  titlePanel("The Stock Book"),
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
                                  "Ireland's Share of the EU TAC",
                                  HTML("<br><br>"),
                                  tags$head(tags$style(type="text/css", ".test_type {font-size: 11px;}")),
                                  div(class="test_type", tableOutput("IntroTable")),
                                  textOutput("TACtext1"),
                                  plotOutput("Introplot",height="800px"),
                                  "1 Only TAC areas where Ireland has share of the TAC>0t are included",p(),
                                  textOutput("TACtext2"),
                                  HTML("<br><br>")),
                         #conditionalPanel(condition = "input.year == '2018'",
                          tabPanel("Data Quality", htmlOutput("DataQualitytext"),
                                          imageOutput("DataQualityimage", height="50%")),
                         tabPanel("ICES Rationale",htmlOutput("Rationaletext"),
                                  HTML("<br><br>")))),
              tabPanel("Long Term Management Plans", 
                       htmlOutput("LongTermManagementtext"),
                       p(),
                       imageOutput("MgtPlan", height="100%"),
                       HTML("<br><br>")),
              tabPanel("Advice Summary", textOutput("AdviceSummtext"),p(),
                       imageOutput("AdviceSummtable1", height="100%"),p(),
                       imageOutput("AdviceSummtable2", height="100%"),
                       HTML("<br><br>")),
              tabPanel("Sustainability Assessment", 
                       htmlOutput("Sustainabilitytext"),
                       "Table 1: Summary of FEAS evaluation of fishing mortality in relation 
                       to FMSY for Stocks of interest to Ireland", 
                       imageOutput("Sustainabilitytable1", height="100%"),
                       HTML("<br>"),
                       "Table 2: Summary of FEAS evaluation of SSB in 
                       relation to biomass reference points for stocks of interest to Ireland.",
                       imageOutput("Sustainabilitytable2", height="100%"),
                       HTML("<br>"),
                       "Table 3: Details of FEAS evaluation of fishing mortality in relation to FMSY and SSB 
                       in relation to biomass reference points for stocks of interest to Ireland.",
                       imageOutput("Sustainabilitytable3", height="100%"),HTML("<br>"),
                       textOutput("SustainabilityTabletext"),
                       imageOutput("Sustainabilitytable4", height="100%"), HTML("<br>")),
              tabPanel("Ecosystem Overview", uiOutput("EcosystemOverview"),
                       HTML("<br><br>")),
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
  ####################
  # Stock Advice Tab #
  ####################
  #Species Table for mapping different codes
  #setwd("H:/Stockbook/shiny/WIP") #If needing to run it
  ICEStable=read.csv('ICES-New-Old - extra species.csv', header=TRUE)
  ICEStable$Fish=as.character(ICEStable$Fish)
  ICEStable$SpeciesByDiv=as.character(ICEStable$SpeciesByDiv)
  Speciesfilter <- unique(ICEStable$Fish)

  
  
  ## Read parameter strings from the URL and change the selection appropriately
  observe({
    urlParameters <- parseQueryString(session$clientData$url_search)
    
    ## If we have a stock parameter in the URL we will try and use it to
    ## choose our default stock
    if (!is.null(urlParameters[['stock']])) {
      
      stockURLParameter <- urlParameters[['stock']]
      #stockURLParameter <- 'mon.27.78ab'
      
      #print(stockURLParameter)
      
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

      # create the DescSelector input
      output$DescSelector <- renderUI({
        SpeciesbyDiv=filter(ICEStable, Fish %in% c(input$speciesfilter))
        Descriptions <- unique(SpeciesbyDiv$SpeciesByDiv)
        selectInput("speciesbydiv", h3("Select Stock Area"), as.list(Descriptions), selected = stockParameterDiv)
      })
      
      #Show the Stock Advice tab using SELECT - this is a bit of hack to make sure the
      # user is taken to the Stock page first if a stock paramter is provided
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
  
  
  #Introduction
  Introduction=read.csv('Introduction.csv', header=TRUE)
  output$Introtext <- renderText({
    paste0(Introduction[1, which(colnames(Introduction)==paste0("X", input$year))])
  })
  output$Introtext2 <- renderText({
    paste0(Introduction[2, which(colnames(Introduction)==paste0("X", input$year))])
  })

  #Introduction Table
  IntroductionTable=read.csv('IntroductionTable.csv', header=TRUE)
  IntroductionTable$Est..Value.of.Irl.Quota=paste("\u20ac", IntroductionTable$Est..Value.of.Irl.Quota)
  IntroductionTable$Est..Value.of.EU.TAC=paste("\u20ac", IntroductionTable$Est..Value.of.EU.TAC)
  IntroductionTable$Stock= paste(IntroductionTable$Species, IntroductionTable$TAC.Area, sep=' ')
  names(IntroductionTable)=c("Year", "Species", "TAC Area", "Irl. Tonnes", 
                             "Est. Value of Irl. Quota", "Est. Value of EU TAC", "Ireland", "Other", "Stock")
  output$IntroTable = renderTable({
    IntroTable1=filter(IntroductionTable, Year==input$year)
    IntroTable1[,2:6]
  }, colnames = TRUE, bordered = TRUE,height=1600)#,width = '100%')#, height="50%", width = "60%") 
  
  output$Introplot= renderPlot({
    FilterbyYear=filter(IntroductionTable, Year==input$year & Species!="Total")
    FilterbyYear$Stock2 <- factor(FilterbyYear$Stock, as.character(FilterbyYear$Stock))
    mdat = melt(FilterbyYear, id.vars=c("Year", "Species", "TAC Area", "Irl. Tonnes", 
                                        "Est. Value of Irl. Quota", "Est. Value of EU TAC", "Stock", "Stock2"), 
                measure.vars=c("Ireland", "Other"))
    
    p4 <- ggplot(aes(y = value, x = Stock2, fill = forcats::fct_rev(variable)), data = mdat) + 
      geom_bar(stat="identity") + 
      scale_fill_manual(values=c('#C6D9F1', '#9EBD5F')) + 
      scale_x_discrete(limits = rev(levels(mdat$Stock2)))+
      coord_flip() +
      geom_text(aes(label = paste(round(mdat$value*100,0),"%",sep=""))) +
      scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
      theme(axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank())
    p4})
  
  output$TACtext1=renderText({
    paste0("% Share of ", input$year, " EU TAC by Ireland and Other EU members and approximate value of Irish Quota")
  })
  output$TACtext2=renderText({
    paste0("2 Estimated value per tonne based on ", as.numeric(input$year)-1, " average values of Irish landings in Irish ports")
  })
  
  #Rationale
  output$Rationaletext <- renderText({
    paste0(Introduction[3, which(colnames(Introduction)==paste0("X", input$year))])
  })
  
  output$DataQualitytext <- renderText({
    if(input$year>2017){
      paste0(Introduction[7, which(colnames(Introduction)==paste0("X", input$year))])
    }else{
      paste0("Data Quality Section introduced in 2018 Stockbook")
    }
    
  })
  output$DataQualityimage <- renderImage({
    image_file <- paste0("www/Introduction/DataQuality",input$year,".png")
    return(list(src = image_file, filetype = "image/png", height = 700))
  }, deleteFile = FALSE)
  #Management Plan
  output$LongTermManagementtext <- renderText({
    if(input$year>2017){
    paste0(Introduction[8, which(colnames(Introduction)==paste0("X", input$year))])
    }else
      paste0("<p>Long Term Management Strategies (Plans) in Place for Stocks of Irish Interest. </p>
             <p>History of Implementation of Strategies Shown in each case</p>")
  })
  output$MgtPlan <- renderImage({
    image_file <-if(input$year==2018){
      paste0("www/Introduction/ManagementPlan",input$year,".png")
    }else{
      paste0("www/Introduction/ManagementPlan",input$year,".PNG")}
    return(list(src = image_file, filetype = "image/png", height = 700))
  }, deleteFile = FALSE)

  #Summary of Advice
  output$AdviceSummtable1 <- renderImage({
    image_file <- paste0("www/Introduction/AdviceSumm",input$year,"table1.PNG")
    return(list(src = image_file, filetype = "image/png", width = 1100))
  }, deleteFile = FALSE)
  output$AdviceSummtable2 <- renderImage({
    image_file <- paste0("www/Introduction/AdviceSumm",input$year,"table2.PNG")
    return(list(src = image_file, filetype = "image/png", width = 1100))
  }, deleteFile = FALSE)
  output$AdviceSummtext<- renderText({
    paste0("Marine Institute Summary on the Status, Scientific Advice for ", as.numeric(input$year)+1,
         " for those Stocks of Interest to Ireland")
  })

  #Sustainability
  output$Sustainabilitytext <- renderText({
    paste0(Introduction[4, which(colnames(Introduction)==paste0("X", input$year))])
  })
  output$SustainabilityTabletext <- renderText({
    paste0("Table 4: Stocks with a status change between the ", as.numeric(input$year)-1, 
           " and ", input$year, " Stock Books.")
  })
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
  
  #Definitions
  output$Defns <- renderText({
    paste0(Introduction[5, 3])
  })
  output$Defns2 <- renderText({
    paste0(Introduction[6, 3])
  })
  
  #Mixed Fisheries Outputs
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
  
  output$SurfaceArea <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/SurfaceArea.png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  output$RelativeF <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/RelativeF.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$RelativeSSB <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/RelativeSSB.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$Kobe <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/Kobe_Plot_Final.png")
    return(list(src = image_file, filetype = "image/png", height = 550))
  }, deleteFile = FALSE)
  output$Bar <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/Bar_Plot_Final.png")
    return(list(src = image_file, filetype = "image/png", height = 550))
  }, deleteFile = FALSE)
  
  #2017 Specific
  output$guild2017 <- renderImage({
    image_file <- paste0("www/MixedFisheries/2017/cs_guild.png")
    return(list(src = image_file, filetype = "image/png", width = 650))
  }, deleteFile = FALSE)
  output$D3table <- renderImage({
    image_file <- paste0("www/MixedFisheries/2017/D3TablePart1.png")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3table2 <- renderImage({
    image_file <- paste0("www/MixedFisheries/2017/D3TablePart2.png")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3results1 <- renderImage({
    image_file <- paste0("www/MixedFisheries/2017/Results1.png")
    return(list(src = image_file, filetype = "image/png", width = 600))
  }, deleteFile = FALSE)
  output$D3results2 <- renderImage({
    image_file <- paste0("www/MixedFisheries/2017/Results2.png")
    return(list(src = image_file, filetype = "image/png", width = 600))
  }, deleteFile = FALSE)
  output$D3results3 <- renderImage({
    image_file <- paste0("www/MixedFisheries/2017/Results3.png")
    return(list(src = image_file, filetype = "image/png", width = 600))
  }, deleteFile = FALSE)
  
  #2018 Specific
  output$D3table1a <- renderImage({
    image_file <- paste0("www/MixedFisheries/2018/D3Table1a.PNG")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3table1b <- renderImage({
    image_file <- paste0("www/MixedFisheries/2018/D3Table1b.PNG")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3table1c <- renderImage({
    image_file <- paste0("www/MixedFisheries/2018/D3Table1c.PNG")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3table1d <- renderImage({
    image_file <- paste0("www/MixedFisheries/2018/D3Table1d.PNG")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3table1e <- renderImage({
    image_file <- paste0("www/MixedFisheries/2018/D3Table1e.PNG")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  output$D3table22018 <- renderImage({
    image_file <- paste0("www/MixedFisheries/2018/D3Table2.PNG")
    return(list(src = image_file, filetype = "image/png", width = 550))
  }, deleteFile = FALSE)
  
  #Mixed Fisheries
  output$MixedFish_1 <-renderText({
    paste0(MixedFish[6, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$MixedFish_2 <-renderText({
    paste0(MixedFish[7, which(colnames(MixedFish)==paste0("X", input$year))])
  })
  output$MixedFishimage <- renderImage({
    image_file <- paste0("www/MixedFisheries/", input$year, "/MixedFish.PNG")
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  #2016 Specific
  output$IrishSea <- renderImage({
    image_file <- paste0("www/MixedFisheries/",input$year,"/IrishSea-Pressure-State.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$IrishSeaFSSB <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/IrishSea-F-SSB.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$CelticSea <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/CelticSea-Pressure-State.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$CelticSeaFSSB <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/CelticSea-F-SSB.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$WestScot <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/WestScot-Pressure-State.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$WestScotFSSB <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/WestScot-F-SSB.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$DistSpecies <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/DistSpecies-Pressure-State.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  output$DistSpeciesFSSB <- renderImage({
    image_file <- paste0("www/MixedFisheries/2016/DistSpecies-F-SSB.png")
    return(list(src = image_file, filetype = "image/png", height = 350))
  }, deleteFile = FALSE)
  
  # Code moved to the start of the server function because we need it to handle URL parameters
  # ####################
  # # Stock Advice Tab #
  # ####################
  # #Species Table for mapping different codes
  # #setwd("H:/Stockbook/shiny/WIP") #If needing to run it
  # ICEStable=read.csv('ICES-New-Old - extra species.csv', header=TRUE)
  # ICEStable$Fish=as.character(ICEStable$Fish)
  # ICEStable$SpeciesByDiv=as.character(ICEStable$SpeciesByDiv)
  # Speciesfilter <- unique(ICEStable$Fish)
  # output$speciesSelector <- renderUI({
  #   selectInput("speciesfilter", h3("Select Species"), as.list(Speciesfilter), selected = "Cod") 
  # })
  # output$DescSelector <- renderUI({
  #   SpeciesbyDiv=filter(ICEStable, Fish %in% c(input$speciesfilter))
  #   Descriptions <- unique(SpeciesbyDiv$SpeciesByDiv)
  #   selectInput("speciesbydiv", h3("Select Stock Area"), as.list(Descriptions), selected = Descriptions[1])
  # })

  ###################
  # Species Summary #
  ###################
  #Biology Text
  output$biology_text <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Description"])
  })
  #Fish sketch
  output$display.fish <- renderImage({
    image_file <- paste0("www/FishSketches/", 
                        ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], ".png")
    return(list(src = image_file, filetype = "image/png",width = 200))
  }, deleteFile = FALSE)
  
  #International Landings
  output$display.InternationalLandings <- renderImage({
    image_file <- paste0("www/Internationallandings/", input$year, "/Rect",
                        ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"FAQCode"], 
                        ".png", sep="")
    return(list(src = image_file, width = 350))#
  }, deleteFile = FALSE)
  output$text.InternationalLandings <- renderText({
      if(input$speciesfilter=="Nephrops"){      
      paste0("The distribution of international landings of <em>",
          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], "</em> during ", 
          (as.numeric(as.character(input$year))-1), sep="")
    }else{
      paste0("The distribution of international landings of ",
             ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], " during ", 
             (as.numeric(as.character(input$year))-1), sep="")
    }
  })
  #Irish Landings
  output$display.IrishLandings <- renderImage({
    image_file <- paste0("www/Irishlandings/", input$year, "/Land",
                         ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"FAQCode"], 
                         ".png", sep="")
    return(list(src = image_file, width = 350))
  }, deleteFile = FALSE)
  output$text.IrishLandings <- renderText({
    if(input$speciesfilter=="Nephrops"){      
      paste0("The distribution of <em>", 
           ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], 
           "</em> landings by Irish Vessels during ", (as.numeric(as.character(input$year))-1), sep="")
    }else{
      paste0("The distribution of ", 
             ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], 
             " landings by Irish Vessels during ", (as.numeric(as.character(input$year))-1), sep="")
    }
  })
  #Landings text
  output$LandingsText <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                     paste0("TAC", as.numeric(as.character(input$year)), sep="")])
  })
  output$DivisionsText <- renderText({
    if(input$speciesfilter=="Nephrops"){      
      paste0("The distribution of <em>", 
             ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], 
             "</em> landings by Irish vessels by division for the last three years.", sep="")
    }else{
      paste0("The distribution of ", 
             ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], 
             " landings by Irish vessels by division for the last three years.", sep="")
    }
  })
  output$ValueText <- renderText({
    if(input$speciesfilter=="Nephrops"){      
      paste0("A historical view of the value of <em>", 
             ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], 
             "</em> landings", sep="")
    }else{
      paste0("A historical view of the value of ", 
             ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"], 
             " landings.", sep="")
    }
  })
  
  #Landings by division
  output$display.LandingsbyDiv <- renderImage({
    image_file <- paste0("www/LandingsByDivision/", input$year, "/",
                        ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"],".png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  #Value of TAC
  output$display.TACValue <- renderImage({
    image_file <- paste0("www/ValueTimeSeries/", input$year, "/", 
                        ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Fish"],".png")
    return(list(src = image_file, filetype = "image/png", height = 300))
  }, deleteFile = FALSE)
  
 
  #####################
  # Adding MI Summary #
  #####################
  output$display.assarea <- renderImage({
    image_file <- paste0("www/maps/",ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],
                        ".png", sep="")
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  output$display.landingsbygear <- renderImage({
    if(input$year==2016){
      image_file <- paste0("www/Landingsbygear/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],".png")
      return(list(src = image_file, filetype = "image/png", height = 250))
    }else{
      image_file <- paste0("www/Landingsbygear/", input$year, "/", 
                        ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],".png")
    return(list(src = image_file, filetype = "image/png", height = 250))}
  }, deleteFile = FALSE)

  #Pie Chart
  output$TACtext <-renderText({
    if(input$speciesfilter=="Seabass" | input$speciesfilter=="Sprat"){
      paste0("")
    }else{
    paste0(input$year, " Quota Allocations", sep="")}
  })
  output$display.TAC <- renderImage({
    image_file <- paste0("www/Quota/",input$year,"/",ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],
                        ".png", sep="")
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  #Catch/Discards plot
  output$display.CatchDiscards <- renderImage({
    image_file <- paste0("www/CatchDiscards/", input$year, "/", 
                         ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],".png", sep="")
    return(list(src = image_file, filetype = "image/png", height = 100))
  }, deleteFile = FALSE)
  
  
  #Key Points table
  KeyPoints=read.csv("KeyPoints.csv", header=TRUE)
  output$KPtable = renderTable({
    if(input$year==2015){
      paste0("There was no Key Points table in the 2015 Stock Book")
    }else{
    KPFilter=filter(KeyPoints, Year==input$year & 
                      Group == "KeyPoints" &
                      FishStock %in% paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% 
                                                             input$speciesbydiv),"New"]))
    KPFilter[,4:5]}
  }, colnames = FALSE, bordered = TRUE) 
  
  output$KPtableFootnote = renderText({
    KPFilter=filter(KeyPoints, Year==input$year & 
                      Group == "Footnotes" &
                      FishStock %in% paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% 
                                                              input$speciesbydiv),"New"]))
    paste0(KPFilter[,4])
  }) 
  
  
  #Links
  output$Stockbooklink <-renderUI({
    if(input$year==2018){
      a(href=paste0("https://oar.marine.ie/bitstream/handle/10793/1383/The%20Stock%20Book%202018.pdf"),
        "The Stock Book 2018",target="_blank")
    }else if(input$year==2017){
      a(href=paste0("http://oar.marine.ie/bitstream/10793/1337/1/The%20Stock%20Book%202017.pdf"),
                    "The Stock Book 2017",target="_blank")
    }else if(input$year==2016){
      a(href=paste0("http://oar.marine.ie/bitstream/10793/1178/1/The%20Stock%20Book%202016.pdf"),
        "The Stock Book 2016",target="_blank")
    }else if(input$year==2015){
      a(href=paste0("http://oar.marine.ie/bitstream/10793/1121/1/Stock%20Book%202015.pdf"),
        "The Stock Book 2015",target="_blank")
    }else{
      paste0("No Link Available")
    }
  })

  output$ICESlink <-renderUI({
    if(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),
                 paste0("ICESCode",input$year, sep="")]=="Not Available"){
      paste0("No Link Available")
    }else{
      a(href=paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),
                              paste0("ICESCode",input$year, sep="")]),
        "ICES Advice",target="_blank")}
  })
  
  output$ICESlinkpdf <-renderUI({
    if(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),
                 paste0("ICESCode",input$year, sep="")]=="Not Available"){
      paste0("No Link Available")
    }else{
      a(href=paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),
                              paste0("ICESLink",input$year, sep="")]),
        "ICES Advisory Sheet",target="_blank")}
  })
  
  #Management Advice/Additional Information
  output$ManagementAdviceHeader = renderText({
    paste("Management Advice in ", input$year, sep="")
  }) 
  ManagementAdvice=read.csv("ManagementAdvice.csv", header=TRUE)#, encoding = 'ASCII'
  output$ManagementAdvice = renderText({
    MAFilter=filter(ManagementAdvice, Year==input$year)
    MAFilter2=filter(MAFilter, FishStock %in% paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% 
                                                                       input$speciesbydiv),"New"]))
    paste(MAFilter2[,3], sep="")
  }) 
  output$AddInfoHeader = renderText({
    if(input$year<2018){
      "Additional Information"
    }else{
      "Key Stock Considerations"
    }
    }) 
  output$Addinfo = renderText({
    MAFilter=filter(ManagementAdvice, Year==input$year)
    MAFilter2=filter(MAFilter, FishStock %in% paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% 
                                                                       input$speciesbydiv),"New"]))
    paste(MAFilter2[,4], sep="")
  }) 

  #Nephrops
  output$nephrops <- renderImage({
    image_file <- paste0("www/Nephrops/advicechangeplot",input$year,".png", sep="")
    return(list(src = image_file, filetype = "image/png", height = 500))
  }, deleteFile = FALSE)
  
  ######################
  # Adding ICES output #
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
    if(input$year<2018){
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                    paste0("ICESAdvice", as.numeric(as.character(input$year)), sep="")])
    }else{
      paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                       paste0("FEASAdvice", as.numeric(as.character(input$year)), sep="")])
      }
  })
  output$ICESAdviceText <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                           paste0("ICESAdvice", as.numeric(as.character(input$year)), sep="")])
  })
  output$ices_ref <- renderText({
    paste0("Reference: ICES Stock Database, Extraction date: November of STOCK(S)/ASSESSMENT ", 
           input$year,". ICES, Copenhagen" )
  })
  
  output$ICESCategory <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                     paste0("Category", as.numeric(as.character(input$year)), sep="")])
  })
  output$ICESFrequency <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                     "Frequency"])
  })
  output$ManagementPlan <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                     "ManagementPlan"])
  })
  output$ICESAdviceBasis <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),  
                     "ICESAdviceBasis"])
  })
  output$Text.Stock <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"SpeciesByDivOrig"])
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
    a(href=paste0("http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2016/2016/Introduction_to_advice_2016.pdf"),
      "Link to description of ICES Advice Basis",target="_blank"),
    HTML("<hr>"),
    h4("ICES Advice Frequency:"),
    textOutput("ICESFrequency"),
    HTML("<hr>"),
    h4("Management Plan:"),
    textOutput("ManagementPlan"),
    easyClose = TRUE))
  })
  
  #Stock Development Over Time
  output$display.Stock_Dev <- renderImage({
    if(input$year<2017){
      image_file <- paste0("www/ICES/SAG/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],".png")
    }else{
      image_file <- paste0("www/ICES/SAG/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],".png")}
    return(list(src = image_file, filetype = "image/png", height = 400))
  }, deleteFile = FALSE)
  
  output$Text.Stock_Dev <- renderText({
    paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),
                     paste0("StockDev",input$year, sep="")])
  })

  #Stock and Exploitation status
  output$display.ICESStatus <- renderImage({
    if(input$year<2017){
      image_file <- paste0("www/ICES/Status/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],".png")}
    else{
      image_file <- paste0("www/ICES/Status/", input$year, "/",
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],".png")
      }
    return(list(src = image_file, filetype = "image/png", width = 750))
  }, deleteFile = FALSE)
  
  output$Text.ICESStatus <- renderText({
    paste0(input$speciesfilter, " in ", input$speciesbydiv,
           ". State of Stock and fishery relative to reference points.")
  })

  #Quality of Assessment
  output$Text.Quality <- renderText({
    paste0(input$speciesfilter, " in ", input$speciesbydiv,
           ". Historical assessment results.")
  })
  
  output$display.SSB_Hist <- renderImage({
    if(input$year<2017){
      image_file <- paste0("www/ICES/SSB/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],".png")}
    else{
      image_file <- paste0("www/ICES/SSB/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],".png")}
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  output$display.Fish_Mort <- renderImage({
    if(input$year<2017){
      image_file <- paste0("www/ICES/Fishmort/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],".png")
      }else{
        image_file <- paste0("www/ICES/Fishmort/", input$year, "/", 
                            ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],
                            ".png")}
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  output$display.Recruit_Hist <- renderImage({
    if(input$year<2017){
      image_file <- paste0("www/ICES/RecruitHist/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"Old"],".png")
    }else{
      image_file <- paste0("www/ICES/RecruitHist/", input$year, "/", 
                          ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"],".png")}
    return(list(src = image_file, filetype = "image/png", height = 250))
  }, deleteFile = FALSE)
  
  ###############
  # Forecasting #
  ###############
  #setwd("H:/Stockbook/shiny/WIP")
  Forecasting=read.csv('ForecastingData.csv', header=TRUE)
  Forecasting$value <- as.numeric(as.character(Forecasting$value))
  # djc Forecasting$value <- as.numeric(Forecasting$value)
  Forecasting$Year <- as.numeric(Forecasting$Year)
  Forecasting = Forecasting[,2:6]
  output$ForecastOptionsSelector <- renderUI({
    sbl <- filter(Forecasting, 
                  FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    Options <- unique(sbl$Basis)
    Options <- Options[Options!= "Assessment"]
    Options <- Options[Options!= "ICES Advice"]
    Options <- Options[Options!= "TAC"]
    checkboxGroupInput("forecastoptionselection", h3("Select Forecast Options"), as.list(Options) ,
                       inline = TRUE) #, selected = "F = F2017"
  })

  #sbl <-reactive({
  #  filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
  #})

  mypalette<-primary.colors(length(factor(Forecasting$Basis)))
  
  #output$plotforecasting <- renderPlotly({
  output$plotSSB <- renderPlotly({
    sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    ssb <- filter(sbl, var=="SSB")
    Blim <- filter(sbl, var=="Blim")[1,5]
    Bpa <- filter(sbl, var=="Bpa")[1,5]
    ssb1 <- filter(ssb, Basis %in% c("Assessment", "ICES Advice"))
    #ssb2018= data.frame(FishStock=ssb1[1,1], Year=2018, Basis="Assessment", 
    #                    var="SSB", value=ssb1[ssb1$Year==2018 & ssb1$Basis=="ICES Advice",][,5])
    ssb2 <- filter(ssb, Basis %in% c(input$forecastoptionselection))
    ssb3 <- rbind(ssb1, ssb2)#, ssb2018
    
    ssb3[ssb3$Year==2019 & ssb3$Basis=="Assessment",][,5] <- head(ssb3[ssb3$Year==2019 & ssb3$Basis=="ICES Advice",][,5],1)
    # DJC ssb3[ssb3$Year==2019 & ssb3$Basis=="Assessment",][,5] <- ssb3[ssb3$Year==2019 & ssb3$Basis=="ICES Advice",][,5]
    # DJC ssb3[ssb3$Year==2018 & ssb3$Basis=="Assessment",][,5] <- ssb3[ssb3$Year==2018 & ssb3$Basis=="ICES Advice",][,5]
    
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
    sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    f <- filter(sbl, var=="F")
    #if(dim(f[f$Year==2018 & f$Basis=="Assessment",])[1]==0){
    #      f2018= data.frame(FishStock=f[1,1], Year=2018, Basis="Assessment", 
    #                    var="F", value=f[f$Year==2018 & f$Basis=="ICES Advice",][,5])
    #      f=rbind(f, f2018)
    #}else{
    f[f$Year==2019 & f$Basis=="Assessment",][,5] <- head(f[f$Year==2019 & f$Basis=="ICES Advice",][,5],1)
    # DJCf[f$Year==2019 & f$Basis=="Assessment",][,5] <- f[f$Year==2019 & f$Basis=="ICES Advice",][,5]
    # DJC f[f$Year==2018 & f$Basis=="Assessment",][,5] <- f[f$Year==2018 & f$Basis=="ICES Advice",][,5]
    #}
    Fmsy <- filter(sbl, var=="Fmsy")[1,5]
    Fpa <- filter(sbl, var=="Fpa")[1,5]
    f1 <- filter(f, Basis %in% c("Assessment", "ICES Advice"))
    f2 <- filter(f, Basis %in% c(input$forecastoptionselection))
    f3 <- rbind(f1,f2) 
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
    sbl <- filter(Forecasting, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    la <- filter(sbl, var %in% c("Landings", "TAC"))
    yaxislabel="Landings"
    if(dim(la[la$Year==2019 & la$Basis=="Assessment",])[1]==0){
    # DJC if(dim(la[la$Year==2018 & la$Basis=="Assessment",])[1]==0){
      la2019= data.frame(FishStock=la[1,1], Year=2019, Basis="Assessment", 
                         var="Landings", value=la[la$Year==2019 & la$Basis=="ICES Advice",][,5])
      la=rbind(la, la2019)
      # DJC la2018= data.frame(FishStock=la[1,1], Year=2018, Basis="Assessment", 
      # DJC                   var="Landings", value=la[la$Year==2018 & la$Basis=="ICES Advice",][,5])
      # DJC la=rbind(la, la2018)
    }else{
      la[la$Year==2019 & la$Basis=="Assessment" & la$var =="Landings",][,5] <- la[la$Year==2019 & la$Basis=="F=F2019" & la$var =="Landings",][,5]
      # DJC la[la$Year==2018 & la$Basis=="Assessment" & la$var =="Landings",][,5] <- la[la$Year==2018 & la$Basis=="F = F2018" & la$var =="Landings",][,5]
    }
    if(is.na(la[which(la$Basis=="ICES Advice"),"value"])[1]){
      la <- filter(sbl, var %in% c("Catch", "TAC"))
      yaxislabel="Total Catch"
      la[la$Year==2019 & la$Basis=="Assessment" & la$var =="Catch",][,5] <- la[la$Year==2019 & la$Basis=="F=F2019" & la$var =="Catch",][,5]
      # DJC la[la$Year==2018 & la$Basis=="Assessment" & la$var =="Catch",][,5] <- la[la$Year==2018 & la$Basis=="F = F2018" & la$var =="Catch",][,5]
    }
    la1 <- filter(la, Basis %in% c("Assessment", "ICES Advice", "TAC"))
    la2 <- filter(la, Basis %in% c(input$forecastoptionselection))
    la3 <- rbind(la1, la2)
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
  ForecastingTable=read.csv('ForecastOptionsV2.csv', header=TRUE)
  # DJC Get rid of the X column - we don't need it
  ForecastingTable$X <- NULL
  ForecastingTable=ForecastingTable[,c(1,3,4,5,6,7,10,11,12)]#3 missing)]#
  ForecastingTable$Catch...2020=formatC(as.numeric(as.character(ForecastingTable$Catch...2020)), format="d", big.mark=",")
  ForecastingTable$Landings...2020=formatC(as.numeric(as.character(ForecastingTable$Landings...2020)), format="d", big.mark=",")
  ForecastingTable$Discards...2020=formatC(as.numeric(as.character(ForecastingTable$Discards...2020)), format="d", big.mark=",")
  ForecastingTable$SSB...2021=formatC(as.numeric(as.character(ForecastingTable$SSB...2021)), format="d", big.mark=",")
  colnames(ForecastingTable)=c("FishStock", "Basis", 
                               "Total Catch (2020)", 
                               "Wanted Catch (2020)", "Unwanted Catch (2020)", 
                               "F total (2020)", "SSB (2021)",
                               "% SSB change*", "% TAC change**")
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
    ForecastingFilter=filter(ForecastingTable, FishStock==paste0(ICEStable[which(ICEStable[,"SpeciesByDiv"] %in% input$speciesbydiv),"New"]))
    ForecastingFilter[,c(-1)]
  }, options = list(autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = c(1)))), 
  colnames = TRUE, bordered = TRUE)
  
  ForecastingStocks= c("Cod Divisions 7.e-k (eastern English Channel and southern Celtic Seas)",
                       #"Cod Division 6.a (West of Scotland)",
                       "Cod Division 7.a (Irish Sea)",
                       "Haddock  Division 6.b (Rockall) ",
                       "Haddock  Division7.a (Irish Sea) ",
                       "Haddock Divisions 7.b-k (southern Celtic Seas and English Channel)",
                       "Herring Divisions 7.a South of 52 30N  7.g-h  and 7.j-k (Irish Sea  Celtic Sea  and southwest of Ireland)",
                       "Hake Subareas 4  6  and 7  and Divisions 3.a  8.a-b  and 8.d  Northern stock (Greater North Sea  Celtic Seas  and the northern Bay of Biscay)",
                       "Horse Mackerel Subarea 8 and Divisions 2.a  4.a  5.b  6.a  7.a-c e-k (the Northeast Atlantic)",
                       "Mackerel Subareas 1-8 and 14 and Division 9.a (the Northeast Atlantic and adjacent waters)",
                       "Megrim  Divisions 7.b-k  8.a-b  and 8.d (west and southwest of Ireland  Bay of Biscay)",
                       "Plaice Division 7.a (Irish Sea)    ",
                       "Saithe Subareas 4  6 and Division 3.a (North Sea  Rockall and West of Scotland  Skagerrak and Kattegat)   ",
                       "Sole Divisions 7.f and 7.g (Bristol Channel and Celtic Sea)  ",
                       "Blue Whiting Subareas 1-9  12  and 14 (Northeast Atlantic and adjacent waters)",
                       #"Whiting  Division 7.a (Irish Sea)",
                       "Whiting Divisions 7.b -c and 7.e-k (southern Celtic Seas and eastern English Channel)",
                       "Anglerfish Lophius piscatorius in Divisions 7.b-k, 8.a-b, and 8.d (southern Celtic Seas and Bay of Biscay)")

  NephropsStock=c("Division 7.a  Functional Unit 14 (Irish Sea  East)", 
                  "Division 7.a  Functional Unit 15 (Irish Sea  West)",
                  "Divisions 7.b-c and 7.j-k  Functional Unit 16 (west and southwest of Ireland  Porcupine Bank)",
                  "Division 7.b  Functional Unit 17 (west of Ireland  Aran grounds)",
                  "Divisions 7.a  7.g  and 7.j  Functional Unit 19 (Irish Sea  Celtic Sea  eastern part of southwest of Ireland)",
                  "Divisions 7.g and 7.h  Functional Units 20 and 21 (Celtic Sea)",
                  "Divisions 7.g and 7.f  Functional Unit 22 (Celtic Sea  Bristol Channel)",
                  "Subarea 7 - Functional Unit 18 and rectangles outside the functional units (Southern Celtic Seas, Southwest of Ireland)")
  
  output$tabstest <- renderUI({
    panels= if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
      list(tabPanel("Stockbook Summary"))
    }else if(input$speciesfilter=="Spurdog"){
      list(tabPanel("Stockbook Summary", value="stockbook_summ",
                    h3(textOutput("ICESAdviceTextMI")),
                    htmlOutput("ICESAdviceTextMI2"),p(),
                    HTML("<br><br>"),#Adding white space
                    fluidRow(column(width = 3, imageOutput("display.assarea", height = "50%")),
                             column(width = 6, imageOutput("display.landingsbygear", height = "50%"))),
                    fluidRow(column(width = 6, 
                                    h3("Key Points"),
                                    tags$head(
                                      tags$style("td:nth-child(1) {font-weight: bold;}
                                                 td:nth-child(1) {background: #f2f2f2;}")),
                                    tableOutput("KPtable"),
                                    if(input$year>2017){
                                      list(tags$head(tags$style(HTML("
                                                                      #KPtableFootnote{
                                                                      font-size: 11px;}"))),
                                           htmlOutput("KPtableFootnote"))
                                    }),
                             column(width = 6, h3(textOutput("TACtext")),
                                    imageOutput("display.TAC", height = "50%"),
                                    if(input$year>2017){
                                      imageOutput("display.CatchDiscards", height = "25%")})),
                    tabsetPanel(id="MgtAdvice", type="pills",
                                tabPanel(textOutput("ManagementAdviceHeader"), 
                                         htmlOutput("ManagementAdvice"),p()),
                                tabPanel(textOutput("AddInfoHeader"), #"Additional Information", 
                                         htmlOutput("Addinfo"),p())),
                    h3("Links"),
                    h5("Link to the Stock Book PDF:"), 
                    uiOutput("Stockbooklink"),
                    h5("Link to the ICES Species Advice page:"), 
                    uiOutput("ICESlink"), 
                    h5("Link to the ICES Advisory Sheet pdf:"), 
                    uiOutput("ICESlinkpdf"), 
                    HTML("<br><br>")
                    ))
    }else{
      list(tabPanel("Species Summary", value="species_summ",
                          h3("Biology"), 
                          fluidRow(column(width = 9, htmlOutput("biology_text")), 
                                   column(width = 3, imageOutput("display.fish", height = "50%"))),
                          h3("Landings Distribution"),
                          if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
                          }else if(input$speciesfilter=="Seabass"){
                            list("Though distributed around the entire coastline of Ireland, sea bass are 
predominately concentrated along the southwest, south and south-east coasts. During colder periods however, 
the majority of adults are believed to move offshore to feed and spawn where research surveys have found 
a relatively clustered distribution in the eastern Celtic Sea.",
                                 fluidRow(column(width= 4, 
                                                 imageOutput("display.InternationalLandings",height = "100%")),#
                                          column(width= 4,
                                                 "The distribution of sea bass sampled during
                                                 research surveys in offshore waters shows
                                                 clustering in the eastern Celtic Sea and Bristol
                                                 Channel region.")),
                                 h3("Irish Landings and Value of TAC"),
                                 htmlOutput("LandingsText"))
                          }else{
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
                          h3(textOutput("ICESAdviceTextMI")),
                          htmlOutput("ICESAdviceTextMI2"),p(),
                          HTML("<br><br>"),#Adding white space
                          fluidRow(column(width = 3, imageOutput("display.assarea", height = "50%")),
                                   column(width = 6, imageOutput("display.landingsbygear", height = "50%"))),
                          fluidRow(column(width = 6, 
                                          h3("Key Points"),
                                          tags$head(
                                            tags$style("td:nth-child(1) {font-weight: bold;}
                                                       td:nth-child(1) {background: #f2f2f2;}")),
                                          tableOutput("KPtable"),
                                          if(input$year>2017){
                                            list(tags$head(tags$style(HTML("
                                                                      #KPtableFootnote{
                                                                      font-size: 11px;}"))),
                                            htmlOutput("KPtableFootnote"))
                                            }),
                                   column(width = 6, h3(textOutput("TACtext")),
                                          imageOutput("display.TAC", height = "50%"),
                                          if(input$year>2017){
                                          imageOutput("display.CatchDiscards", height = "25%")})),
                          tabsetPanel(id="MgtAdvice", type="pills",
                                      tabPanel(textOutput("ManagementAdviceHeader"), 
                                               htmlOutput("ManagementAdvice"),p()),
                                      tabPanel(textOutput("AddInfoHeader"), #"Additional Information", 
                                               htmlOutput("Addinfo"),p())),
                          h3("Links"),
                          h5("Link to the Stock Book PDF:"), 
                          uiOutput("Stockbooklink"),
                          h5("Link to the ICES Species Advice page:"), 
                          uiOutput("ICESlink"), 
                          h5("Link to the ICES Advisory Sheet pdf:"), 
                          uiOutput("ICESlinkpdf"), 
                          HTML("<br><br>")
                          ))}
  
  if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
  }else if(!(input$speciesfilter %in% c("Albacore Tuna", "Bluefin Tuna", "Swordfish"))){
    panels[[length(panels)+1]]=tabPanel("ICES Advice", value="ices_summ",
                         h3("ICES Stock Advice"),
                         textOutput("ICESAdviceText"),
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
                                     tabPanel("Quality of Assessment", 
                                              h3("Quality of Assessment"), 
                                              textOutput("Text.Quality"), HTML("<br>"),
                                              fluidRow(column(width = 4, imageOutput("display.SSB_Hist")),
                                                       column(width = 4, imageOutput("display.Fish_Mort")),
                                                       column(width = 4, imageOutput("display.Recruit_Hist"))))),
                         "*Images may be missing due to ICES Stock Data Category or Frequency of Advice",p(),
                         textOutput("ices_ref"),
                         HTML("<br><br>"))}
  
    
    if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
    }else if(paste(input$speciesfilter, input$speciesbydiv, sep=" ") %in% ForecastingStocks){
      # DJC panels[[4]]=tabPanel("Forecasting 2019", value="ForecastingTab",
      panels[[4]]=tabPanel("Forecasting 2020", value="ForecastingTab",
                           uiOutput("ForecastOptionsSelector"),
                           #plotlyOutput("plotforecasting"),
                           fluidRow(column(width = 3 ,plotlyOutput("plotSSB", width = "100%")), 
                                    column(width = 3 ,plotlyOutput("plotF", width = "100%")), 
                                    column(width = 5 ,plotlyOutput("plotLandings", width = "100%"))), HTML("<br><br>"),
                           h3("Annual Catch Options"),
                           tags$head(
                             tags$style("td:nth-child(1) {background: #f2f2f2;}")),
                           tableOutput("Forecasting_Table"),
                           "* SSB 2021 relative to SSB 2020",p(),
                           "** Landings in 2020 relative to TAC in 2019", HTML("<br><br>")
                           # DJC"* SSB 2020 relative to SSB 2019",p(),
                           # DJC"** Landings in 2019 relative to TAC in 2018", HTML("<br><br>")
                           )
    }
    
    if(is.null(input$speciesfilter) || is.na(input$speciesfilter)){
    }else if(input$year>2015){
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
                                         "No Link Available",p(),
                                         a(href=paste0(
                                           if(input$year==2016){"https://oar.marine.ie/handle/10793/59/recent-submissions"}
                                           else if(input$year==2017){"http://hdl.handle.net/10793/1333"}
                                           else if(input$year==2018){"https://oar.marine.ie/handle/10793/1378"}),"Link to UWTV for FU15",target="_blank"),p(),
                                         a(href=paste0(
                                           if(input$year==2016){"https://oar.marine.ie/handle/10793/1185"}
                                           else if(input$year==2017){"http://hdl.handle.net/10793/1334"}
                                           else if(input$year==2018){"https://oar.marine.ie/handle/10793/1379"}),"Link to UWTV for FU16",target="_blank"),p(),
                                         a(href=paste0(
                                           if(input$year==2016){"https://oar.marine.ie/handle/10793/1184"}
                                           else if(input$year==2017){"http://hdl.handle.net/10793/1335"}
                                           else if(input$year==2018){"https://oar.marine.ie/handle/10793/1374"}),"Link to UWTV for FU17",target="_blank"),p(),
                                         "No Link Available",p(),
                                         a(href=paste0(
                                           if(input$year==2016){"https://oar.marine.ie/handle/10793/59/recent-submissions"}
                                           else if(input$year==2017){"http://hdl.handle.net/10793/1332"}
                                           else if(input$year==2018){"https://oar.marine.ie/handle/10793/1375"}),"Link to UWTV for FU19",target="_blank"),p(),
                                         a(href=paste0(
                                           if(input$year==2016){"https://oar.marine.ie/handle/10793/59/recent-submissions"}
                                           else if(input$year==2017){"http://hdl.handle.net/10793/1330"}
                                           else if(input$year==2018){"https://oar.marine.ie/handle/10793/1377"}),"Link to UWTV for FU2021",target="_blank"),p(),
                                         a(href=paste0(
                                           if(input$year==2016){"https://oar.marine.ie/handle/10793/59/recent-submissions"}
                                           else if(input$year==2017){"http://oar.marine.ie/handle/10793/1331"}
                                           else if(input$year==2018){"https://oar.marine.ie/handle/10793/1376"}),"Link to UWTV for FU22",target="_blank")
                                  )),
                         HTML("<br><br>"))}}
    do.call(tabsetPanel, panels)
    #tabsetPanel(panels)
  })
  
  output$EcosystemOverview <-renderUI({
    if(input$year=="2015"){
      tagList(h3("Ecosystem Overview and Mixed Fisheries was introduced in 2016"))
    }else if(input$year>2016){
      panelsEO= list(
                  tabPanel("Ecosystem Overview",
                           h2("Ecosystem Overview"),
                           h3("Summary"),
                           fluidRow(column(width = 9, htmlOutput("SummaryText")),
                                    column(width = 3, imageOutput("SurfaceArea",height = "50%"),
                                           "Surface swept area ration of otter trawls in 2015 (ICES, 2016)")),
                           h3("FEAS Ecosystem Considerations"),
                           htmlOutput("Considerations"), HTML("<br>"),
                           imageOutput("guild2017"),HTML("<br>"),
                           "Time-series of average of relative fishing mortality (F to Fmsy ratio) and biomass 
                           (SSB to Bmsy trigger ratio) by fish guild. Mean F and mean SSB is by total number of 
                           stocks with reference points."))
      panelsD3MF= list(
        tabPanel("D3 Assessment", 
                           htmlOutput("D3summ"),
                           htmlOutput("D3text"),p(),
                          if(input$year==2017){
                           list("Table 2: MSFD GES assessment for D3 in 2017",
                           fluidRow(column(width = 6, imageOutput("D3table",height = "50%")),
                                    column(width = 6, imageOutput("D3table2",height = "50%"))),
                           HTML("<br>"),
                           h3("Preliminary Results"),
                           "Table 3: MSFD Preliminary GES Results for D3 in 2017",
                           imageOutput("D3results1", height="50%"),
                           imageOutput("D3results2", height="50%"),
                           imageOutput("D3results3", height="50%"))
                            }else if(input$year==2018){
                             list(
                               "Table 1a. MSFD GES assessment for D3 in 2018, demersal stocks.",
                               imageOutput("D3table1a",height = "50%"),
                               "Table 1b. MSFD GES assessment for D3 in 2018, pelagic stocks.",
                               imageOutput("D3table1b",height = "50%"),
                               "Table 1c. MSFD GES assessment for D3 in 2018, Nephrops stocks.",
                               imageOutput("D3table1c",height = "50%"),
                               "Table 1d. MSFD GES assessment for D3 in 2018, elasmobranch stocks.",
                               imageOutput("D3table1d",height = "50%"),
                               "Table 1e. MSFD GES assessment for D3 in 2018, deepwater stocks.",
                               imageOutput("D3table1e",height = "50%"),
                               "Table 2. MSFD Preliminary GES Results for D3 in 2018.",
                               imageOutput("D3table22018",height = "50%")
                             )
                           },
                           HTML("<br>"),
                           h3("Time Series of Pressure and State Indicators"),
                           htmlOutput("PressureState"), p(), HTML("<br>"),
                           fluidRow(column(width = 4, imageOutput("RelativeF", height="50%")),
                                    column(width = 4, imageOutput("RelativeSSB",height = "50%"))),
                           "Figure 4: Relative F and Relative SSB of all assessed stocks. For stocks 
                           included in each category, see Table 2",
                           HTML("<br><br>"),
                           fluidRow(column(width = 7, imageOutput("Kobe", height="50%"), 
                                           "Figure 5: F/FMSY and SSB/MSYbtrigger for all assessed stocks. 
                                           (Grey lines represent MSY reference values)."),
                                    column(width = 5, imageOutput("Bar",height = "50%"),
                                           "Figure 6: Catch (triangles) and landings (circles) of all 
                                           assessed stocks at GES (green), non GES (red) or unknown (grey).")),
                           HTML("<br><br>")),
                  tabPanel("Mixed Fisheries", 
                           htmlOutput("MixedFish_1"),
                           htmlOutput("MixedFish_2"),
                           imageOutput("MixedFishimage", height="50%"), HTML("<br><br>")))
      
      panelstest=if(input$year==2018){panelsD3MF}else{c(panelsEO, panelsD3MF)}
      
      do.call(tabsetPanel, panelstest)
  }else if(input$year=="2016"){
    tagList(h2("Ecosystem Overview & Mixed Fisheries"),
    h3("Summary"),
    fluidRow(column(width = 9, htmlOutput("SummaryText")),
             column(width = 3, imageOutput("SurfaceArea",height = "50%"),
                    "Surface swept area ration of otter trawls in 2015 (ICES, 2016)")),
    h3("FEAS Ecosystem Considerations"),
    htmlOutput("Considerations"),
    fluidRow(column(width = 4, imageOutput("RelativeF", height="50%"),
                    "Relative F (the ratio of F to F_MSY) of all fully assessed stocks with Irish Quota. 
                            (Excludes Haddock in 6a as the ICES advice was not finalised at the time of preparation.)"),
             column(width = 4, imageOutput("RelativeSSB",height = "50%"),
                    "Relative SSB (the ratio of SSB to BTrigger) of all fully assessed stocks with Irish Quota.
                            (Excludes Haddock in 6a)")),
    h3("Pressure and State Indicators 2016:"), 
    htmlOutput("PressureState"),
    tabsetPanel(id="test", type = "pills",
                tabPanel("Irish Sea",
                         fluidRow(column(width = 6, imageOutput("IrishSea",height = "50%")),
                                  column(width = 6, imageOutput("IrishSeaFSSB",height = "50%")))),
                tabPanel("Celtic Sea",
                         fluidRow(column(width = 6, imageOutput("CelticSea",height = "50%")),
                                  column(width = 6, imageOutput("CelticSeaFSSB",height = "50%")))),
                tabPanel("West of Scotland and Rockall",
                         fluidRow(column(width = 6, imageOutput("WestScot",height = "50%")),
                                  column(width = 6, imageOutput("WestScotFSSB",height = "50%")))),
                tabPanel("Widely Distributed Species",
                         fluidRow(column(width = 6, imageOutput("DistSpecies",height = "50%")),
                                  column(width = 6, imageOutput("DistSpeciesFSSB",height = "50%"))))),
    h5("Link to the Celtic Seas Ecoregion - Ecosystem Overview pdf:"),
    a(href=paste0("http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2016/2016/Celtic_Sea_Ecoregion-Ecosystem_overview.pdf"),
      "2016 Ecosystem Overview",target="_blank"),
    h5("Link to the ICES Advice pdf:"), 
    a(href=paste0("http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2016/2016/mix-celt.pdf"),
      "2016 ICES Advice for Mixed Fisheries",target="_blank"))}
  })
}

shinyApp(ui, server)
