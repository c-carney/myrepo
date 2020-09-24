
#load the relevant libraries
library(ggplot2)
library(shiny)
library(plotly)
library(shinydashboard)
library(tidyverse)
library(zoo)
library(lubridate)
library(data.table)
library(DT)
library(tidyverse)
library(curl)
library(RCurl)
library(readODS)

setwd("~/desktop/r/myrepo")

all_sid<-read.csv("all_sid.csv")

choice.sector <- as.list(all_sid$sector)
choice.sector<-unique(choice.sector)
choice.sector

#names(choice.sector) <- all_sid$sector
choice.sector


sector<-unique(all_sid$sector)
sector
choice.sector <- as.list(sector)
names(choice.sector) <- sector

choice.sector_v2 <- as.list(sector)
names(choice.sector_v2) <- sector


extendingagency<-unique(all_sid$ExtendingAgency)
extendingagency
choice.agency <- as.list(extendingagency)
names(choice.agency) <- extendingagency
all_sid$sector<-as.factor(all_sid$sector)

extendingagency<-unique(all_sid$dept)
extendingagency
choice.agency <- as.list(extendingagency)

extendingagency_s<-unique(all_sid$dept)
extendingagency_s
choice.agency_s <- as.list(extendingagency_s)

subsector<-unique(all_sid$sub_sector)
subsector<-c("Unspecified","Basic","Secondary","Post-Secondary")
choice.subsector <- as.list(subsector)   

choice.subsector_v2 <- as.list(subsector)              

### make list for detailed categories                    



crslist<-c("Facilities & training","Teacher training","Edu research","Primary edu", "Early childhood edu",
           "School feeding",	"Secondary edu",	"Vocational","Higher edu","Advanced tech & managerial")
choice.crs <- as.list(crslist)     
choice.crs


### list of recipenet countries


summary(all_sid$country)


countrylist<-unique(all_sid$country)
countrylist
choice.country <- as.list(countrylist)
names(choice.country) <- extendingagency



#setwd("C:/Users/c-carney/OneDrive - DFID/Documents/R/edu_dashboard")


#################################################################################
#                                 get data
#################################################################################


#Our special graph theme
theme_bespoke <- theme(panel.background = 
                         element_blank(),panel.border=element_blank(),panel.grid.major = 
                         element_blank(),panel.grid.minor = element_blank(),strip.background=
                         element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=
                         element_text(colour="black"),axis.ticks=
                         element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"),
                       legend.key = element_blank(), axis.line.x = element_line(),
                       plot.title = element_text(size = 11), 
                       plot.caption=element_text(size=8, hjust=0, margin=margin(t=5)))

# Dashboard header for the title of the dashboard---- 
header <- dashboardHeader(title="Education Indicators", titleWidth = 350)

# Sidebar content of the dashboard----
sidebar <- dashboardSidebar(width=350, sidebarMenu(
  menuItem("All ODA Spend", tabName="ODA", icon=icon("list")), 
  menuItem("HMG Dept Spend", tabName="HMG", icon=icon("list"))
) # closes sidebarMenu
) #closes dashobard side bar

# Define UI for DIT Monitoring App ----
ui <- dashboardPage(header,sidebar, dashboardBody(
  #  tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #           ".shiny-output-error:before { visibility: hidden; }"),
  
  tabItems(   
    
    # Tab One 
    tabItem(tabName="ODA", h1("All ODA Spend"),
            
            # row with slider to select years          
            fluidRow( 
              sliderInput(inputId = "yr1", label="Years to evaluate",   value=c(2009,2015), min=2000, max=2020,sep="") #close sliderInput
            ), # close fluid row
            
            br(), # break 
            
            # row with box to select sector and grah of sector                                
            fluidRow( 
              # box to select sector
              box(width=3, height = "150px",solidHeader = T,title="Sectors",status="primary",
                  selectInput("sec", "Select Sectors:",choices=choice.sector, multiple=TRUE,selected="Education") ),
              # box for graph
              splitLayout(box(plotlyOutput("plot1a")) ) # close slilt layout
            ), # close previous fluid row
            
            br(), # break
            
            # row with box to select sub sector and grah of sector                                 
            fluidRow(  
              # select sub sector
              box(width=3, height = "150px",solidHeader = T,title="Sub-sector for education",status="primary",
                  selectInput("sub_sec", "Select sub-sector:",choices=choice.subsector, multiple=TRUE,selected="Basic")),
              # box to add graph              
             splitLayout(box(plotlyOutput("plot1b"))) # close split layout
            ), # close previous fuid row
            
            br(), # break 
            
            # row with box to select crs and grah of sector
            fluidRow( 
              # box to select crs
              box(width=3, height = "150px",solidHeader = T,title="CRS for education",status="primary",
                  selectInput("crs", "Select CRS purpose code:", choices=choice.crs, multiple=TRUE,selected="Primary edu")),
              # box for graph   
              splitLayout(box(plotlyOutput("plot1c") ) ) # close slilt layout
            ) # close previous fuid row
            
    ), # closes tab item
    
    # Tab Two
    tabItem(tabName="HMG", h1("ODA Spend by HMG"),
            
            # row with slider to select years     
            fluidRow( 
              sliderInput(inputId = "yr2",label="Years to evaluate",   value=c(2009,2015), min=2000, max=2020, sep="") #close sliderInput
            ), # close fluid row
            
            br(), # break 
            
            # row with boxes for sector, extending agencg and graph
            fluidRow( 
              # box to select sector and grah of sector
              box(width=3, height = "150px",solidHeader = T,title="Sectors",status="primary",
                  selectInput("sec_v2", "Select Sectors:", choices=choice.sector_v2,  multiple=FALSE, selected="Education") ), 
              # box to select Extending Agency 
              box(width=3, height = "150px",solidHeader = T,title="Extending Agency",status="primary",
                  selectInput("ExtAg", "Select HMG Department:",choices=choice.agency_s,  multiple=TRUE, selected="DFID") ) ,
              # box with graph
              splitLayout( box(plotlyOutput("plot2a")) ) # close slilt layout
            ), # close fluid row
            
            ####add box for agency#######################################################################fix
            # row with box to select sub sector and grah of sub sector                
            fluidRow(  
              # box for sub sector
              box(width=3, height = "150px",solidHeader = T,title="Sub-sector for education",status="primary",
                  selectInput("sub_sec_v2", "Select sub-sector:",choices=choice.subsector_v2, multiple=TRUE,selected="Basic")),
              # box with graph
              splitLayout( box(plotlyOutput("plot2b")) ) # close splitlayout
            ) # close previous fluid row
            
            
    ) # closes tab item
  ) # closes tabItems
) # closes dashbaordBody
) # closes header
#)  

# Define server logic for graphs

server <- function(input, output) {
  
  
  
  ################################################################################################################################
  #                                    education spend by DFID / other gov departments 
  ################################################################################################################################
  # graph with DFID/ HMG
  
  output$plot1a <-renderPlotly ({ 
    
    uk_spend  <- all_sid %>% 
      mutate(sector=as.factor(sector)) %>% 
      filter (sector %in% input$sec) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  
    
    graph_data <- uk_spend %>% 
      group_by(sector, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    graph_data<-as.data.frame(graph_data) 
    
    # create the graph 
    uk_sec_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = sector)) +
      theme_minimal() +
      labs(x = "Year", y = "ODA Â£M", title = "ODA by Sector over Time")
    
    plotly_build(uk_sec_spend_g)
  })
  
  
  ################################################################################################################################
  #                                    education spend by DFID / other gov departments 
  ################################################################################################################################
  # graph with all sectors - year
  
  output$plot1b <-renderPlotly ({ 
    
    graph_data  <- all_sid %>% 
      filter (sector %in% "Education") %>% 
      #  filter (dept %in% input$ExtAg_s) %>% 
      filter (sub_sector %in% input$sub_sec) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  %>% 
      group_by(sub_sector, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    graph_data<-as.data.frame(graph_data) # %>% 
    
    # create the graph 
    edu_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = sub_sector)) +
      theme_minimal() +
      labs(x = "Year", y = "ODA Â£M", title = "Education ODA by sub-sector")
    
    plotly_build(edu_spend_g)
  })
  
  
  ################################################################################################################################
  #                                    education spend by DFID / other gov departments 
  ################################################################################################################################
  # graph with all sectors - year
  
  output$plot1c <-renderPlotly ({ 
    
    graph_data  <- all_sid %>% 
      filter (sector %in% "Education") %>% 
      filter (crs %in% input$crs) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  %>% 
      group_by(crs, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    graph_data<-as.data.frame(graph_data) # %>% 
    
    # create the graph 
    edu_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = crs)) +
      theme_minimal()+
      theme(legend.position="bottom") +
      labs(x = "Year", y = "ODA Â£M", title = "Education ODA by CRS")
    
    plotly_build(edu_spend_g)
  })
  
  ###############################################################################################################
  output$plot2a <-renderPlotly ({ 
    #
    uk_spend  <- all_sid %>% 
      filter (sector %in% input$sec_v2) %>% 
      filter (dept %in% input$ExtAg) %>% 
      subset(Year>= input$yr2[1] & Year<=input$yr2[2])  
    
    graph_data <- uk_spend %>% 
      group_by(dept, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    
    graph_data<-as.data.frame(graph_data) 
    
    # create the graph 
    uk_agency_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = dept)) +
      theme_minimal() +
      labs(x = "Year", y = "ODA Â£M", title = "Education ODA by HMG Department over Time")
    
    plotly_build(uk_agency_spend_g)
  })
  output$plot2b <-renderPlotly ({ 
    
    graph_data  <- all_sid %>% 
      #filter (sector %in% "Education") %>% 
      filter (dept %in% input$ExtAg) %>% 
      filter (sub_sector %in% input$sub_sec_v2) %>% 
      subset(Year>= input$yr2[1] & Year<=input$yr2[2])  %>% 
      group_by(sub_sector, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    graph_data<-as.data.frame(graph_data) # %>% 
    
    # create the graph 
    edu_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = sub_sector)) +
      theme_minimal()+
      theme(legend.position="bottom") +
      labs(x = "Year", y = "ODA Â£M", title = "Education ODA by CRS")
    
    plotly_build(edu_spend_g)
  })
  
}


shinyApp(ui, server)

