
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
library(plotly)
library(scales)

setwd("~/desktop/r/myrepo")

all_sid<-read.csv("all_sid.csv")

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
header <- dashboardHeader(title="UK ODA Spend", titleWidth = 350)

# Sidebar content of the dashboard----
sidebar <- dashboardSidebar(width=350, sidebarMenu(
  menuItem("Bilateral ODA sector trends", tabName="ODA", icon=icon("list")), 
  menuItem("HMG Dept Spend", tabName="HMG", icon=icon("list")),
  menuItem("Year overview", tabName="YEAR", icon=icon("list"))
  
) # closes sidebarMenu
) #closes dashobard side bar

# Define UI for DIT Monitoring App ----
ui <- dashboardPage(header,sidebar, dashboardBody(
  #  tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #           ".shiny-output-error:before { visibility: hidden; }"),
  
  tabItems(   
    
    # Tab One 
    tabItem(tabName="ODA", h1("Bilateral ODA Trends"),
            
            # row with slider to select years          
            fluidRow( 
              sliderInput(inputId = "yr1", label="Years to evaluate",   value=c(2015,2019), min=2000, max=2020,sep="") #close sliderInput
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
            # row with box for sector as % of oda
            fluidRow( 
              # box to select crs
              box(width=3, height = "150px",solidHeader = T,title="CRS for education",status="primary",
                  selectInput("sec_v2", "Select Sectors:",choices=choice.sector_v2, multiple=TRUE,selected="Education") ),
              # box for graph   
              splitLayout(box(plotlyOutput("plot1b") ) ), # close slilt layout
              
              splitLayout(box(plotlyOutput("plot1c"))) # close split layout
              
              
              
            ), # close previous fuid row
            
            br(), # break 
            # row with box to select sub sector and grah of sector                                 
            fluidRow(  
              # select sub sector
              box(width=3, height = "150px",solidHeader = T,title="Sub-sector for education",status="primary",
                  selectInput("sub_sec", "Select sub-sector:",choices=choice.subsector, multiple=TRUE,selected="Basic")),
              # box to add graph              
              splitLayout(box(plotlyOutput("plot1d"))), # close split layout
              
            ), # close previous fuid row
            
            br(), # break 
            # row with box to select crs and grah of sector
            fluidRow( 
              # box to select crs
              box(width=3, height = "150px",solidHeader = T,title="CRS for education",status="primary",
                  selectInput("crs", "Select CRS purpose code:", choices=choice.crs, multiple=TRUE,selected="Primary edu")),
              # box for graph   
              splitLayout(box(plotlyOutput("plot1e") ) ) # close slilt layout
            ) # close previous fuid row
            
    ), # closes tab item
    
    # Tab Two
    tabItem(tabName="HMG", h1("ODA Spend by HMG"),
            
            # row with slider to select years     
            fluidRow( 
              sliderInput(inputId = "yr2",label="Years to evaluate",   value=c(2015,2019), min=2000, max=2020, sep="") #close sliderInput
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
            
            
    ), # closes tab item
    
    
    # Tab Three
    tabItem(tabName="YEAR", h1("Year overview"),
            
            # row with slider to select years          
            fluidRow(
              # row with box to select year                
              # box for sub sector
              box(width=3, height = "150px",solidHeader = T,title="Year to evaluate",status="primary",
                  selectInput("yr3", "Select year:",choices=choice.year, multiple=FALSE,selected="2018"))
            ), # close fluid row
            
            br(), # break 
            
            tableOutput("table1"), 
            fluidRow(
              #box(width=4, height = "250px",solidHeader = T,title="Total ODA",status="primary",
               # tableOutput("table1")
             # ),
                box(width=4, height = "250px",solidHeader = T,title="Total Bilateral ODA",status="primary",
                    tableOutput("table2")
                ) #,
            ), # close fluid row
                
                fluidRow(
                  box(width=4, height = "250px",solidHeader = T,title="Total Multilateral ODA",status="primary",
                      tableOutput("table3")
                  ),
                
              box(width=4, height = "250px",solidHeader = T,title="Top Recipient Sector",status="primary",
                  tableOutput("table4")
              ),
              box(width=4, height = "250px",solidHeader = T,title="Top Recipient Country",status="primary",
                  tableOutput("table5")
                  )
           
              ), # close fluid row

          
            # row with box to select sector and grah of sector                                
            fluidRow( 
              # box for graph
              splitLayout(box(plotlyOutput("plot3a")) ) # close slilt layout
            ), # close previous fluid row
            
            br(), # break
            
            # row with box to select sub sector and grah of sector                                 
            fluidRow(  
              # box to select sector and grah of sector
             # box(width=3, height = "150px",solidHeader = T,title="Sectors",status="primary",
             #     selectInput("sec_v3", "Select Sectors:", choices=choice.sector_v3,  multiple=FALSE, selected="Education") ), 
              # box to select Exte
              
              splitLayout(box(plotlyOutput("plot3b"))) # close split layout
            ), # close previous fuid row
            
            br(), # break 
            
            # row with box to select crs and grah of sector
            fluidRow( 
              # box for graph   
              splitLayout(box(plotlyOutput("plot3c") ) ) # close slilt layout
            ) # close previous fuid row
            
    ) # closes tab item
    
  ) # closes tabItems
) # closes dashbaordBody
) # closes header
#)  

# Define server logic for graphs

server <- function(input, output) {
  
  
  
  ################################################################################################################################
  #                                    oda spend for sector over time
  ################################################################################################################################
  # graph with DFID/ HMG
  
  output$plot1a <-renderPlotly ({ 

    uk_spend  <- all_sid %>% # get data
    # mutate(Sector=sector) %>% # make capslock for appearance
      filter (sector %in% input$sec) %>% # filter on the basis of list in input$sec
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  
    
    graph_data <- uk_spend %>% 
      group_by(sector, Year) %>% 
      summarise(oda_year=((sum(oda)/1000)))  
    
    graph_data$oda_year<-round(graph_data$oda_year, 3) 
    fct_explicit_na(graph_data$sector, na_level = "Missing")   
    
    # create the graph 
    uk_sec_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = sector)) +
      theme_minimal() +
      labs(x = "Year", y = "ODA £M",  title = "ODA £M by Sector over Time")
    
    plotly_build(uk_sec_spend_g)
  })
  
  
  ################################################################################################################################
  #                                    sector as % of ODA spend  
  ################################################################################################################################
  
  output$plot1b <-renderPlotly ({ 
    
   # graph data
    uk_spend  <- all_sid %>%  mutate(Sector=as.factor(sector)) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  %>% 
      group_by(Year) %>% 
      summarise(total_oda=((sum(oda)/1000)))  
    
    graph_data  <- all_sid %>%  mutate(Sector=as.factor(sector)) %>% 
      filter (Sector %in% input$sec_v2) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  %>% 
      group_by(Year, Sector) %>% 
      summarise(oda_year=((sum(oda)/1000)))
    
    graph_data<-full_join(uk_spend, graph_data) %>% 
      mutate(p_oda=((oda_year/total_oda)*100))
    
    graph_data$p_oda<-round(graph_data$p_oda, 0)
    
    
    # create the graph 
    sector_graph<- ggplot(graph_data, aes(x=Year, y=p_oda))  + 
      geom_line(aes(color = Sector)) +
      theme_minimal() +
      labs(x = "Year", y = "% of Total Bilateral ODA", title = "Sector's % of Bilateral ODA") 
    sector_graph
    plotly_build(sector_graph) 
  })
  
  
  ################################################################################################################################
  #                                    sub sector education spend
  ################################################################################################################################
 
  
   # graph with all sectors - year
  
  output$plot1d <-renderPlotly ({ 
    
    graph_data  <- all_sid %>% 
      filter (sector %in% "Education") %>% 
      #  filter (dept %in% input$ExtAg_s) %>% 
      filter (sub_sector %in% input$sub_sec) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  %>% 
      group_by(sub_sector, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    
    graph_data$oda_year<-round(graph_data$oda_year, 3) 
    
    # create the graph 
    edu_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = sub_sector)) +
      theme_minimal() +
      labs(x = "Year", y = "ODA £M", title = "Education ODA by sub-sector")
    
    plotly_build(edu_spend_g)
  })
  
output$plot1d <-renderPlotly ({ #
  total_oda  <- all_sid %>%  mutate(Sector=as.factor(sector)) %>% 
    subset(Year>= input$yr1[1] & Year<=input$yr1[2]) %>% 
    filter (sector %in% "Education") %>% 
    group_by(Sector, Year) %>% 
    summarise(total_oda=((sum(oda))/1000)) 
  
  graph_data  <- all_sid %>%  mutate(Sector=as.factor(sector)) %>% 
   subset(Year>= input$yr1[1] & Year<=input$yr1[2]) %>% 
    filter (sector %in% "Education") %>% 
    group_by(sub_sector, Year) %>% 
    summarise(oda_year=((sum(oda))/1000)) 
  
  graph_data<-full_join(total_oda,graph_data) %>% 
    mutate(p_oda=((oda_year/total_oda)*100))
  
  graph_data$p_oda<-round(graph_data$p_oda, 0)
  
  # create the graph 
  sector_graph<- ggplot(graph_data, aes(x=Year, y=p_oda, fill=sub_sector, label =p_oda))  + 
    geom_area() +
    # scale_fill_brewer(palette="Dark2")+
    theme_minimal() +
    geom_text (check_overlap=TRUE, size=3,  position=position_stack(vjust = .5)) +
    labs(x = "Year", y = "% of Sector ODA", title = "Sub-sector's % of Sector Bilateral ODA") 
  sector_graph
  })
  
  ################################################################################################################################
  #                                   crs sector education spend by DFID / other gov departments 
  ################################################################################################################################
  # graph with all sectors - year
  
  output$plot1e <-renderPlotly ({ 
    
    graph_data  <- all_sid %>% 
      filter (sector %in% "Education") %>% 
      filter (crs %in% input$crs) %>% 
      subset(Year>= input$yr1[1] & Year<=input$yr1[2])  %>% 
      group_by(crs, Year) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    
    graph_data$oda_year<-round(graph_data$oda_year, 3) 
    
    # create the graph 
    edu_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = crs)) +
      theme_minimal()+
      theme(legend.position="bottom") +
      labs(x = "Year", y ="ODA £M", title = "Education ODA by CRS")
    
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
    
    graph_data$oda_year<-round(graph_data$oda_year, 3) 
    
    # create the graph 
    uk_agency_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = dept)) +
      theme_minimal() +
      labs(x = "Year", y = "ODA ÃÂÃÂ£M", title = "Education ODA by HMG Department over Time")
    
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
    
    graph_data$oda_year<-round(graph_data$oda_year, 3) 
    
    # create the graph 
    edu_spend_g<- ggplot(graph_data, aes(Year, oda_year)) + 
      geom_line(aes(color = sub_sector)) +
      theme_minimal()+
      theme(legend.position="bottom") +
      labs(x = "Year", y = "ODA ÃÂÃÂ£M", title = "Education ODA by CRS")
    
    plotly_build(edu_spend_g)
  })
  
  
  #####################################################################################################
 
  
  
  output$plot3a <-renderPlotly ({ 

  
  sector  <- all_sid %>% 
    filter (year %in% input$yr3)
  
  graph_data <- sector %>% 
    group_by(year, sector) %>% 
    summarise(oda_year=((sum(oda))/1000)) 
  
  total_oda<- graph_data %>% summarise(total_oda=(sum(oda_year)))
  
  graph_data<-full_join(total_oda, graph_data) %>% 
    mutate(p_oda=((oda_year/total_oda)*100))
  
  graph_data$p_oda<-round(graph_data$p_oda, 0)
  
  
  # create the graph 
  sector_graph<- ggplot(graph_data, aes(x=sector, y=p_oda, fill=sector))  + 
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(legend.position = "none") +
    coord_flip() +
    labs(x = "Sector", y = "% of Total Bilateral ODA", title = "Bilateral ODA by Sector") 
  sector_graph
  plotly_build(sector_graph)
})

  
  
  ################################################################################################################################
  #                                    education spend by DFID / other gov departments 
  ################################################################################################################################
  # graph with all sectors - year
  
  output$plot3b <-renderPlotly ({ 
    
    sector  <- all_sid %>%      
      filter (year %in% input$yr3) %>% 
         filter(sector %in% input$sec_V3)
    
    graph_data <- sector %>% 
      group_by(year, crs) %>% 
      summarise(oda_year=((sum(oda))/1000)) 
    
    total_oda<- graph_data %>% summarise(total_oda=(sum(oda_year)))
    
    graph_data<-full_join(total_oda, graph_data) %>% 
      mutate(p_oda=((oda_year/total_oda)*100))
    
    graph_data$p_oda<-round(graph_data$p_oda, 0)
    
    # create the graph 
    sub_sector_graph<- ggplot(graph_data, aes(x=crs, y=p_oda, fill=crs))  + 
      geom_bar(stat="identity") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip() +
      labs(x = "Sector", y = "% of Total Bilateral Education ODA", title = "Education Bilateral ODA by Sub-Sector")
    sub_sector_graph
    plotly_build(sub_sector_graph)
  })
  
  ################################################################################################################################
  #                                    education spend by DFID / other gov departments 
  ################################################################################################################################
  # graph with all sectors - year
  
  output$plot3c <-renderPlotly ({ 
    
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
      labs(x = "Year", y = "ODA ÃÂÃÂ£M", title = "Education ODA by CRS")
    
    plotly_build(edu_spend_g)
  })
  
  ###############################################################################################################
  
  
}


shinyApp(ui, server)

