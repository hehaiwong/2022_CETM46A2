## app.R ##
library(DT)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(gganimate)
# runGitHub("<private repository name>", "<my user name>") 
# histdata <- read.csv('https://github.com/hehaiwong/2022_CETM46A2/blob/main/users_ff.csv') [1:12,]
histdata <- read.csv('users.csv')# [1:3710,]
summary(histdata)
hist_log <- read.csv('history_log.csv') [1:10,] 
summary(hist_log)
ui <- dashboardPage(
#Header Bar ##############################################################3
dashboardHeader(title = "E-Catch(Phototype)",
  dropdownMenu(type = "messages",
                 messageItem( from = "Sales Dept", message = "Sales are steady this month."
                 ),
                 messageItem( from = "New User", message = "How do I register?",
                   icon = icon("question"), time = "13:45"
                 ),
                 messageItem(  from = "Support", message = "The new server is ready.",
                   icon = icon("life-ring"), time = "2022-7-18"
                 )
    ),
   dropdownMenu(type = "notifications",
                notificationItem( text = "5 new users today", icon("users")
                ),
                notificationItem( text = "12 items delivered",icon("truck"),
                  status = "success"
                ),
                notificationItem( text = "Server load at 86%",icon = icon("exclamation-triangle"),
                  status = "warning"
                )
   ),
   dropdownMenu(type = "tasks", badgeStatus = "success",
                taskItem(value = 90, color = "green","Documentation"
                ),
                taskItem(value = 17, color = "aqua","Project X"
                ),
                taskItem(value = 75, color = "yellow", "Server deployment"
                ),
                taskItem(value = 80, color = "red", "Overall project"
                )
   )
),#Dashbaordheader
#sidebar##############################################################3
dashboardSidebar(
    sidebarMenu(
      menuItem("E-Catch Analysis", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("database")),     
      menuItem("Map", tabName = "map", icon = icon("map-pin")),
      # menuItem("E-Catch Analysis", tabName = "dashboard1", icon = icon("bullseye"),
      #        menuSubItem("Sub-item 1", tabName = "subitem1"),     
      #        menuSubItem("Sub-item 2", tabName = "subitem2")
      # ),

      menuItem("New E-Catch Request", tabName = "upload", icon = icon("plus")),
      
      menuItem("History Log", tabName = "history", icon = icon("list")),
      menuItem("Login", tabName = "login", icon = icon("lock"))
      # ,
      # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
      #                   label = "Search...")
      )#sidebarSearchForm
),#dashboardSideba
#Body#############################################################
dashboardBody(
  tabItems(
    # Dashboard content ##############################################
    tabItem(tabName = "dashboard",
            sidebarLayout(
              sidebarPanel(width=2,
                           
                   div("User Filter",
                       sliderInput("bins",
                                   "Number of rows target:",
                                   min = 1,
                                   max = nrow(histdata),
                                   value = nrow(histdata)
                                   )
                   ),
                   div("",
                       pickerInput("result","Select class of users" , choices = unique(histdata$result),
                                     # c("New Mexico", "Colorado", "California"), 
                                   options = list(`actions-box` = TRUE),multiple = T,
                                   selected = c("Fake follower","Real account","Social spambot")
                                   )
                       
                       )#div
              ), # sidebarPanel
              mainPanel(
                
                fluidRow(
                  # valueBox(nrow(histdata), "No. of accounts verified", icon = icon("credit-card"), color = "green"),
                  valueBoxOutput("total_rowBox"),
                  valueBox( "REQ100001","Request ID", icon = icon("thumbs-up")),
                  valueBox( "ABC Marketing","Agency", icon = icon("building"))
                ),#fluidRow
                fluidRow(
                  # A static infoBox
                  infoBox("Real USers (ra)", 
                          value = tags$p("67%", style = "font-size: 250%;"),
                          color = "green",
                          icon = icon("thumbs-up", lib = "glyphicon")),
                  infoBox("Fake Followers (ff)", 
                          value = tags$p("9%", style = "font-size: 250%;"),
                          color = "red",
                          icon = icon("user" , lib = "glyphicon")),
                  # Dynamic infoBoxes
                  infoBoxOutput("realBox")
                ),#fluidRow
                fluidRow(
                  # Boxes need to be put in a row (or column)
                  # Show a plot of the generated distribution
                 box(plotOutput("plot1", height = 250)
                     # ,title = "Total no. of users by result class", width = 6,
                     #  solidHeader = TRUE,status = "warning", "-4"
                     ),
                  box(plotOutput("plot2", height = 250))                
                  ),#fluidRow
                fluidRow(
                  box(plotOutput("plot3", height = 250)),
                  box(plotOutput("plot4", height = 250)),
                  box(plotOutput("plot5", height = 250)),
                  box(plotOutput("plot6", height = 250)),
                  box(plotOutput("plot7", height = 250)),
                  box(plotOutput("plot8", height = 250)),
                  box(plotOutput("plot9", height = 250))
                )#fluidRow
            )#Main panel
          )#sidebar Layout
        ),#tabItem
    
#E-Catch tab content  ##################################################
    
    tabItem(tabName = "dashboard1",
            
            fluidRow(
              box(
                title = "Title 1", width = 4, solidHeader = TRUE, status = "warning",
                "Box content", collapsible = TRUE
              )   #box

            ) #fluidrow
    ),#tabitem
    
    tabItem("subitem1", "Sub-item 1 tab content",
            fluidRow(
              # A static infoBox
              infoBox("Real USers", "90%", icon = icon("thumbs-up")),
              infoBox("Fake Usesr", 5 * 2, icon = icon("question")),
              
              # Dynamic infoBoxes
              infoBoxOutput("progressBox")
            )#fluidRow
    ),# tabItem
    tabItem("subitem2", "Sub-item 2 tab content"),
    
    #LOG IN tab content  ##################################################
    
    tabItem(tabName = "login",
            box( 
              textInput("Id", "User ID"),br(),
              textInput("pwd", "Password"),
              #button("abc"),
              title = "Login", width = 4, solidHeader = TRUE, background = "black",
              status = "warning",
              "" , collapsible = TRUE,br(),br(),                
              actionButton("button", "LOGIN", class = "btn-primary btn-lg" 
              ),
              
              actionButton("button", "CANCEL", class = "btn-primary btn-lg")
            )   #box

    ),#tabitem
    #NEW REQEUST tab content  ##################################################
    tabItem(tabName = "upload",
            box( 
              title = "+ New e-Catch Request", width = 4, solidHeader = TRUE, background = "black",
              status = "warning","" , collapsible = TRUE,
              
              selectInput("social_media", "Social media:",
                          c("Facebook" = "fb",
                            "Twitter" = "tw",
                            "Instrgram" = "ig")
              ),br(),
              
              fileInput('file1', 'Select the Input File',
                        accept=c('text/csv','text/comma-separated-values,text/plain','.csv')),
              br(),br(),                  
              actionButton("button", "UPLOAD", class = "btn-primary btn-lg" ),
              actionButton("button", "RESET", class = "btn-primary btn-lg" )
            ),   #box
            
            box(
            )   #box
    ),#tabitem
    # MAP content##############################################################
    tabItem(tabName = "map",
            fluidRow(
              
              ui <- fluidPage(
                leafletOutput("mymap", height =600),
                p()
                # ,
                # actionButton("recalc", "New points"),
                #Slider on MAP ###########################
                # box(title = "Controls", 
                #     sliderInput("slider", "Number of observations:", 1, 100, 50)
                # )#box
              )#fluidpage
            )#flluidRow
    ),#tabitem    
    
    # data content #############################################################
    tabItem(tabName = "data",
            fluidRow(
              # h2("Detail of the Analysis"),
              ##set filter code
              box( width =2, solidHeader = TRUE,collapsible = TRUE,
                   div(style = 'overflow-x: scroll', DT::dataTableOutput('view_data1'),
                       checkboxGroupInput("checkGroup1",
                                          h3("Display columns"),
                                          choices = names(histdata),
                                          #selected = names(histdata)
                                          selected = c("id","Country","result", "region")
                       )#Check Box Group Input
                   )#div
              ),#box
              
              box( width = "10",solidHeader = TRUE,collapsible = TRUE,
                   column( width = 12,h3('Detail of the Analysis'), hr(),
                           DT::dataTableOutput('x11'),
                           verbatimTextOutput('y11')
                   )#column
              )#box
            )#fluidRow
    )
    ,#tabItem

            # HISTORY LOG content #############################################################
            tabItem(tabName = "history",
                  fluidRow(
                    #h2("Detail of the Web Scap DAta"),
                    ##set filter code
                    box( width =2, solidHeader = TRUE,collapsible = TRUE,
                         div(style = 'overflow-x: scroll', DT::dataTableOutput('view_data2'),
                             checkboxGroupInput("checkGroup2",
                                                h3("Display columns"),
                                                choices = names(hist_log),
                                                selected = names(hist_log)
                             )#Check Box Group Input
                          ) ,#div
                         # column(3,checkboxGroupInput("checkGroup",
                         #                       h3("Display columns"),
                         #                       choices = names(histdata),
                         #                       selected = names(histdata)
                         #          )#Check Box Group Input
                         #  )#column
                    ),#box

                    box( width = "10",solidHeader = TRUE,collapsible = TRUE,
                      column( width = 12,h3('History of E-Catch Request'), hr(),
                        DT::dataTableOutput('x21'),
                        verbatimTextOutput('y21')
                     )#column
                    )#box
                  )#fluidRow
            )#tabItem
      ) #tabItems
    )#DashboardBody
  )#DashboardPage


###################################################################################
#server############################################################################
###################################################################################

server <- function(input, output, session) {
  
  
#### info/value BOX generation ################################# 
  output$total_rowBox <- renderValueBox({
    valueBox(
    paste0( input$bins, " rows"),   "Total no. of accounts verified in TWITTER",  icon = icon("list")
    # ,
    #   color = "green"
    )
     # infoBox(
    #   "Total no. of rows", paste0( input$bins, "rows"),  icon = icon("list"),
    #   color = "purple"
    # )
  })
  

  output$realBox <- renderInfoBox({
    infoBox(
      "Socail Spambots (ss)", 
      value = tags$p("24%", style = "font-size: 250%;"),
      # + input$count
      icon = icon("robot"),
      #icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
#### Graph generation #################################

  
  output$plot1 <- renderPlot({
    
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    group_data1 <- filter_data %>% group_by(result) %>%
      summarise(total_count_by_result= n())
    
    ggplot(data = group_data1, aes(x="",y = total_count_by_result ,fill=result))+
      geom_bar(stat="identity", width=1) +
      guides(fill = guide_legend(title = "Result class")) +
      # scale_fill_manual(values = c("blue","red","green"))+
      scale_fill_discrete(labels = group_data1$result )+
      # c("Fake follower", "Real", "Social Spambot"))+
      geom_text(aes(label = total_count_by_result),
                position = position_stack(vjust = 0.5), color="white")+
      coord_polar("y", start=0) +
      labs(title="1. Total number of users by result class ",x="",y="")
  })
  
  
  
  output$plot2 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    group_data2 <- filter_data %>% group_by(region,result) %>%
      summarise(total_no_user = n())
    
    
    ggplot(group_data2, aes(x = region, y = total_no_user, fill = result)) + 
      geom_bar(stat = "identity") + 
      geom_text(aes(label=total_no_user), vjust=1.6, color="white") +
      guides(fill = guide_legend(title = "Result class")) +
      scale_fill_discrete(labels = group_data2$result )+
      labs(x="",y="",title="2. Total number of users in each region")
  })
  
  output$plot3 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    
    group_data3 <- filter_data %>% group_by(result) %>%
      summarise(followers_count_by_result= sum(followers_count))
    
    ggplot(data = group_data3, aes(x="",y = followers_count_by_result ,fill=result))+
      geom_bar(stat="identity", width=1)+
      geom_text(aes(label = followers_count_by_result),
                position = position_stack(vjust = 0.5), color="white")+
      guides(fill = guide_legend(title = "Result class")) +
      # scale_fill_discrete(labels = c("Fake follower", "Real", "Social Spambot"))+
      scale_fill_discrete(labels = group_data3$result )+
      coord_polar("y", start=0) +
      labs(title="3. Total number of followers by result class",x="",y="")
  }) 
  
  output$plot4 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    
    group_data4 <- filter_data %>% group_by(region) %>%
      summarise(followers_count_by_region= sum(followers_count))
    
    ggplot(data = group_data4, aes(x="",y = followers_count_by_region ,fill=region))+
      geom_bar(stat="identity", width=1)+
      geom_text(aes(label = followers_count_by_region),
                position = position_stack(vjust = 0.5), color="white")+
      guides(fill = guide_legend(title = "Region")) +
      scale_fill_discrete(labels = group_data4$region )+
      coord_polar("y", start=0) +
      labs(title="4. Total number of followers in each region",x="",y="")
  })  
  
  
  output$plot5 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    
    ggplot(filter_data, aes(x = region, y = followers_count, colour = result)) + 
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE) +
      geom_jitter(color="orange", size=0.4, alpha=0.9)+
      
      guides(fill = guide_legend(title = "Result class")) +
      scale_fill_discrete(labels = c("Fake follower", "Real", "Social Spambot"))+
      labs(title="5. Total number of followers by Result class/region",x="",y="")
  }) 
  
  output$plot6 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    
    ggplot(filter_data, aes(followers_count,friends_count, colour = factor(result), shape = factor(region) )) +
      geom_point()+
      labs(title="6. followers vs friends count by Result/Region", colour = "Result Class(color)", shape = "Region(shape)"  )
  }) 
  
  output$plot7 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    
    ggplot(filter_data, aes(x = region, y = friends_count, colour = result)) + 
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE) +
      geom_jitter(color="orange", size=0.4, alpha=0.9)+   
       labs(title="7. Total number of friends by Result class/Region",x="Result Class",y="Total no. of Friends connected", fill= "Result class")
  }) 
  
  
  output$plot8 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }
    
    ggplot(filter_data, aes(followers_count,friends_count, colour = factor(region), shape = factor(result) )) +
      geom_point()+
      labs( title="8. followers vs friends count by Region/Result class",colour = "Region(color)", shape = "Result class(shape)"  )
  })
  
  output$plot9 <- renderPlot({
    if(all(unique(histdata$result) %in% input$result)){
      filter_data <- histdata[1:input$bins,]
    }else{
      filter_data <- histdata[1:input$bins,] %>% filter( result %in% input$result)
    }

    
    ggplot(filter_data, aes(x = region, y = favourites_count, colour = result)) +
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2, notch=FALSE) +
      geom_jitter(color="orange", size=0.4, alpha=0.9)+
      
      guides(fill = guide_legend(title = "Result class")) +
      scale_fill_discrete(labels = c("Fake follower", "Real", "Social Spambot"))+
      labs(title="9. Total number of favourite by Result class/Region",x="",y="")
  })
  

 #   output$plot1 <- renderPlot({
 #   filter_data <- histdata[1:input$bins,] 
 #   # %>% filter( result==input$result)
 #   # filter_data <-histdata
 #   group_data1 <- filter_data %>% group_by(result) %>%
 #     summarise(total_count_by_result= n())  
 #   
 #    ggplot(data = group_data1, aes(x=result, y =total_count_by_result))+
 #         # geom_bar(stat="count")+
 #      geom_bar(stat="identity" , width = 0.3)+
 #         labs(y="Count",x="Type of Users Real vs Fake follower vs Social Spambot",
 #              title="1. Total number of users. in each group")
 # })


 #### DT - Data table generation #######################################################
 options(DT.options = list(pageLength = 10))
 output$x11 = DT::renderDataTable(histdata[,input$checkGroup1, drop = FALSE], server = FALSE, selection = 'single', option = list(scrollX = TRUE))
 output$y11 = renderPrint(input$x11_rows_selected)
  
 points <- eventReactive(input$recalc, {
   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
 }, ignoreNULL = FALSE)
 
 
 options(DT.options = list(pageLength = 10))
 output$x21 = DT::renderDataTable(hist_log[,input$checkGroup2, drop = FALSE], server = FALSE, selection = 'single', option = list(scrollX = TRUE))
 output$y21 = renderPrint(input$x21_rows_selected)
 
 points <- eventReactive(input$recalc, {
   cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
 }, ignoreNULL = FALSE)
 
 #MAP generator #################################################################
 
 output$mymap <- renderLeaflet({
           histdata %>%
           leaflet() %>%
           # addProviderTiles(providers$Stamen.TonerLite,
           #                  options = providerTileOptions(noWrap = TRUE)
           addTiles() %>%
           addMarkers(~longitude,~latitude,popup=~id, label=~Country)
         #   addMarkers(data = points())
 })#renderLeaflet
} #server

shinyApp(ui, server)