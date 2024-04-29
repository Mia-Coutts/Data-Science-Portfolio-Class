#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(htmltools)
library(readr)
library(leaflet)
library(shinythemes)
library(rsconnect)


HighNeeds <- read_csv("HighNeeds.csv")
HighNeeds <- subset (HighNeeds, select = -c (...1))
HighNeeds$'Total Average SAT' <-  HighNeeds$`Average SAT`+ HighNeeds$`Average SAT`
MAall<- read_csv("MAall.csv")
MAall$'Total Average SAT' <-  MAall$`Average SAT`+ MAall$`Average SAT`
MAall <- subset (MAall, select = -c (...1,`City/town`,`Average SAT`))
names(MAall)[names(MAall) == "SAP"] <- "School Accountability Percentile (1-99)"
MAstats<- MAall[, c('Town', 'School Name', '% Attending College', '% High Needs', 'Total Average SAT', 'Median annual household income', '% AP_Score 3-5')]

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  tags$a(tags$style(HTML("a {color: white}")), href="https://belmontburs.com/", 
                  "Belmont Project", target="_blank"),
                  tabPanel("About This Project",
                           mainPanel(
                             h2("About This Project"),
                             h4("Hi! My name is Mia Coutts and I’m a Data Science major at Belmont University graduating in May 2024. This is my semester research project for my DSC 4900 Data Science Project/Portfolio course.", style="color:black"),
                             h4("This project analyzes Massachusetts Public School data from 2017, with a specific interest in predicting SAT scores. I was inspired to conduct this research because I’m from Massachusetts and was curious about what factors are significant predictors of scoring high on the SAT. The goal of this project was to use data skills we’ve been developing for the past 4 years as well as challenge ourselves and learn new skills, some of which are showcased on this website. This website contains an interactive map with some of the schools from the dataset, a search bar with all the schools, an interactive scatter plot to explore relationships between the variables in the dataset, and a table with the data I used.", style="color:black"),
                             h2("Methods Used"),
                             h4("For this project, I coded in Python and R. I used sklearn packages in Python to run multiple predictive models. These models included linear regression, multiple regression, logistic regression, Random Forest, K Nearest Neighbors, and gradient boosting. All of these models scored 85% or higher in accuracy. My best model was K Nearest Neighbors with an accuracy score of 90%. I performed error analysis on these models through cross-validation, k-fold CV, stratified CV, and bootstrapping. Furthermore, I did a feature importance analysis to identify which variables were the most significant in predicting SAT scores. I then created a website using R Shiny to display some of my results as well as some interactive visualizations.", style="color:black"),
                             h2("Abstract"),
                             h4("The SAT is a standardized test that evaluates a high schooler's reading, writing, and math skills. These scores are used across the United States for college admissions. Based on previous studies, SAT scores are strongly predictive of college performance, student retention, and income. This research project analyzes public school data with a specific interest in predicting SAT scores. Various predictive models were used on data from 292 high schools in Massachusetts in 2017. These models aimed to predict which schools performed above average on the SAT. Python was used to run models including Random Forest, K Nearest Neighbors, and Logistic Regression, all with an accuracy score above 85%. Through feature importance analysis, it was observed that the most significant predictor of a school's SAT score being above average is the percentage of high-needs students. Only 2 of the 66 schools that have a high-needs student population of 50% or more scored above average on the SAT. Additionally, schools with high percentages of students with high needs scored lower on Advanced Placement exams, graduated at lower rates, and had lower college attendance rates. Similarly, MCAS (Massachusetts Comprehensive Assessment System) scores were lower in schools that had high percentages of high-needs students. This strong negative relationship between the percentage of high-needs students and standardized test performance illustrates how the public school system in Massachusetts should make an intentional effort to better serve students with high needs through support and aid so that they can achieve academic success.", style="color:black"))# mainPanel
                           
                  ), # Navbar 1, tabPanel
                  tabPanel("Map", titlePanel("Map of Schools"), mainPanel(
                    h5("This map shows all Massachusetts schools with a high needs student population 50% or above")), leafletOutput("mymap")),
                  tabPanel("Search", titlePanel("Search By Town"),
                           textInput("x", "Enter Town:", value = ""),
                           actionButton("Submit", "Display Statistics"),
                           tableOutput("result"), h5("Note: A list of Massachusetts towns can be found in the Dataset tab", style="color:grey")),
                  tabPanel("Explore Data", titlePanel("Compare Data With Scatter Plots"), sidebarLayout(
                    sidebarPanel(
                      selectInput(inputId = "VarX",
                                  label = HTML("Select Two Attributes to Compare<br/><br/>First Attribute:"),
                                  choices = list("Total Average SAT", "% High Needs", "% Attending College", "Median annual household income", "% Economically Disadvantaged", "% Females", "% Graduated", "% AP_Score 3-5", "SAP", "% Dropped Out", "Progress and Performance Index (PPI) - All Students")),
                      selectInput(inputId = "VarY",
                                  label = "Second Attribute:",
                                  choices = list("% High Needs", "Total Average SAT",
                                                 "% Attending College", "Median annual household income", "% Economically Disadvantaged", "% Females", "% Graduated", "% AP_Score 3-5", "SAP", "% Dropped Out", "Progress and Performance Index (PPI) - All Students"))          
                    ),
                    
                    mainPanel(
                      plotOutput("scatter")
                    )
                  )
                  
                  ),
                  tabPanel("Dataset", titlePanel("Dataset from Massachusetts Public Schools in 2017"), mainPanel(tableOutput("table")))
                  
                ) # navbarPage
) # fluidPage
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>%
      #setView(lng = 103.85, lat = 1.3, zoom = 11) %>%
      addMarkers(data = HighNeeds,
                 lng = ~Longitude,
                 lat = ~Latitude,
                 label = ~Town,
                 popup = ~paste(Town, "<br>School Name:", `School Name`,
                                "<br>Average SAT Score:", `Total Average SAT`,
                                "<br>% High Needs:", `% High Needs`))
  })
  my_function<-function(x) {if (x %in% MAstats$Town == TRUE){MAstats[which(MAstats$Town == x),]} else{"This town isn’t in the dataset"}}
  result <- eventReactive(input$Submit, {
    my_function(input$x)
  })
  
  output$result <- renderTable({
    result()
  })
  
  # Get the data from the variables declared on the ui.R file
  df <- reactive({MAall[, c(input$VarX, input$VarY)]})
  
  # Create the plot
  output$scatter <- renderPlot({plot(df(), pch = 20, cex = 1, col = "blue",
                                     main = "Massachusetts Public School Dataset")})
  output$table <- renderTable({MAall
  })
  
  
} # server

# Run the application 
shinyApp(ui = ui, server = server)
