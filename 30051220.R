library(shiny)
library(shinythemes)
library(rsconnect)
library(ggplot2)
library(DT)
library(bslib)
thematic::thematic_shiny(font = "auto")


#import data set
tempLondon <- read.csv("London.csv", header = TRUE, sep = ',')
tempLondon$Date <- as.Date(tempLondon$Date)
tempLondon$Area <- as.factor(tempLondon$Area)



ui <- fluidPage(
  #Theme
  theme = shinytheme("simplex"),
  
  
  navbarPage(
    #--main Title--
    "Population in different parts of London",

    ),
    #-------Tab 01 Start-------
    tabPanel(
      #---Tab Name---
      "Scatterplot",
      
      #```Tab 01 - side bar```
      sidebarPanel(
        #first imput for the plot
        selectInput(
          inputId = "num",
          label = "Numbers",
          choices = c("salary", "life_satisfaction", "mean_salary", "Population"),
          selected = "Population"
        ),#second imput for the plot
        selectInput(
          inputId = "area",
          label = "Area",
          multiple = TRUE,
          choices = c(tempLondon$Area)
        ),
        ## imput for line x in the plot
        dateRangeInput(
          inputId = "date",
          label = "Select date range",
          start = min(tempLondon$Date),
          end = max(tempLondon$Date)
        )
        
      ),
      
      #```Tab Main - main bar```
      mainPanel(
        #``Tab Main - title``
        h1("Data on a Scatterplot"),
        
        plotOutput(outputId = "tabOnePlot"),
        
        em("Positive & Negative percentages indicate an increase & dicrease from the baseline period repectively"),  # em can used to showcase paragraphs
        
        # Making a data Table
        DT :: dataTableOutput(outputId = "table")
       
      )))
      
# Define server logic required to draw a histogram
server <- function(input, output) {
 
  # Filtering Data for Tab Main
  filteredData <- reactive({
    subset(
      tempLondon,
        Area %in% input$area &
        Date >= input$date[1] & Date <= input$date[2]
    )
  })
  
  # Plot for Tab Main
  output$tabOnePlot <- renderPlot({
    ggplot(
      filteredData(),
      
      aes_string(x = "Date", y = input$num, color = "Area")) +
      geom_point(alpha = 0.5) +
      ylab("% Change from baseline")
  })
  
  #Table for Tab Main
  output$table <- DT:: renderDataTable({filteredData()
    
})
  
  #Table for Tab 2
  #output$table2 <- DT:: renderDataTable({filteredData2()})
  
  #Table for Tab 3
  #output$table3 <- DT:: renderDataTable({filteredData3()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)