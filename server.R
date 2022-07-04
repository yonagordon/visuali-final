# Load libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly) 

# Read in data
adult <- read_csv("adult.csv")
# Convert column names to lowercase for convenience 
names(adult) <- tolower(names(adult))

# Define server logic
shinyServer(function(input, output) {
  
  df_country <- reactive({
    adult %>% filter(native_country == input$categorical_variable)
  })
  
  # TASK 5: Create logic to plot histogram or boxplot
  output$p1 <- renderPlot({
    if (input$graph_type == "histogram") {
      # Histogram
      ggplot(df_country(), aes(x = input$continuous_variable)) +
        geom_histogram()  # histogram geom
        labs(x=paste("input", input$continuous_variable), y="number of hours") +  # labels
        facet_wrap(.~adult$prediction)   # facet by prediction
    }
    else {
      # Boxplot
     ggplot(df_country(), aes(y = input$continuous_variable)) +
        geom_boxplot() +  # boxplot geom
        coord_flip() +  # flip coordinates
        labs(
          y = "number of people",        # y axis label
          title = "Trend of continuous variable"      # plot title
        ) +  # labels
        facet_wrap(.~adult$prediction)   # facet by prediction
    }
    
  })
  
  # TASK 6: Create logic to plot faceted bar chart or stacked bar chart
  output$p2 <- renderPlot({
    # Bar chart
    p <- ggplot(df_country(), aes(x = input$categorical_variable)) +
      geom_bar()+
      labs(x="categorical",
           y="number of people",
           title="trend of work/class") +  # labels
      theme()  # modify theme to change text angle and legend position
    
    if (input$is_stacked) {
      p +  geom_bar(position = "stack")  # add bar geom and use prediction as fill
    }
    else{
      p + 
        #... + # add bar geom and use input$categorical_variables as fill 
        facet_wrap(.~adult$prediction)  # facet by prediction
    }
  })
})
