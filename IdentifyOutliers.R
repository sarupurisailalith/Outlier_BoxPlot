#install.packages("shiny")
library(shiny)
#install.packages("miniUI")
library(miniUI)
#install.packages("ggplot2")
library(ggplot2)

ggbrush <- function(data, xvar, yvar) {
  
  ui <- miniPage(
    gadgetTitleBar("Drag to select points"),
    miniContentPanel(
      # The brush="brush" argument means we can listen for
      # brush events on the plot using input$brush.
      plotOutput("plot", height = "100%", brush = "brush")
    )
  )
  
  server <- function(input, output, session) {
    
    # Render the plot
    output$plot <- renderPlot({
      # Plot the data with x/y vars indicated by the caller.
      ggplot(data, aes_string(xvar, yvar)) + geom_boxplot()
    })
    
    # Handle the Done button being pressed.
    observeEvent(input$done, {
      # Return the brushed points. See ?shiny::brushedPoints.
      print(brushedPoints(data, input$brush))
    })
  }
  
  runGadget(ui, server)
}





# grouped box plot for iris dataset
ggplot(data=iris,aes(y = Sepal.Width, x = Species, fill=Species))+geom_boxplot(outlier.colour = "red", outlier.size = 3)

# grouped box plot with jitters
ggplot(data=iris,aes(y = Sepal.Width, x = Species, fill=Species))+geom_boxplot(outlier.colour = "red", outlier.size = 3)+geom_jitter(width=0.2)

# ggbrush sample execution 
ggbrush(iris,"Species","Sepal.Width")

