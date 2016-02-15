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




# effect of outliers on regression equation 
sampleData <- data.frame(x=c(1:20), y = c(4.2,6.6,12,12,15,17.5,21,23,26,30,31,33.7,36.6,40,46,44.5,47.5,55,52.8,55.3))
fit <- lm(y~x, data = sampleData)
ggplot(sampleData,aes(x=x,y=y))+geom_point()+geom_smooth(method='lm',se=FALSE)


# adding outlier point to the above data and computing regression equation 
sampleDataNew <- rbind(sampleData, data.frame(x=21,y=120))
fitNew <- lm(y~x, data = sampleDataNew)
ggplot(sampleDataNew,aes(x=x,y=y))+geom_point()+geom_smooth(method='lm',se=FALSE)
