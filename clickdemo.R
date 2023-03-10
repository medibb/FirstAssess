library(shiny)

ui <- fluidPage(
  titlePanel("Smart Pen Demo"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("line_width", "Line Width", min = 1, max = 10, value = 3),
      actionButton("clear_btn", "Clear Drawing")
    ),
    mainPanel(
      plotOutput("drawing_area", click = "click"),
      verbatimTextOutput("coord_text")
    )
  )
)

server <- function(input, output) {
  
  # Initialize reactive values
  rv <- reactiveValues(drawing = NULL)
  
  # Render the drawing area
  output$drawing_area <- renderPlot({
    plot(1:10, type = "n", xlab = "", ylab = "", main = "")
    
    # If there are points in the drawing, plot them as lines
    if (!is.null(rv$drawing)) {
      for (i in seq_along(rv$drawing$x)) {
        lines(c(rv$drawing$x[i], rv$drawing$prev_x[i]), 
              c(rv$drawing$y[i], rv$drawing$prev_y[i]), 
              col = rv$drawing$col[i], lwd = rv$drawing$lwd[i])
      }
    }
    
    # Add a point where the user clicks to start drawing
    if (!is.null(input$click)) {
      points(input$click$x, input$click$y, pch=19)
      
      # Save the starting point for the next line segment
      rv$start_x <- input$click$x
      rv$start_y <- input$click$y
      
      # Update coordinates display
      output$coord_text <- renderPrint(paste0("x: ", input$click$x, ", y: ", input$click$y))
      
    }
    
  }, height=400)
  
  # Clear drawing when clear button is clicked
  observeEvent(input$clear_btn, {
    rv$drawing <- NULL
  })
  
  # Add new line segment to drawing when user clicks and drags mouse
  observeEvent(input$drawing_area_mousemove,{
    
    if (!is.null(input$drawing_area_mousemove)) {
      
      # Create a new line segment with the starting point and current mouse position
      new_segment <- data.frame(x=c(rv$start_x, input$drawing_area_mousemove$x),
                                prev_x=c(rv$start_x, input$drawing_area_mousemove$x)[-2],
                                y=c(rv$start_y, input$drawing_area_mousemove$y),
                                prev_y=c(rv$start_y, input$drawing_area_mousemove$y)[-2],
                                col="black",
                                lwd=input$line_width)
      
      # If this is not the first segment of the drawing,
      # add it to the existing segments; otherwise create a new list of segments.
      if (is.null(rv$drawing)) {
        rv$drawing <- new_segment
      } else {
        rv$drawing <- rbind(rv$drawing,new_segment) 
      }
      
      # Update starting point for next line segment
      rv$start_x <- input$drawing_area_mousemove$x 
      rv$start_y <- input$drawing_area_mousemove$y
      
    }
    
  })
  
}

shinyApp(ui=ui,server=server)