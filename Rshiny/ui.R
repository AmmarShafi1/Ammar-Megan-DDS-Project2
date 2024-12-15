library(shiny)

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Data Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Choose Plot Type:", 
                  choices = c("Scatterplot", "Bar Chart (Count)", "Percentage Bar Chart")),
      selectInput("x_attr", "Choose X-axis Attribute:", choices = NULL),
      
      # Conditional panels for non-scatterplot options
      conditionalPanel(
        condition = "input.plot_type != 'Scatterplot'",
        sliderInput("bins", "Number of Bins (For Numerical Attributes):", min = 2, max = 10, value = 5),
        checkboxInput("show_totals", "Show Totals on Bars", value = FALSE)
      ),
      
      # Conditional panel for scatterplot options
      conditionalPanel(
        condition = "input.plot_type == 'Scatterplot'",
        checkboxInput("facet_by_quality", "Separate by Quality", value = FALSE)
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)