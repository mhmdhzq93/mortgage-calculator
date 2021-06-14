library(shiny)

options(scipen = 999)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Mortgage Calculator"),

  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      numericInput("principal", "Principal (loan amount)", 200000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "Annual interest rate (in %)", 2, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "Duration of the loan (in years)",
                  min = 0,
                  max = 30,
                  value = 25,
                  step = 1
      ),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- P * J / (1 - (1 + J)^(-N))
    monthPay <<- M
  }
  
  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0(
      "<h3>", "Summary", "</h3>",
      "Principal (loan amount): ", format(round(input$principal, 2), big.mark = ","),
      "<br>",
      "Annual interest rate: ", input$interest, "%",
      "<br>",
      "Term: ", input$length, " years (", input$length * 12, " months)",
      "<br>",
      "<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)