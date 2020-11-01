library(shiny)



d<<-read.csv("AutorhythmSample.csv")

shinyUI(pageWithSidebar(
  headerPanel("Cosinor Fit"),
  sidebarPanel(
    h5("Data Input"),
    helpText("This app performs a cosinor analysis on daily periodic (24 h) data. 
             Load in your csv file below.  The file should be formatted with one
             column indicating the cumulative time in hours, espressed as simple decimal hours, with midnight = 0.  
             For example, the first day values will range from 0.0 to 24.0, second day
             values range from 24.0 to 48.0, etc."),
    br(),
    fileInput('file1', 'Choose CSV File containg autorhythm data',
              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
    tags$hr(),
    
    checkboxInput('header', 'Header', TRUE),
    
    selectInput('xcol', 'Select the Cumulative Time Variable', 
                choices=names(d), selected=names(d)[[1]]),
    
    selectInput('ycol', 'Select the Response Variable', 
                choices=names(d), selected=names(d)[[3]]),
    
    sliderInput("digs", "Significant Digits for Equation Parameters:",
                min = 2, max = 10, value = 5),
    
    br(),
    
    helpText("Glenn Tattersall, PhD"),
    HTML("https://TattersallLab.com"),
    br(),
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Data",
               h5("If the data window is blank, download the sample file here for a guide on how to set up your data: "),
               uiOutput("tab"),
               tableOutput("contents")
      ),
      tabPanel("Plot",
               h5("The chart shows a best fit sinusoidal plot through the response variable."),
               plotOutput("mainPlot")

      ),
      tabPanel("Residuals",
               h5("The residuals allow you to examine the appropriateness of the fit."),
               plotOutput("residPlot"),
               h5("For help in interpreting these plots, see: "),
               HTML("https://data.library.virginia.edu/diagnostic-plots/")
               
      ),
      tabPanel("Statistics",
               h4("The statistical summary table:"),
               verbatimTextOutput("modelSummary")
               ),
      
      tabPanel("Equation Parameters",
               h4("The estimated equation parameters are:"),
               br(),
               fluidRow(
                 column(12,
                        tableOutput("equationtable")
                 )),
               br(),
               h4("Derived from fitting the following equation:"),
               withMathJax("$$R = M + A\\cdot cos(2\\cdot \\pi\\cdot  \\frac{t}{24}) + B\\cdot sin(2\\cdot \\pi\\cdot \\frac{t}{24})$$"),
               p("Where R is the response variable,"),
               p("A and B are the slope parameters,"),
               p("t is the time variable, expressed in hours, with midnight on day 0 = 0,"),
               p("M is the Mesor, or mean of the response variable."),
               p("Amplitude (in units of the response variable) can be calculated as:"),
               withMathJax("$$Amplitude = \\sqrt{(A^2+B^2)}$$"),
               p("Acrophase (in hours) can be calculated as:"),
               withMathJax("$$Acrophase = \\arctan{\\frac{-B}{A}}$$"),
               h4("")
     
      )
    )
  )
)
)


