

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  headerPanel(""),

  sidebarPanel(  
    textInput("stock_price",
                strong("Stock Price:"),
                100),
    br(),
    selectInput("sigma", 
                strong('Sigma:'), 
                choices = seq(0, 1, by=.05), selected=.5),
    br(),
    selectInput("time_yr", 
                strong('Time (In Years):'), 
                choices = seq(1, 100, by=.25), selected=1),
    br(),
    selectInput("dividends", 
                strong('Dividends'), 
                choices = seq(.01, 100, by=.01), selected=.01),
    br(),
    selectInput("rate", 
                strong('Rate'), 
                choices = seq(.01, 1, by=.01), selected=.01),
    br(),
    textInput("strike_price", 
                strong('Strike Price'), 
                100),
    br(),
    selectInput("number_stock_price", 
                strong('Number of Stock Prices'), 
                choices = seq(1, 500, by=1), selected=100),
    br(),
    selectInput("simulation_number", 
                strong('Simulation Number'), 
                choices = seq(100, 10000, by=100), selected=100)
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Main", 
               plotOutput("stockPlot"),
               HTML('<table border=0 width="100%"><tr bgcolor="#f5f5f5"><td>'),
               div(style="width:80%;max-width:300px;"),
               HTML('</td><td>'),
               HTML('</td></tr></table>'),
               tableOutput("statsTable"),
               downloadButton("downloadReport", "Download Report"),
               downloadButton("downloadData", "Download Data")  
      ),			
      
      tabPanel("About",
               p('This application allows the user to simulate Asian Option prices using 3 methods using the ',
                 a("Shiny", href="http://www.rstudio.com/shiny/", target="_blank"), 'framework',
                 '. Example of Asian option head over to ',
                 a("Wiki Asian option:", href="http://en.wikipedia.org/wiki/Asian_option", target="_blank"),
                 '.'),
               
               br(),
               
               strong('Author'), p('Team Stochastic Static!', target="_blank"),
               
               br(),
               
               strong('Code'), p('Original source code for this application at',
                                 a('GitHub', href='https://github.com/Eric-Stewart/4830_project')),
               
               br(),
               
               strong('References'),
               p(HTML('<ul>'),
                 HTML('<li>'),'The web application is built with the amazing', a("Shiny.", href="http://www.rstudio.com/shiny/", target="_blank"),HTML('</li>'),
                 HTML('</ul>'))
    )     
    )
  )
))

