library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Stock Portfolio Guru - G6"),
  
  sidebarPanel(
    
    tags$head(
      tags$style(type="text/css", "label.radio { display: inline-block; }", ".radio input[type=\"radio\"] { float: none; }"),
      tags$style(type="text/css", "select { max-width: 200px; }"),
      tags$style(type="text/css", "textarea { max-width: 185px; }"),
      tags$style(type="text/css", ".jslider { max-width: 200px; }"),
      tags$style(type='text/css', ".well { padding: 12px; margin-bottom: 5px; max-width: 280px; }"),
      tags$style(type='text/css', ".span4 { max-width: 280px; }")
    ),
    
    selectInput("profile", "Investment Profile:",
                c("Distribute Equally" = "equal",
                  "Weighted by Returns" = "weighted",
                  "Highest Returns" = "hi.ret")
    ),
    
    br(),
    br(),
    sliderInput("risk.limit", "Risk Appetite:", 
                min=0, max=5, value=3,  step= 0.1),
    br(),
    
    checkboxGroupInput("stocks", "Stock:",
                       list("Asian Paints"="Asian Paints.csv", 
                            "Bharati Airtel"="Bharati Airtel.csv", 
                            "BHEL"="BHEL.csv", 
                            "Cipla"="Cipla.csv", 
                            "Coal INDIA Ltd"="Coal INDIA Ltd.csv", 
                            "DLF"="DLF.csv", 
                            "Dr. Reddy's"="Dr. Reddy's.csv", 
                            #                             "GAIL"="GAIL.csv", 
                            #                             "HCL TECH"="HCL TECH.csv", 
                            "HDFC Bank"="HDFC Bank.csv", 
                            "Hero Motor Corp Ltd"="Hero Motor Corp Ltd.csv", 
                            "ICICI Bank"="ICICI Bank.csv", 
                            "ITC"="ITC.csv", 
                            "Jindal Steel"="Jindal Steel.csv", 
                            "LT"="LT.csv", 
                            "Mahindra & Mahindra"="Mahindra & Mahindra.csv", 
                            "Maruti Suzuki"="Maruti Suzuki.csv", 
                            "ONGC"="ONGC.csv", 
                            #                             "Punjab National Bank"="Punjab National Bank.csv", 
                            #                             "Ranbaxy"="Ranbaxy.csv", 
                            "SBI"="SBI.csv", 
                            "Tata Motors"="Tata Motors.csv", 
                            "Tata Steel"="Tata Steel.csv", 
                            "TCS"="TCS.csv", 
                            "Ultra Tech Cements"="Ultra Tech Cements.csv")),
    
    br(),
    submitButton("Show Results") 
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Portfolio Analysis", plotOutput("returnsPlot", height=700)),
      tabPanel("Summary", tableOutput("returnsTable"), verbatimTextOutput("returnsDetails")), 
      tabPanel("Individual Stocks", plotOutput("indivPlots", height="auto"))
    )
  )
))
