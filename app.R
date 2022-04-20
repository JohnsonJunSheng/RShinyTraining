library(shiny)
library(shinydashboard)
library(formattable)
library(readxl)

ui <- dashboardPage(
  dashboardHeader(title="R Shiny Assignment"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Claims_Data",tabName="Claims_Data",icon=icon("Claims_Data")),
      menuItem("Stimulating",tabName="Stimulating",icon=icon("Stimulating"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName="Claims_Data",
              fluidPage(
                titlePanel("Uploading Files"),
                sidebarLayout(
                  sidebarPanel(
                    fileInput("file1", "Choose CSV File",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    tags$hr(),
                    checkboxInput("header", "Header", TRUE),
                    radioButtons("sep", "Separator",
                                 choices = c(Comma = ",",
                                             Semicolon = ";",
                                             Tab = "\t"),
                                 selected = ","),
                    radioButtons("quote", "Quote",
                                 choices = c(None = "",
                                             "Double Quote" = '"',
                                             "Single Quote" = "'"),
                                 selected = '"'),
                    tags$hr(),
                    radioButtons("disp", "Display",
                                 choices = c(Head = "head",
                                             All = "all"),
                                 selected = "all")
                    
                  ),
                  mainPanel(
                    box(tableOutput("contents"),width=12)
                  )
                  
                )
              )
      ),
      
      tabItem(tabName="Stimulating",
              fluidPage(
                titlePanel("Stimulating Cumulative Paid Claims"),
                box(plotOutput("graph"),title="Graph",width=12),
                box(tableOutput("table"),title="Cumulative Paid Claims",align="center",width=6),
                box(sliderInput("slider","Tail Factor:",1.00,2.00,1.10),title="Input Parameter",align="center",width=6)
                
              )
      )
    )
  )
)

server <- function (input,output,session) {
  
  ##Displaying the file uploaded
  output$contents <- renderTable({
    req(input$file1)
    tryCatch(
      {
        ClaimsData <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(ClaimsData))
    }
    else {
      return(ClaimsData)
    }
  })
  
  ###########################################
  ClaimsData <- reactive({
    req(input$file1, input$header, file.exists(input$file1$datapath))
    data.matrix(read.csv(input$file1$datapath, header = input$header))
  })
  
  PC <- reactive({
    
    PC <- matrix(NA, nrow = 3, ncol = 4)
    rownames(PC) <- c('Loss Year 2017','Loss Year 2018','Loss Year 2019')
    colnames(PC) <- c('Development Year 1','Development Year 2','Development Year 3','Development Year 4')
    
    req(ClaimsData())
    ClaimsData=ClaimsData()
    
    for ( i in 1:3 ) {
    n <- 2016 + i;
    
    for ( j in 1:6 ) {
      if ( ClaimsData[j,1] == n ) {
        
        for ( k in 1:3 ) {
          if ( ClaimsData[j,2] == k ) {PC[i,k] <- ClaimsData[j,3]}
          
        }
      }
    }
  }
  })
  
  CPC <- reactive({
    
    CPC <- matrix(NA, nrow = 3, ncol = 3)
    
    req(PC())
    PC=PC()
    
    for ( i in 1:3 ) {
      for ( j in 1:3 ) {
        CPC[i,j] <- sum(PC[i,(1:j)])
      }
      }
  
    for ( i in 1:3 ) {
      for ( j in 1:3 ) {
       if ( is.na (CPC[i,j]) == TRUE ) {
        
         for ( k in 1:3 ) {
            if ( is.na (sum(CPC[k,j])) == TRUE ) {
              CPC[i,j] <- CPC[i,(j-1)] * sum(CPC[1:(k-1),j]) / sum(CPC[1:(k-1),(j-1)]);
              break
          }
        }
      }
    }
    }
  })
  
  CPCR <- reactive({
    
    CPCR <- matrix(NA, nrow = 3, ncol = 1)
    
    req(CPC())
    CPC=CPC()
    
    for ( i in 1:3 ) {
      CPCR[i,1] <- CPC[i,3]
    }

  })
  
  R1 <- reactive({
    req(CPC())
    CPC=CPC()
    CPC[1,3]*input$slider
  })
  
  R2 <- reactive({
    req(CPC())
    CPC=CPC()
    CPC[2,3]*input$slider
  })
  
  R3 <- reactive({
    req(CPC())
    CPC=CPC()
    CPC[3,3]*input$slider
  })
  
  CPCO <- reactive({
    req(R1(),R2(),R3())
    R1=R1()
    R2=R2()
    R3=R3()
    cbind(CPC(),rbind(R1,R2,R3))
})
  
  CPCO1 <- reactive({
    req(CPC(),R1())
    CPC=CPC()
    R1=R1()
    currency(c(CPC[1,1:3],R1()),symbol="$",big.mark = ",")})
  
  CPCO2 <- reactive({
    req(CPC(),R2())
    CPC=CPC()
    R2=R2()
    currency(c(CPC[2,1:3],R2()),symbol="$",big.mark = ",")})
  
  CPCO3 <- reactive({
    req(CPC(),R3())
    CPC=CPC()
    R3=R3()
    currency(c(CPC[3,1:3],R3()),symbol="$",big.mark = ",")})
###################################################################################
  
  
  output$graph <- renderPlot({
    req(CPCO1(),CPCO2(),CPCO3())
    CPCO1=CPCO1()
    CPCO2=CPCO2()
    CPCO3=CPCO3()
    plot(x<-c(1,2,3,4), CPCO1(),
         type='p',frame=FALSE,pch=19,
         col='green',ylab='Cumulative Paid Claims ($)',xlab='Development Year', 
         xlim=c(1,4),ylim=c(500000,2500000),xaxt="n") +
      axis(1, at = 1:4) +
      lines(x, CPCO2(),
            type='p',pch=19,col='orange') +
      lines(x, CPCO3(),
            type='p',pch=19,col='blue') +
      
      lines(x,predict((lm(CPCO1()~poly(x,3))),data.frame(x=seq(1:4))),col='green') + text(x,CPCO1(),labels=round(CPCO1()),cex=0.8,pos=3) +
      lines(x,predict((lm(CPCO2()~poly(x,3))),data.frame(x=seq(1:4))),col='orange') + text(x,CPCO2(),labels=round(CPCO2()),cex=0.8,pos=3) +
      lines(x,predict((lm(CPCO3()~poly(x,3))),data.frame(x=seq(1:4))),col='blue') + text(x,CPCO3(),labels=round(CPCO3()),cex=0.8,pos=3);
  })
  
  output$table <- renderTable(CPCO(),rownames=TRUE,colnames=TRUE)
  }

shinyApp(ui,server)