library(shiny)
library(readxl)

ClaimsData <- data.matrix(read_excel("~/Desktop/RShinyTraining/RShinyAssignment.xlsx", sheet = "Claims Data"))


PC <- matrix(NA, nrow = 3, ncol = 4)
rownames(PC) <- c('Loss Year 2017','Loss Year 2018','Loss Year 2019')
colnames(PC) <- c('Development Year 1','Development Year 2','Development Year 3','Development Year 4')

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

CPC <- matrix(NA, nrow = 3, ncol = 3)
rownames(CPC) <- rownames(PC)
colnames(CPC) <- colnames(PC[1:3,1:3])
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


CPCR <- matrix(NA, nrow = 3, ncol = 1)
rownames(CPCR) <- rownames(PC)
colnames(CPC) <- colnames(PC[1:3,4])

for ( i in 1:3 ) {
  CPCR[i,1] <- CPC[i,3]
}

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("slider","Tail Factor:",1.00,2.00,1.10)
    ),
  
    mainPanel(
      plotOutput(outputId="graph"),
      tableOutput(outputId="table")
  )
)
)


server <- function (input,output,session) {
  output$graph <- renderPlot({
    plot(x<-seq(1:4), CPCO1 <- c(CPC[1,1:3],CPCR[1,1]*input$slider),
         type='p',frame=FALSE,pch=19,
         col='green',ylab='Cumulative Paid Claims ($)',xlab='Development Year',
         xlim=c(1,4.1),ylim=c(500000,2500000)) +
      lines(x, CPCO2 <- c(CPC[2,1:3],CPCR[2,1]*input$slider),
            type='p',pch=19,col='orange') +
      lines(x, CPCO3 <- c(CPC[3,1:3],CPCR[3,1]*input$slider),
            type='p',pch=19,col='blue') +
    
      lines(x,predict((lm(CPCO1~poly(x,3))),data.frame(x=seq(1:4))),col='green') + text(x,CPCO1,labels=round(CPCO1),cex=0.8,pos=3) +
      lines(x,predict((lm(CPCO2~poly(x,3))),data.frame(x=seq(1:4))),col='orange') + text(x,CPCO2,labels=round(CPCO2),cex=0.8,pos=3) +
      lines(x,predict((lm(CPCO3~poly(x,3))),data.frame(x=seq(1:4))),col='blue') + text(x,CPCO3,labels=round(CPCO3),cex=0.8,pos=3);
    
    CPCO <- cbind(CPC,CPCR * input$slider)
    colnames (CPCO) <- colnames (PC)
    rownames (CPCO) <- rownames (PC)
    
  output$table <- renderTable(CPCO,digits=0,rownames=TRUE,colnames=TRUE)
  })
}

shinyApp(ui,server)
