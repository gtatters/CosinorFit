library(shiny)

phasefit<-function(CumulativeTime, Response){
  
  CosTime<-cos(2*pi*CumulativeTime / 24) # Create two new predictor variables, CosTime and SinTime
  SinTime<-sin(2*pi*CumulativeTime / 24)
  lm1<-lm(Response ~ CosTime + SinTime)
  
  summary(lm1)                    # Response can be modelled as a linear regression
  lm.coef<-coef(lm1)              # Extract regression coeficients
  names(lm.coef)<-NULL            # Remove names from the coeficients
  mesor<-lm.coef[1]               # mean level = average value
  A<-lm.coef[2]
  B<-lm.coef[3]
  amplitude<-sqrt(A^2+B^2)        # amplitude = peak - average, average - nadir
  acrophase<-atan2(-B, A)         # angle at which peak occurs
  
  if(-1*acrophase>0) acrotime<- (-1*acrophase*24/(2*pi))
  # if negative # then subtract from 24 to obtain acrophase in 0-24 range
  if(-1*acrophase<=0) acrotime<-24-1*acrophase*24/(2*pi)
  
  r.squared<-summary(lm1)$adj.r.squared
  pval<-anova(lm1)$`Pr(>F)`[1]
  
  phasevalues<-c(mesor, amplitude, acrotime, r.squared, pval)
  names(phasevalues)<-c("mesor","amplitude","acrotime","r2","pval")
  
  newtime<-seq(min(CumulativeTime), max(CumulativeTime),1)
  newtime<-seq(0,max(CumulativeTime),0.1)
  
  sintime<-sin(2*pi*newtime / 24)
  costime<-cos(2*pi*newtime / 24)
  
  fitresponse<-mesor+A*costime+B*sintime
  preds <- predict(lm1, interval = 'confidence', type="response")
  fits<-fitted(lm1)
  
  results<-list(mesor=mesor, amplitude=amplitude, acrotime=acrotime, 
                acrophase=acrophase, r.squared=r.squared, 
                pval=pval, newtime=newtime, fitresponse=fitresponse, fits=fits)
  
  return(results)
  
}

shinyServer(function(input, output, session){
  # Session is required to make the selectInput update after a file is uploaded
  
  # input$file1 will be NULL initially. After the user selects and uploads a 
  # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
  # columns. The 'datapath' column will contain the local filenames where the 
  # data can be found.
  
  url <- a("Sample File", href="https://raw.githubusercontent.com/gtatters/CosinorFit/master/AutorhythmSample.csv")
  
  output$tab <- renderUI({
    tagList("", url)
  })
  
  contentsrea <- reactive({
    
    inFile <- input$file1
    if (is.null(inFile))
       return(NULL)
    read.csv(inFile$datapath, header=input$header)
  })
  
  output$contents <- renderTable({
    contentsrea()
  })
  
  observe({
    updateSelectInput(session, "xcol", choices = names(contentsrea()))
  })
  
  observe({
    updateSelectInput(session, "ycol", choices = names(contentsrea()))
  })
    
  selectedData <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    d<-read.csv(inFile$datapath, header=input$header)
    d[, c(input$xcol, input$ycol)]
  })
  
  
 output$mainPlot <- renderPlot({
   inFile <- input$file1
   if (is.null(inFile))
     return(NULL)
    d<-read.csv(inFile$datapath, header=input$header)
    d<-d[, c(input$xcol, input$ycol)]
    
    CumulativeTime <- d[,1]
    Hour <- CumulativeTime - floor(CumulativeTime/24)*24
    Response <- d[,2]
    
    CosTime<-cos(2*pi*CumulativeTime / 24) # Create two new predictor variables, CosTime and SinTime
    SinTime<-sin(2*pi*CumulativeTime / 24)
    lm1<-lm(Response ~ CosTime + SinTime)
    
    summary(lm1)                    # Response can be modelled as a linear regression
    lm.coef<-coef(lm1)              # Extract regression coeficients
    names(lm.coef)<-NULL            # Remove names from the coeficients
    mesor<-lm.coef[1]               # mean level = average value
    A<-lm.coef[2]
    B<-lm.coef[3]
    amplitude<-sqrt(A^2+B^2)        # amplitude = peak - average, average - nadir
    acrophase<-atan2(-B, A)         # angle at which peak occurs
    
    if(-1*acrophase>0) acrotime<- (-1*acrophase*24/(2*pi))
    # if negative # then subtract from 24 to obtain acrophase in 0-24 range
    if(-1*acrophase<=0) acrotime<-24-1*acrophase*24/(2*pi)
    
    r.squared<-summary(lm1)$adj.r.squared
    pval<-anova(lm1)$`Pr(>F)`[1]
    
    phasevalues<-c(mesor, amplitude, acrotime, r.squared, pval)
    names(phasevalues)<-c("mesor","amplitude","acrotime","r2","pval")
    
    newtime<-seq(min(CumulativeTime), max(CumulativeTime),1)
    newtime<-seq(0,max(CumulativeTime),0.1)
    
    sintime<-sin(2*pi*newtime / 24)
    costime<-cos(2*pi*newtime / 24)
    
    fitresponse<-mesor+A*costime+B*sintime
    preds <- predict(lm1, interval = 'confidence', type="response")
    fits<-fitted(lm1)
    
    ind<-order(Hour)

    plot(Hour[ind], Response[ind], xaxp=c(0,24,4), pch=20, 
        xlab="Hour of Day", ylab="Temperature (°C)")
    #lines(Hour[ind], Response[ind])
    lines(newtime, fitresponse, lwd=3, col="grey")
    abline(mesor, 0, lwd=2, lty=2, col="black")
    abline(mesor+amplitude, 0, lwd=1, lty=2, col="red")
    abline(mesor-amplitude, 0, lwd=1, lty=2, col="blue")
    rect(acrotime, min(Response), acrotime, max(Response), lwd=3, lty=2)
    
  })
  
 output$residPlot <- renderPlot({

   inFile <- input$file1
   if (is.null(inFile))
     return(NULL)
   d<-read.csv(inFile$datapath, header=input$header)
   d<-d[, c(input$xcol, input$ycol)]
   
   CumulativeTime <- d[,1]
   Hour <- CumulativeTime - floor(CumulativeTime/24)*24
   Response <- d[,2]
   
   CosTime<-cos(2*pi*CumulativeTime / 24) # Create two new predictor variables, CosTime and SinTime
   SinTime<-sin(2*pi*CumulativeTime / 24)
   lm1<-lm(Response ~ CosTime + SinTime)
   oldpar<-par()
   par(mfrow=c(2,2))
   plot(lm1)
   par<-oldpar
   
 })
 
 output$modelSummary <-renderPrint({
   inFile <- input$file1
   if (is.null(inFile))
     return(NULL)
   d<-read.csv(inFile$datapath, header=input$header)
   d<-d[, c(input$xcol, input$ycol)]
   
   CumulativeTime <- d[,1]
   Hour <- CumulativeTime - floor(CumulativeTime/24)*24
   Response <- d[,2]
   
   CosTime<-cos(2*pi*CumulativeTime / 24) # Create two new predictor variables, CosTime and SinTime
   SinTime<-sin(2*pi*CumulativeTime / 24)
   lm1<-lm(Response ~ CosTime + SinTime)
   summary(lm1)                   
   
 })
 
 output$equationtable <-renderTable({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    d<-read.csv(inFile$datapath, header=input$header)
    d<-d[, c(input$xcol, input$ycol)]
    
    CumulativeTime <- d[,1]
    Hour <- CumulativeTime - floor(CumulativeTime/24)*24
    Response <- d[,2]
    
    CosTime<-cos(2*pi*CumulativeTime / 24) # Create two new predictor variables, CosTime and SinTime
    SinTime<-sin(2*pi*CumulativeTime / 24)
    lm1<-lm(Response ~ CosTime + SinTime)
    
    summary(lm1)                    # Response can be modelled as a linear regression
    lm.coef<-coef(lm1)              # Extract regression coeficients
    names(lm.coef)<-NULL            # Remove names from the coeficients
    mesor<-lm.coef[1]               # mean level = average value
    A<-lm.coef[2]
    B<-lm.coef[3]
    amplitude<-sqrt(A^2+B^2)        # amplitude = peak - average, average - nadir
    acrophase<-atan2(-B, A)         # angle at which peak occurs
    
    if(-1*acrophase>0) acrotime<- (-1*acrophase*24/(2*pi))
    # if negative # then subtract from 24 to obtain acrophase in 0-24 range
    if(-1*acrophase<=0) acrotime<-24-1*acrophase*24/(2*pi)
    
    r.squared<-summary(lm1)$adj.r.squared
    pval<-anova(lm1)$`Pr(>F)`[1]
    
    digs<-10
    phasevalues<-data.frame(format(mesor, digits=digs), 
                            format(amplitude, digits=digs),
                            format(acrotime, digits=digs),
                            format(r.squared, digits=digs),
                            format(pval, digits=digs))
    colnames(phasevalues)<-c("Mesor","Amplitude","Acrophase","R.Squared","P")
    
    phasevalues
    
      })
  
})

