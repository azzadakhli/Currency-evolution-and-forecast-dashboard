ui <- fluidPage (tags$style('.container-fluid {
                             background-color: #E0FFFF;
              }'),
                 tags$style(
                   ".title {  color: #008080;
                                 background-color: white;
                                 font-size: 40px;
                                 font-style: italic;}"
                 )
                 ,tags$div(class="title", titlePanel(h1("Your Dashboard",align="center"))) ,
                 
                 sidebarLayout(
                   sidebarPanel( fileInput("file", "Choose excel File",
                                           multiple = FALSE),h4("Time Serie plot"),hr(),selectInput("variable","Select your variable",""),hr(),
                                 h4("For the barplot"),hr(),
                                 dateInput("date", label = "Choose a date",value = "",
                                           min = "",
                                           max = "",
                                           format = "yyyy-mm-dd"),
                                 hr(),
                                 h4("For the simple plot"),hr(),
                                 selectInput("x","Select your 1st variable",""),
                                 selectInput("y","Select your 2nd variable","")
                                 ,width=4),
                   mainPanel(
                     navbarPage("Your Dashboard",
                                tabPanel("Data",h4("Here's a preview of your data"), tableOutput("tab")),
                                tabPanel("Plots",verbatimTextOutput("text0"), plotOutput("TimeSerie"),hr()
                                         ,verbatimTextOutput("text3"),hr()
                                         ,plotOutput("Barplot"),hr(),verbatimTextOutput("text4"),
                                         plotOutput("plt"),
                                         tags$head(tags$style("#text0{color: #E0FFFF;
                                 background-color: #20B2AA;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                         )
                                         ),tags$head(tags$style("#text3{color: #E0FFFF;
                                 background-color: #20B2AA;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                         )
                                         ),
                                         tags$head(tags$style("#text4{color: #E0FFFF;
                                 background-color: #20B2AA;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                         )
                                         )
                                ),
                                tabPanel("Summary", verbatimTextOutput("text2"), verbatimTextOutput("text1"),
                                         tags$head(tags$style("#text1{color: #E0FFFF;
                                 background-color: #20B2AA;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                         )
                                         ), tags$head(tags$style("#text2{color:#E0FFFF;
                                 background-color: #20B2AA;
                                 font-size: 20px;
                                 font-style: italic;
                                 }"
                                         )
                                         )),
                                tabPanel("Arima Model",h4("Arima Modeling and forecasting"),h5("Augmented Dickey–Fuller:Testing for stationarity"),
                                         verbatimTextOutput("adf"),h5("Kwiatkowski-Phillips-Schmidt-Shin: Testing for trend stationarity"),
                                         verbatimTextOutput("kpss"),h5("ARIMA Parameters:"),
                                         fluidRow(splitLayout(cellWidth=c("50%","50%"),plotOutput("acf"),plotOutput("pacf")))),
                                verbatimTextOutput("model"),plotOutput("forecast")),
                     position = c("left", "right")
                   )))

server <- function(output,input,session){
  Df <- reactive({
    req(input$file)
    if(is.null(input$file))return(NULL) 
    as.data.frame(read_excel(input$file$datapath))
  })
  observe({
    #all_na <- function(x) any(!is.na(x))
    #Df[,which(unlist(lapply(df, function(x) !all(is.na(x)))))]
    updateSelectInput(
      session=session ,
      inputId="variable",
      choices=names(Df()),
      selected=names(Df())[2])
    updateDateInput(
      session = session,
      inputId = "date",
      value = as.Date(Df()$Date)[1],
      min = as.Date(Df()$Date)[length(Df()$Date)],
      max = as.Date(Df()$Date)[1])
    updateSelectInput(
      session=session ,
      inputId="x",
      choices=names(Df()),
      selected=names(Df())[2])
    updateSelectInput(
      session=session ,
      inputId="y",
      choices=names(Df()),
      selected=names(Df())[2])
  })
  
  output$tab <- renderTable({
    head(Df())
  })
  
  output$text0 <- renderText({
    paste("This is the graph associated to the",input$variable)
  })
  output$TimeSerie <- renderPlot( {
    Dates<-as.Date(Df()$Date)
    Ts<- data.frame(Dates,df[input$variable])
    names(Ts) <- c("x","y")
    ggplot(Ts,aes(x=x,y=y))+geom_line(color="blue") +xlab("Date")+
      ylab(paste("Evolution of ",input$variable))+theme_economist()
  })
  output$Barplot<- renderPlot({
    #data=Df()
    #Dates<-as.Date(data["Date"])
    #row.names(data)<-Dates
    #data< -subset(data, select = -Date)
    #l1<- data[input$date,]
    #l2<-unlist(l1)[2:length(l2)]
    #paste(Dates)
    # v<-Vec(vals(l1))
    barplot(as.numeric(l5) , names.arg = names(l5))
  })
  output$text4 <- renderText({
    paste("You chose to visualize the plot of",input$x,"in terms of",input$y)
  })
  output$plt <- renderPlot({
    Df1<- data.frame(df[input$x],df[input$y])
    names(Df1) <- c("x","y")
    ggplot(Df1,aes(x=x,y=y))+geom_line(color="blue") +xlab("Date")+
      ylab(paste("Evolution of ",input$x,"and",input$y ))+theme_economist()
  })
  output$text1 <- renderText({
    data<- Df()
    paste("The minimum is",summary(data[input$y])[1],"\n","The maximum is",summary(data[input$y])[6],"\n",
          "The Median is",summary(data[input$y])[3],"\n","The mean is",summary(data[input$y])[3],"\n",
          "The 1st Quartile is",summary(data[input$y])[2],"\n","2nd Quartile",summary(data[input$y])[5])
  })
  output$text2 <- renderText({
    paste("You chose to visualize the summary of",paste(input$variable))
  })
  output$text3 <- renderText({
    paste("The date you chose is",paste(input$date) )
  })
  
  output$adf<-renderPrint({
    data=Df()
    timeseries=ts(data[,c(input$variable)])
    count_ts=ts(na.omit(data[input$variable]), frequency=12)
    data[input$variable]=tsclean(timeseries)
    adf<-adf.test(count_ts, alternative = "stationary")
    adf
  })
  output$kpss<-renderPrint({
    data=Df()
    timeseries=ts(data[,c(input$variable)])
    count_ts=ts(na.omit(data[input$variable]), frequency=12)
    data[input$variable]=tsclean(timeseries)
    model1<-auto.arima(count_ts, seasonal = F)
    model1
    # kpss=kpss.test(count_ts)
    #kpss
  })
  output$acf <- renderPlot({
    data=Df()
    timeseries=ts(data[,c(input$variable)])
    count_ts=ts(na.omit(data[input$variable]),frequency=12)
    Acf(count_ts, main='')
  })
  output$pacf <- renderPlot({
    data=Df()
    timeseries=ts(data[,c(input$variable)])
    count_ts=ts(na.omit(data[input$variable]), frequency=12)
    Pacf(count_ts, main='')
  })
  output$model <- renderText({
    data=Df()
    timeseries=ts(data[,c(input$variable)])
    count_ts=ts(na.omit(data[input$variable]),frequency=12)
    model1<-auto.arima(count_ts, seasonal = F)
    paste(model1)
  })
  output$forecast <- renderPlot({
    data=Df()
    timeseries=ts(data[,c(input$variable)])
    count_ts=ts(na.omit(data[input$variable]),frequency=12)
    model1<-auto.arima(count_ts, seasonal = F)
    fcast<-forecast(model1, h=22)
    plot(fcast)
  })
  
}

shinyApp(ui,server)
