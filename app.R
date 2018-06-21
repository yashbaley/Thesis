library(shiny)
library(readxl)
library(shinyjs)
library(shinythemes)
library(ggplot2)
library(shinydashboard)
library(corrplot)
library(tableHTML)
library(DT)
library(randomForest)
library(dplyr)
library(caret)
library(e1071)
library(lattice)
library(rintrojs)
library(ggthemes) 

ui <-
  dashboardPage(skin = "black",
                dashboardHeader(title = "Dual Degree Project",titleWidth = 300),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Home", tabName = "Home", icon = icon("home")),
                    menuItem("Upload Data here!", tabName = "Data", icon = icon("table")),
                    menuItem("Predictions", tabName = "LR", icon = icon("line-chart")),
                    menuItem("Data Insight", tabName = "DI", icon = icon("eye")),
                    menuItem("Contact", tabName = "About", icon = icon("address-book"))
                  )
                ),
                
                dashboardBody(
                  
                  tags$head(
                    tags$style(HTML("
                                    @import url('//fonts.googleapis.com/css?family=Georgia|Cabin:400,700');
                                    
                                    h1 {
                                    font-family: 'Georgia';
                                    font-weight: normal;
                                    line-height: 1;
                                    color: black;
                                    }
                                    h5 {
                                    font-family: 'Georgia';
                                    font-weight: normal;
                                    line-height: 1.1;
                                    color: black;
                                    font-size = 24px;
                                    font-variant: small-caps;
                                    }
                                    
                                    h4 {
                                    font-family: 'Georgia';
                                    font-weight: bold;
                                    font-color = red;
                                    line-height: 1;
                                    color: black;
                                    font-size = 20px;
                                    }
                                    
                                    "))
                    ),
                  
                  
                  fluidPage(
                    tags$style(make_css(list('.box', 
                                             c('font-size', 'font-family', 'color'), 
                                             c('14px', 'Georgia', 'Grey')))),
                    
                    tags$head(tags$style(HTML('
                                              .main-header .logo {
                                              font-family: "Georgia", Times, "Georgia", serif;
                                              font-weight: bold;
                                              font-size: 24px;
                                              }
                                              '))),
                    
                    #Selecting theme
                    #shinythemes::themeSelector(),
                    #theme = shinytheme("united"),
                    useShinyjs(),
                    fluidRow(
                      img(height = 100, 
                          width = 100,src="https://upload.wikimedia.org/wikipedia/en/thumb/5/58/IIT_Bombay_Logo.svg/1200px-IIT_Bombay_Logo.svg.png", 
                          align = "left"),
                      img(height = 100, 
                          width = 100,src="https://i.pinimg.com/originals/eb/0e/d5/eb0ed51dac78c6f5873bcb8099416401.png", 
                          align = "right"),
                      
                      
                      column(8,h1("Data Analytics in Warranty Management"),align = "center",offset = 1),
                      
                      
                      HTML('<hr style="color: white;">')
                      
                    ),
                    ##Making tabs
                    tabItems(
                      #Tab1 - Home
                      tabItem(tabName = "Home",icon = icon("home"),
                              #sidebarLayout(
                              # sidebarPanel( titlePanel(h3("Application of Data
                              #                            Analytics in Warranty Management"))),
                              #
                              #mainPanel (h6("This application is focused on analyzing data related to royal
                              #             Enfield customers.
                              #            Machine learning and cash flow analysis are incorporated to 
                              #           optimize warranty policy.
                              #          ")
                              #                     )
                              #                    ),
                              box(width = 20,height = 5),
                              fluidRow(
                                infoBox(icon = icon("bullseye","fa-1.5x"),title = h5("Aim"),value = h4("Application of Data Analytics in Warranty Management"),
                                        width = "100%",fill = FALSE,color = "green")
                              ),
                              fluidRow(
                                infoBox(icon = icon("angle-double-up","fa-1.5x"),title = h5("Purpose"),
                                        value = h4("This application is focused on analyzing data related to Royal
                                                   Enfield customers. Machine learning and cash flow analysis are incorporated to 
                                                   optimize warranty policy."),
                                        width = "100%",fill = FALSE,color = "purple")
                                ),
                              fluidRow(
                                infoBox(icon = icon("check","fa-1.5x" ),title = h5("Deliverables"),
                                        value = h4("A. B. C. D."),
                                        width = "100%",fill = FALSE,color = "light-blue")
                              ),
                              fluidRow(
                                infoBox(icon = icon("file","fa-1.5x"),title = h5("Project Report"),
                                        value = h4("Click to view report"),href = "https://bighome.iitb.ac.in/index.php/s/A58PBrp8NnmkiWJ",
                                        width = "100%",fill = FALSE,color = "aqua")
                              )
                              
                                ),  #End of Tab1
                      #Tab 2 - Load Data
                      tabItem(tabName = "Data",icon = icon("table"),
                              tabsetPanel(
                                tabPanel("Select Data",
                                         box(
                                           fileInput("file1", 'Choose Failure Data and Customer profile CSV File',
                                                     accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
                                         ,width = 6),
                                         box(
                                           fileInput("file2", 'Choose Ratings CSV File',
                                                     accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
                                         ,width = 6)
                                ),
                                tabPanel("View Data Tables & Summary",
                                         "Summary",
                                         DTOutput(outputId = 'DataSummary'),
                                         
                                         fluidRow(
                                           actionButton("hideshow", "Show/Hide Data"),
                                           div(style = 'overflow-x: scroll', DT::dataTableOutput('tableOutput'))
                                           #DTOutput(outputId = 'tableOutput')
                                         ),
                                         fluidRow(
                                           actionButton("hideshow3", "Show/Hide Data"),
                                           div(style = 'overflow-x: scroll', DT::dataTableOutput('tableOutputCPV'))
                                         )
                                )
                              )
                              #checkboxInput("showModel1", "Show/Hide Model 1", value = FALSE)
                              
                      ), #End of Tab 2
                      #Tab 3
                      tabItem(tabName = "LR",icon = icon("line-chart", lib = "font-awesome"),
                              tabsetPanel(
                                tabPanel("Linear Regression",
                                         box(
                                           selectInput('xcol', 'X Variable', "abc", selected = "Please select data first"),
                                           selectInput('ycol', 'Y Variable', "pqr", selected = ""),
                                           width = "100%"
                                         ),
                                         box(
                                           plotOutput("regression"),
                                           width = "100%"
                                           #, plotOutput("linear1")
                                         )
                                         
                                ),
                                tabPanel("Algos",
                                         box(
                                           selectInput('algo_name', 
                                                       'Please select an algorithm',
                                                       "Please select data first"), width = 6),
                                         box(
                                           selectInput('y_var', 
                                                       'Please select month to predict failure',
                                                       "Please select data first"),
                                           width = 6),
                                         downloadButton("downloadData", "Download"),
                                         verbatimTextOutput('conf_matrix')
                                         #fluidRow(column(7,dataTableOutput('dto')))
                                         #tableOutput('conf_matrix_csv')
                                         
                                )
                              )
                      ), #End of tab 3
                      tabItem(tabName = "DI",icon = icon("eye"),
                              tabsetPanel(
                                
                                # tabPanel("Show/Hide Data",
                                #         fluidRow(
                                #          actionButton("hideshow2", "Show/Hide Data")
                                #         
                                #      )
                                # ),
                                
                                tabPanel("Random Forest","Importance Plot",
                                         fluidRow(
                                           plotOutput("rfplot")
                                         )
                                ),
                                tabPanel("Data Insights",
                                         
                                         fluidRow(
                                           box(
                                             selectInput('feature1', 'Select 1st Feature', "abc"),
                                             selectInput('feature2', 'Select 2nd Feature', "pqr", selected = "")
                                           ),
                                           box(selectInput('A', 'Select 1st Factor', "abc"),
                                               selectInput('B', 'Select 2nd Factor', "pqr", selected = ""),
                                               selectInput('C', 'Select 3rd Factor', "abc")
                                           ),
                                           box(DTOutput('tableOutput2'),width = 
                                                 "100%"),
                                           
                                           plotOutput(outputId = 'plot1',width = "100%",height = 600)
                                           
                                           # plotOutput(outputId = 'corrplot')
                                           #actionButton("hideshow2", "Show/Hide Data"),
                                           #tableOutput(outputId = 'tableOutput2')
                                         )
                                ),
                                
                                tabPanel("Data Summary",
                                         "Summary",
                                         tableOutput(outputId = 'DataSummary2')
                                )
                                
                              )
                              #checkboxInput("showModel1", "Show/Hide Model 1", value = FALSE)
                              
                      ),
                      
                      tabItem(tabName = "About", icon = icon("address-book"),
                              
                              
                              
                              box(
                                tags$div(class = "header", checked = NA,
                                         tags$h4("Guided by- Prof. A. Subash Babu"),
                                         tags$img(height = 200, 
                                                  width = 200,src="http://www.akgec.in/sites/default/files/styles/testimonial_70x70/public/a_subash_babu.jpg?itok=tQb4yWby", 
                                                  align = "left")
                                ),
                                actionButton(inputId='homepage', label="Homepage", 
                                             icon = icon("home"), 
                                             onclick ="window.open('http://www.me.iitb.ac.in/faculty/48/profile/', '_blank')"
                                )
                              ),
                              box(
                                tags$div(class = "header", checked = NA,
                                         tags$h4("Created by- Mr. Yash A. Baley"),
                                         tags$img(height = 200, 
                                                  width = 200,src="https://media.licdn.com/dms/image/C5103AQGiykSKccxcJQ/profile-displayphoto-shrink_200_200/0?e=1530270000&v=beta&t=oTtotYG1zm2yB_3YJ_mWa2jvHHRADEjmebbIJIxUTFQ", 
                                                  align = "left")
                                ),
                                actionButton(inputId='linkedin', label="LinkedIn", 
                                             icon = icon("linkedin"), 
                                             onclick ="window.open('https://www.linkedin.com/in/yash-a-baley-52301281/', '_blank')"
                                ),
                                tags$br(),
                                actionButton(inputId='Facebook', label="Facebook", 
                                             icon = icon("facebook"), 
                                             onclick ="window.open('https://www.facebook.com/YashABaley', '_blank')"
                                )
                              )
                              
                              
                              
                      )
                              )
                    )
                  
                  )
                  )
#End of Ui


server <- function(input,output,session){
  
  runjs('
        var el2 = document.querySelector(".skin-black");
        el2.className = "skin-black sidebar-mini";
        ')
  
  myData <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, header = TRUE)
    
    
    
    failure <- tbl[,(ncol(tbl)-23):ncol(tbl)]
    abc <- ifelse(failure>1,1,0)
    TotFailure <- rowSums(abc)
    tbl["Total.Failures"] <- TotFailure
    
    #Algo_names = data.frame(c("Linear Regression", "Logistic Regression", "KNN", "SVM", "Random Forest"))
    
    updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                      choices = names(tbl), selected = names(tbl)[35])
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(tbl), selected = names(tbl)[ncol(tbl)])
    
    updateSelectInput(session, inputId = 'algo_name', label = 'Please select an algorithm',
                      choices = c("Linear Regression", "Logistic Regression", "KNN", "SVM", "Random Forest"),
                      selected = "SVM")
    
    updateSelectInput(session, inputId = 'y_var', label = 'Please select month to predict failure',
                      choices = c("Total failures",1:24),
                      selected = 1)
    
    
    return(tbl)
    
  })
  
  output$tableOutput <- renderDT({
    myData()
  })
  
  output$DataSummary <- renderDT({
    summary(myData())
  })
  
  results_table <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, header = TRUE)
    
    df3 <- data.frame(matrix(c(tbl[,"Km.day"],tbl[,"number.of.years"],
                               tbl[,"City"],tbl[,"Village"],tbl[,"Mountain"],
                               tbl[,"Regular"],tbl[,"Occasional"],tbl[,"Scale.of.10"]),
                             nrow = nrow(tbl), ncol = 8))
    ptm <- proc.time()
    x = df3
    month = as.numeric(input$y_var)
    y <- tbl[,74+month]
    y = ifelse(y>0,1,0)
    y = as.factor(y)
    x = scale(x, center = TRUE, scale = TRUE)
    
    inTrain = createDataPartition(y, p = 0.8,list = FALSE)
    NCustomers_train = 0.8*nrow(x)
    Train = x[1:NCustomers_train,]
    Test = x[NCustomers_train:nrow(x),]
    Trainy = y[1:NCustomers_train]
    Testy = y[NCustomers_train:nrow(x)]
    
    dataframe <- data.frame(x,y)
    traindata <- data.frame(Train,Trainy)
    as.data.frame(traindata)
    
    if(input$algo_name == "KNN") {
      
      model = train(x = Train, y= Trainy, method = "knn" )
      pred = predict(model, Test)
      result_matrix = confusionMatrix(pred, Testy)
      time_elapsed = proc.time() - ptm
      return(result_matrix)
    }
    
    if(input$algo_name == "SVM") {
      model_svm = svm(Train, Trainy)
      pred_svm = predict(model_svm,Test)
      result_matrix = confusionMatrix(pred_svm, Testy)  
      return(result_matrix)
    }
    
    if(input$algo_name == "Random Forest") {
      traindata <- data.frame(Train,Trainy)
      testdata <- data.frame(Test,Testy)
      rf = randomForest(traindata$Trainy ~., ntree = 1000, data = traindata)
      pred_rf = predict(rf , Test)
      
      result_matrix = confusionMatrix(pred_rf, Testy)
      return(result_matrix)
    }
    
    if(input$algo_name == "Logistic Regression") {
      
      traindata <- data.frame(Train,Trainy)
      testdata <- data.frame(Test,Testy)
      lr = glm(traindata$Trainy ~., data = traindata,
               family = binomial(link="logit"))
      pred_lr = round(predict(lr , testdata,type = "response"))
      pred_lr[1] = 1
      pred_lr = factor(pred_lr)
      result_matrix = confusionMatrix(pred_lr, Testy)
      return(result_matrix)
    }
    
    
    else {
      statement <- "Please select an Algo"
      return(statement)
    }    
    
  })
  output$conf_matrix <- renderPrint({
    results_table()
  })  
  
  conf_matrix_csv <- reactive({
    ab <- results_table()
    cd <- as.data.frame.matrix(ab)
    bc <- data.frame(matrix(unlist(ab), nrow=12, byrow=T),stringsAsFactors=FALSE)
    #ab <- data.frame(cbind(t(results_table()$overall),t(results_table()$byClass)))
    return(cd)
  })
  
  
  
  output$dto <- renderDataTable(conf_matrix_csv(), extensions = 'Buttons', 
                                options = list(dom = 'Bfrtip',
                                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  
  myData2CPV <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2))
      return(NULL)
    
    tbl2 <- read.csv(inFile2$datapath, header = TRUE)
    tbl2 <- tbl2[,2:17]
    
    #failure <- tbl[,(ncol(tbl)-23):ncol(tbl)]
    #TotFailure <- rowSums(failure)
    #tbl["Total.Failures"] <- TotFailure
    
    
    
    updateSelectInput(session, inputId = 'feature1', label = 'Select 1st Feature',
                      choices = names(tbl2), selected = names(tbl2)[3])
    updateSelectInput(session, inputId = 'feature2', label = 'Select 2st Feature',
                      choices = names(tbl2), selected = names(tbl2)[2])
    
    updateSelectInput(session, inputId = 'A', label = 'Select 1st factor',
                      choices = names(myData()), selected = names(myData())[4])
    updateSelectInput(session, inputId = 'B', label = 'Select 2nd factor',
                      choices = names(myData()), selected = names(myData())[5])
    updateSelectInput(session, inputId = 'C', label = 'Select 3rd factor',
                      choices = names(myData()), selected = names(myData())[6])
    
    
    return(tbl2)
    
  })
  
  myData3<- reactive({
    CPV <- myData2CPV()[,ncol(myData2CPV())]
    feature1_rating <- myData2CPV()[,input$feature1]
    feature2_rating <- myData2CPV()[,input$feature2]
    
    first <- c(input$A,input$B,input$C)
    first <- as.matrix(first)
    a1 <- first[1,]
    b1 <- first[2,]
    c1 <- first[3,]
    mean_value = matrix(0,nrow = nrow(first),ncol = 4)
    for (i in 1:nrow(first)){
      mean_value[i,1] <- sum( myData()[,first[i,]]>0)
      mean_value[i,2] <- sum(myData()[,first[i,]]*CPV)/mean_value[i,1]
      mean_value[i,3] <- sum(myData()[,first[i,]]*feature1_rating)/mean_value[i,1]
      mean_value[i,4] <- sum(myData()[,first[i,]]*feature2_rating)/mean_value[i,1]
    }
    
    df1 <- data.frame(
      Rating_Type = factor(c(rep("Overall",nrow(first)),
                             rep(input$feature1,nrow(first)),
                             rep(input$feature2,nrow(first)))),
      time = factor(c(rep(first,nrow(first)))), 
      levels=c(first)
      
    )
    
    df1$Mean_rating <- c(mean_value[1,2:4],mean_value[2,2:4],mean_value[3,2:4])
    return(df1)
  })
  
  output$plot1 <- renderPlot({
    ggplot(data=myData3(), aes(x=time, y=Mean_rating, group=Rating_Type, 
                               shape=Rating_Type,color = Rating_Type),
           environment = environment() )+ 
      geom_line(size = 1.5) + 
      geom_point(size = 1.5) + 
      labs(x="", y = "Ratings") + 
      theme(axis.text=element_text(size=16,face = "bold"),
            axis.title=element_text(size=16,face="bold"),
            legend.text=element_text(size=16,face="bold"),
            legend.title=element_text(size=16,face="bold"),
            legend.key.size = unit(2,"line"))
    
    
  })
  
  output$rfplot <- renderPlot({
    tbl <- myData2CPV()[,2:ncol(myData2CPV())] 
    rf_out <- randomForest(CPV ~ ., data=tbl)
    
    
    # Sorts by variable importance and relevels factors to match ordering
    var_importance <- data_frame(variable=setdiff(colnames(tbl), "CPV"),
                                 importance=as.vector(importance(rf_out)))
    var_importance <- arrange(var_importance, desc(importance))
    var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
    
    p <- ggplot(var_importance, aes(x=variable, weight=importance, fill=variable))
    p <- p + geom_bar() + ggtitle("Variable Importance from Random Forest Fit")
    p <- p + xlab("Parameters") + ylab("Variable Importance (Contribution towards overall CPV)")
    p <- p + scale_fill_discrete(name="Parameter Names")
    p <- p + theme(axis.text.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18),
              legend.title=element_text(size=16),
              legend.text=element_text(size=12))
    p #+  geom_text(var_importance,aes(label=importance), position=position_dodge(width=0.9), vjust=-0.25)
    
    
  })
  
  
  output$corrplot <- renderPlot({
    corrplot(as.matrix(myData2CPV()), is.corr = FALSE, method="square", order="FPC", tl.srt = 90)
  })
  
  output$tableOutput2 <- renderDT({
    myData3()
  })
  
  output$tableOutputCPV <- renderDT({
    myData2CPV()
  })
  
  
  output$DataSummary2 <- renderTable({
    summary(myData2CPV())
  })
  
  output$regression <- renderPlot({
    ggplot(myData(),aes_string(x=input$xcol,y=input$ycol))  + 
      geom_smooth(method='lm',formula=y~x)+ggtitle('Linear Regression Curve')+
      theme(plot.title = element_text(color="black", size=16, face="bold.italic"))
    
  })
  
  observeEvent(input$hideshow, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("tableOutput")
  })
  
  observeEvent(input$hideshow3, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("tableOutputCPV")
  })
  
  observeEvent(input$hideshow2, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("tableOutput2")
  })
  
  
  output$linear1 <- renderPlot({
    #ggplot(myData(),aes_string(x=input$xcol,y=input$ycol))  + geom_smooth(method='lm',formula=y~x)+ggtitle('Lm Curve')+theme(plot.title = element_text(color="black", size=16, face="bold.italic"))
    #  plot(myData(),aes_string(x=input$xcol,y=input$ycol),ylim=c(0,20), xlim=c(0,20))
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$algo_name,"_Month_",input$y_var ,".csv", sep = "")
    },
    content = function(file) {
      write.csv(conf_matrix_csv(), file )
    }
  )
  
}
shinyApp(ui = ui, server = server)

