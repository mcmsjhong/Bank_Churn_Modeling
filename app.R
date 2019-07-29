# Bank churn modeling - July 29, 2019
# Jin Hong


library(shiny)
library(tidyverse)
library(DT)
library(knitr)
library(caret)


# Read Data
data <- read.csv("Churn_Modelling.csv", colClasses = c(NumOfProducts = "factor",
                                                       HasCrCard = "factor",
                                                       IsActiveMember = "factor",
                                                       Exited = "factor")) %>% select(-RowNumber, -CustomerId, -Surname)


ui <- fluidPage(
   withMathJax(),
  
   # Application title
   titlePanel("Bank Customer Churn Modeling"),
   
   # Sidebar
   sidebarLayout(
      sidebarPanel(
        
        selectizeInput("explanatory", "Explanatory variables (x)", choices = names(data)[1:10], multiple = FALSE),
        selectizeInput("response", "Response ariable (y)", choices = names(data)[11]),
        sliderInput("densitySlider", "Density Bandwidth adjustment", 0, 1, 0.7),
        hr(style="border-color:lightgrey"),
        
        strong("Select the PCs to plot"),
        br(),br(),
        selectizeInput("pca_x", "X axis", choices = c("PC1","PC2","PC3","PC4","PC5"), selected = "PC1"),
        selectizeInput("pca_y", "Y axis", choices = c("PC1","PC2","PC3","PC4","PC5"), selected = "PC2"),
        hr(style="border-color:lightgrey"),
        
        checkboxInput("modeling", strong("Ready for modeling?")),
        conditionalPanel(condition = "input.modeling",
                         
                         selectizeInput("predictors", "Predictors", choices = names(data)[1:10], 
                                        selected = "CreditScore", multiple = TRUE),    
                         radioButtons("model", "Select modeling method", choices = c("Classification" = "rpart",
                                                                                     "Random forest" = "rf",
                                                                                     "Bagged tree" = "treebag")),
                         sliderInput("cvFold", "Select # of fold CV", 2, 20, 10),
                         numericInput("cvRepeat", "Repeated k-fold CV", value = 1, min = 1, max = 5, step = 1),
                         br(),
                         
                         uiOutput("misclass"),
                         br(),
                         
                         actionButton("updateBtn", strong("Train Model", style = "color:blue"))
                         
                         ),
        hr(style="border-color:lightgrey"),
        
        h5("Data details"),
        a(href = "https://www.kaggle.com/barelydedicated/bank-customer-churn-modeling", "Link to Kaggle")
        
      ),
      
      
      mainPanel(
        tabsetPanel(
          
          # ============ Tab1 - Info ============  
          tabPanel(
            
            title = "Info",
            br(),
            
            h4(strong("What does this app do?")),
            h5("This Shiny application is intended to help to make ", strong("Churn Modeling for Bank customer.")),
            h5("Given a banking data, can you predict if bank customers will turnover next cycle? 
                This app will give you an opportunity to answer this question and make your own model. 
                Also, you can use this applet in many ways such as marketing or risk management."),
            h5("You will start from data exploration and unsupervised learning for predictor selection. 
                Then, let's train your model and test accuracy."),
            hr(),
            
            h4(strong("Description of data")),
            h5("This data is optained from ", strong("Kaggle - Competitions."), 
               "It is made for modeling purpose and has 10,000 observations and 11 variables for bank customers."),
            h5("Response variable is ", strong("Exited"), ", and there are 10 candidate predictors."),
            br(),
            
            strong("Variables description (#: numeric / F: factor)"),
            h5(strong("#"), "CreditScore"),
            h5(strong("F"), "Geography"),
            h5(strong("F"), "Gender"),
            h5(strong("#"), "Age"),
            h5(strong("#"), "Tenure"),
            h5(strong("#"), "Balance"),
            h5(strong("F"), "NumOfProducts - How many accounts, bank account affiliated products the person has"),
            h5(strong("F"), "HasCrCard"),
            h5(strong("F"), "IsActiveMember; Subjective, but for the concept"),
            h5(strong("#"), "EstimateSalary"),
            h5(strong("F"), "Exited - Response variable. Did they leave the bank after all?")
            
          ),
          
          
          # ============ Tab2 - Data exploration ============  
          tabPanel(
            
            title = "Data exploration",
            br(),
            
            h5("Please find summaries of each variable and decide final model variables before modeling process. 
                You can compare with our response variable within each variable, and this could be a basis on your decision."),
            h5("Click ", strong("Save plot"), "if needed."),
            hr(),
            
            strong("Numeric Summary"),
            br(),br(),
            verbatimTextOutput("summary"),
            br(),
            
            strong("Graphical Summary"), 
            br(),
            h5("Please use ", strong("Explanatory variable (x)"), "on sidebar panel to change variable. 
                For quantitative variable, density plot will be shown, and you can adjust bandwidth, too."),
            h5("If you click on density plot, you can get x and y coordinate info."),
            br(),
            downloadButton("downloadPlot", "Save plot"),
            br(),br(),
            plotOutput("variablePlot", click = "plot_click"),
            strong(verbatimTextOutput("plotInfo"))
            
          ),
          
          
          # ============ Tab3 - Unsupervised Learning ============
          tabPanel(
            
            title = "Unsupervised learning (PCA)",
            br(),
            
            h5("Principal Component Analysis (PCA) is obtaining a linear combination of the variables that counts for the largest amount of variability."),
            h5("Namely, find"), 
            h5("$$z_{i1} = \\phi_{11}x_{i1} + \\phi_{21}x_{i2} + ... + \\phi_{p1}x_{ip}$$"),
            h5("values so that the set of \\(z's\\) has the largest variance."),
            h5("This page allows you to inspect PCs and review several plots and summary.
                Only numeric data are included since PCA doesn't work with non-numeric data."),
            hr(),
            
            strong("Summary of PCs"),
            br(),br(),
            verbatimTextOutput("summaryPCA"),
            br(),
            
            h5("Choose two PCs on left selection to check biplot."),
            plotOutput("biPlot"),
            br(),
            
            h5("These two plots explain importance of PCs for the data. 
                You can choose PCs fewer than variables that explain most of variability."),
            plotOutput("PCAplot")
            
          ),
          
          
          # ============ Tab4 - Modeling ============ 
          tabPanel(
            
            title = "Modeling",
            br(),
            
            h5("In this section, you can build your own model based on exploratory data analysis(EDA) and try simple prediction case."),
            h5("Please click on ", strong("Ready for modeling"), "checkbox on left sidebar and fit your model setting such as predictors, modeling method, etc.
                After this, click on", strong("Train Model"), "button. You will see misclassification rate of your model and model fit summary on this page."),
            h5("If you want to try your model, click ", strong("Custom prediction"), "check box. The page will show only predictors selected on your model. 
                Click ", strong("Result"), "buttion after updating your own data and confirm model decision."),
            hr(),
            
            strong("Model Fit Summary"),
            br(),br(),
            verbatimTextOutput("modelFit"),
            br(),
            
            checkboxInput("customPrediction", strong("Custom prediction")),
            conditionalPanel(condition = "input.customPrediction",
                             
                             h5("Make your own prediction data with selected predictors below and confirm modified data."),
                             tableOutput("predictionData"),
                             actionButton("predictBtn", strong("Result", style = "color:blue")),
                             br(),br(),
                             
                             verbatimTextOutput("prediction"),
                             hr(),
                             
                             uiOutput("col_1"),
                             uiOutput("col_2"),
                             uiOutput("col_3"),
                             uiOutput("col_4"),
                             uiOutput("col_5"),
                             uiOutput("col_6"),
                             uiOutput("col_7"),
                             uiOutput("col_8"),
                             uiOutput("col_9"),
                             uiOutput("col_10")
                             
                             )
            
          ),
          
          
          # ============ Tab5 - Data subset ============
          tabPanel(
            
            title = "Data subset",
            br(),
            
            h5("Please filter raw data and download if needed."),
            hr(),
            
            downloadButton("downloadTable", "Download results"),
            br(), br(),
            
            strong(textOutput("filteredText"), style = "color:blue"),
            br(),
            dataTableOutput("DTtable")
            
          )
          
        ) #End - tabsetPanel 
      ) #End - mainPanel
    ) #End - sidebarLayout
) #End - UI.r



server <- function(input, output) {

  
  # ============ Tab2 - Data exploration ============ 
  
  # User select variable
  newData <- reactive({
    data %>% select(input$explanatory)
  })
  
  # Variable summary
  output$summary <- renderPrint({
    summary(newData()) 
  })
  
  
  buildPlot <- reactive({
    
    g <- ggplot(data, aes_string(x = input$explanatory)) + 
         labs(title = paste("Plot of", input$explanatory, "variable")) +
         scale_fill_discrete(labels = c("No","Yes"))
    
    if(input$explanatory %in% c("CreditScore","Age","Balance","EstimatedSalary")){# numeric variables
      
      gFinal <- g + geom_density(adjust = input$densitySlider, alpha = 0.5, aes(fill = Exited))
      
    } else {# categorical variables
      
      gFinal <- g + geom_bar(aes(fill = Exited), position = "dodge")
      
    }
    
    gFinal
    
  })
  
  # Plot download button
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("ExplanatoryVariable_", input$explanatory, ".png")
    },
    content = function(file) {
      ggsave(file, buildPlot())
    }
  )
  
  # ggplot
  output$variablePlot <- renderPlot({
    buildPlot()
  })
  
  # ggplot x info
  output$plotInfo <- renderText({
    
    # numeric variables(density plot)
    if(input$explanatory %in% c("CreditScore","Age","Balance","EstimatedSalary")){
      paste(input$explanatory,"=",input$plot_click$x %>% as.numeric() %>% round(0),
          "\nDensity =", input$plot_click$y)
    }
  })
  
    
  # ============ Tab3 - Unsupervised Learning ============ 
  
  # Numeric variables only for PCA
  numData <- reactive({
    data %>% select(CreditScore, Age, Tenure, Balance, EstimatedSalary)
  })
  
  # PCA
  PCs <- reactive({
    prcomp(numData(), scale. = TRUE)
  })
  
  # PCA Summary
  output$summaryPCA <- renderPrint({
    PCs()
  })
  
  # biplot
  output$biPlot <- renderPlot({
    
    par(mfrow = c(1,2))
    biplot(PCs(), xlabs = rep(".", nrow(data)),
           choices = c(unlist(input$pca_x) %>% substr(3,3) %>% as.numeric(),
                       unlist(input$pca_y) %>% substr(3,3) %>% as.numeric())
           )    
    
  })
  
  # Importance plot for PCA
  output$PCAplot <- renderPlot({
    
    par(mfrow = c(1,2))
    plot(PCs()$sdev^2/sum(PCs()$sdev^2), main = "Individual importance of PCs",
         xlab = "Principal Component", 
         ylab = "Proportion of Variance Explained", 
         type = 'b')
    
    plot(cumsum(PCs()$sdev^2/sum(PCs()$sdev^2)), main = "Cumulative importance of PCs",
         xlab = "Principal Component", 
         ylab = "Cum. Prop of Variance Explained", 
         ylim = c(0,1), type = 'b')
    
  })
  
  
  # ============ Tab4 - Modeling ============
  
  # Modeling data - predictors
  subData <- reactive({
    data %>% select(Exited, input$predictors)
  })
  
  # Sampling index
  trainNum <- sample(1:nrow(data), size = nrow(data)*0.8)
  testNum <- setdiff(1:nrow(data), trainNum)
  
  # Training dataset
  bankTrain <- reactive({
    subData()[trainNum, ]
  })
  
  # Test dataset
  bankTest <- reactive({
    subData()[testNum, ]
  })
  
  
  
  # Fit model - Cross Validation setting
  ctrl <- reactive({
    trainControl(method = "repeatedcv", number = input$cvFold, repeats = input$cvRepeat)
  })
  
  # Fit model - Training
  Fit <- reactive({
    
    set.seed(111)
      
    if(input$model == "rf") {
      
      train(Exited ~., data = bankTrain(),
            method = input$model,
            preProcess = c("center", "scale"),
            trControl = ctrl(),
            # Random Forest tuning
            ntree = 10,
            tuneGrid = data.frame(mtry = 1:(ncol(bankTrain())-1)) # only predictors = -1(Exited)
            )
      
    } else {
      
      train(Exited ~., data = bankTrain(),
            method = input$model,
            preProcess = c("center", "scale"),
            trControl = ctrl())
      
    }
    
  })
    
  # Fit model - Summary
  output$modelFit <- renderPrint({
    
    input$updateBtn
    isolate({ 
      
      if(input$updateBtn == 0){return("Update your model setting and click [Train model] button")}
      Fit()
      
    })
  })
  
  # Fit model - Misclassification rate (sidebar)
  output$misclass <- renderUI({
    
    input$updateBtn
    isolate({ 
      
      if(input$updateBtn == 0){return(strong("... Misclassification rate of your model is ...", style = "color:blue"))}
      
      Pred <- predict(Fit(), newdata = bankTest())
      CM <- confusionMatrix(Pred, bankTest()$Exited)
      misclass <- 1 - sum(diag(CM$table)) / sum(CM$table)
      strong(paste("... Misclassification rate of your model:", misclass, "(Test dataset)"), style = "color:blue")
      
    })
  })
  
  
  
  ## Custom prediction - Show only selected predictors on model
  output$col_1 <- renderUI({
    
    if(any(colnames(subData()) == "CreditScore")) {
      sliderInput("creditScore","CreditScore", min = 300, max = 850, value = 600)
    } else NULL
    
  })
  
  output$col_2 <- renderUI({
    
    if(any(colnames(subData()) == "Geography")) {
      selectInput("geography","Geography", choices = levels(data$Geography))
    } else NULL
    
  })
  
  output$col_3 <- renderUI({
    
    if(any(colnames(subData()) == "Gender")) {
      selectInput("gender","Gender", choices = levels(data$Gender))    
    } else NULL
    
  })
  
  output$col_4 <- renderUI({
    
    if(any(colnames(subData()) == "Age")) {
      sliderInput("age"," Age", min = 10, max = 100, value = 30)    
    } else NULL
    
  })
  
  output$col_5 <- renderUI({
    
    if(any(colnames(subData()) == "Tenure")) {
      sliderInput("tenure","Tenure", min = 0, max = 10, value = 5)    
    } else NULL
    
  })
  
  output$col_6 <- renderUI({
    
    if(any(colnames(subData()) == "Balance")) {
      sliderInput("balance","Balance", min = 0, max = 1000000, value = 10000)    
    } else NULL
    
  })
  
  output$col_7 <- renderUI({
    
    if(any(colnames(subData()) == "NumOfProducts")) {
      selectInput("numOfProducts","NumOfProducts", choices = levels(data$NumOfProducts))    
    } else NULL
    
  })
  
  output$col_8 <- renderUI({
    
    if(any(colnames(subData()) == "HasCrCard")) {
      selectInput("hasCrCard","HasCrCard", choices = levels(data$HasCrCard))    
    } else NULL
    
  })
  
  output$col_9 <- renderUI({
    
    if(any(colnames(subData()) == "IsActiveMember")) {
      selectInput("isActiveMember","IsActiveMember", choices = levels(data$IsActiveMember))    
    } else NULL
    
  })
  
  output$col_10 <- renderUI({
    
    if(any(colnames(subData()) == "EstimatedSalary")) {
      sliderInput("estimatedSalary","EstimatedSalary", min = 0, max = 1000000, value = 50000)    
    } else NULL
    
  })
  
  ## Custom prediction - Update dataset from above *Input by user (1 observation)
  predictionData <- reactive({
    
    test <- subData()[1,]
    
    if(any(colnames(test) == "CreditScore")) {
      test["CreditScore"] <- input$creditScore
    }
    
    if(any(colnames(test) == "Geography")) {
      test["Geography"] <- input$geography
    }
    
    if(any(colnames(test) == "Gender")) {
      test["Gender"] <- input$gender
    }
    
    if(any(colnames(test) == "Age")) {
      test["Age"] <- input$age
    }
    
    if(any(colnames(test) == "Tenure")) {
      test["Tenure"] <- input$tenure
    }
    
    if(any(colnames(test) == "Balance")) {
      test["Balance"] <- input$balance
    }
    
    if(any(colnames(test) == "NumOfProducts")) {
      test["NumOfProducts"] <- input$numOfProducts
    }
    
    if(any(colnames(test) == "HasCrCard")) {
      test["HasCrCard"] <- input$hasCrCard
    }
    
    if(any(colnames(test) == "IsActiveMember")) {
      test["IsActiveMember"] <- input$isActiveMember
    }
    
    if(any(colnames(test) == "EstimatedSalary")) {
      test["EstimatedSalary"] <- input$estimatedSalary
    }
    
    test
    
  })
  
  ## Custom prediction - Data table
  output$predictionData <- renderTable({
    
    predictionData() %>% tbl_df()
    
  })
  
  ## Custom prediction - Prediction result
  output$prediction <- renderPrint({
    
    input$predictBtn
    isolate({
      
    if(input$predictBtn == 0){return("Update prediction data and click [Result] button")}
      
    if(predict(Fit(), newdata = predictionData()) == 0){
      "Not Exit"
    } else {"Exit"}
      
    })
  })
  
  
  # ============ Tab5 - Data subset ============ 
  
  # Raw data
  output$DTtable <- renderDataTable({
    datatable(data, filter = "top", rownames = FALSE)
     })
  
  # How many obs are selected?
  output$filteredText <- renderText({
    numObs <- nrow(data[input[["DTtable_rows_all"]], ])
    paste("You've selected", numObs, "observations")
  })
  
  # Download filtered data
  output$downloadTable <- downloadHandler(
    filename = function() {
      "BankChurnData_filtered.csv"
    }, 
    content = function(file) {
      write.csv(data[input[["DTtable_rows_all"]], ], file, row.names = FALSE)
    }
  )

} #End - Server.r



# Run the application 
shinyApp(ui = ui, server = server)
