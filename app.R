#Install Packages
install.packages("shiny")
install.packages("plotly")
install.packages("DT")

#Load Packages
library(shiny)
library(plotly)
library(DT)  # for data table display


# UI Definition
ui <- fluidPage(
  titlePanel(
    HTML("<h1> Simple Linear Regression Analysis and Power Analysis</h1><br/><p>R-Shiny app Developed by <a href='https://www.linkedin.com/in/johnlemueldalisay/'>John Lemuel M. Dalisay</a><br>Co-CiPhil:Co-Creatubg Environmental Citizen Science Capacity in the Philippines<br>ITP-WS2 Week 2 Lecture - Prof. Thomas Neyens</p>")
  ),
  sidebarLayout(
    sidebarPanel(
      # Select file
      fileInput("file1", "Choose CSV File",
                accept = c(".csv", "text/comma-separated-values,text/plain")),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "),
                   selected = ","),
      uiOutput("varSelectUI"),
      # Option to choose the analysis type
      radioButtons("analysisType", "Select Analysis Type:",
                   choices = list("Simple Linear Regression" = "slr",
                                  "Power Analysis for Minimum Sample Size" = "powerSampleSize",
                                  "Power Analysis for Minimum Detectable Effect" = "powerMDE")),
      
      # Inputs for Power Analysis
      conditionalPanel(
        condition = "input.analysisType != 'slr'",
        numericInput("seed", "Random Seed:", value = 12345),
        numericInput("nsim", "Number of Simulations:", value = 1000)
      ),
      conditionalPanel(
        condition = "input.analysisType == 'powerSampleSize'",
        numericInput("ssfrom", "Beginning (Sample Size):", value = 190),
        numericInput("ssto", "End (Sample Size):", value = 220),
        numericInput("ssby", "Increment (Sample Size):", value = 1),
        numericInput("betaValue", "Beta:", value = 0.234),
        numericInput("alphaValueSampleSize", "Alpha:", value = 0.05),
        numericInput("targetPowerSampleSize", "Power:", value = 0.90)
      ),
      conditionalPanel(
        condition = "input.analysisType == 'powerMDE'",
        numericInput("sampleSize", "Sample Size:", value = 200),
        numericInput("betafrom", "Beginning (Beta):", value = 0.23),
        numericInput("betato", "End (Beta):", value = 0.235),
        numericInput("betaby", "Increment (Beta):", value = 0.001),
        numericInput("alphaValueMDE", "Alpha:", value = 0.05),
        numericInput("targetPowerMDE", "Power:", value = 0.90)
      ),
      actionButton("goButton", "Run Analysis")
    ),
    mainPanel(
      # Conditional Outputs for each Analysis Type
      conditionalPanel(
        condition = "input.analysisType == 'slr'",
        h3("Data Preview"),
        DT::DTOutput("dataPreview"),
        h3("Regression Plot"),
        plotlyOutput("regPlot"),
        h3("Residuals vs Fitted Plot"),
        plotOutput("residualPlot"),
        textOutput("residualsInterpretation"),
        h3("Normal Q-Q Plot"),
        plotOutput("qqPlot"),
        textOutput("qqPlotInterpretation"),
        h3("Scale-Location Plot"),
        plotOutput("scaleLocPlot"),
        textOutput("scaleLocInterpretation"),
        h3("Residuals vs Leverage Plot"),
        plotOutput("leveragePlot"),
        textOutput("leverageInterpretation"),
        h3("Model Diagnostics"),
        verbatimTextOutput("modelDiagnostics"),
        textOutput("modelInterpretation"),
        tags$footer(HTML("<div style='text-align: center; margin-top: 20px;'><p><small>Disclaimer: This application is provided for informational purposes only. It is not intended for professional use. The developer makes no representations or warranties of any kind, express or implied, about the completeness, accuracy, reliability, suitability, or availability with respect to the application or the information, products, services, or related graphics contained on the application for any purpose. Any reliance you place on such information is therefore strictly at your own risk.</small></p></div>"))
      ),
      conditionalPanel(
        condition = "input.analysisType == 'powerSampleSize'",
        # Outputs for Power Analysis for Minimum Sample Size
        h3("Data Preview"),
        DT::DTOutput("dataPreviewSampleSize"),
        h3("Power Analysis for Minimum Sample Size"),
        h4("Power Analysis for Minimum Sample Size - Plot"),
        plotOutput("powerSampleSizePlot"),
        h4("Power Analysis for Minimum Sample Size - Interpretation"),
        textOutput("powerSampleSizeText"),
        DT::dataTableOutput("powerTable"),  # Table output for power values
        tags$footer(HTML("<div style='text-align: center; margin-top: 20px;'><p><small>Disclaimer: This application is provided for informational purposes only. It is not intended for professional use. The developer makes no representations or warranties of any kind, express or implied, about the completeness, accuracy, reliability, suitability, or availability with respect to the application or the information, products, services, or related graphics contained on the application for any purpose. Any reliance you place on such information is therefore strictly at your own risk.</small></p></div>"))
      ),
      conditionalPanel(
        condition = "input.analysisType == 'powerMDE'",
        # Outputs for Power Analysis for Minimum Detectable Effect
        h3("Data Preview"),
        DT::DTOutput("dataPreviewMDE"),
        h3("Power Analysis for Minimum Detectable Effect"),
        h4("Power Analysis for Minimum Detectable Effect - Plot"),
        plotOutput("powerMDEPlot"),
        h4("Power Analysis for Minimum Detectable Effect - Interpretation"),
        textOutput("powerMDEText"),
        DT::dataTableOutput("powerTableMDE"),  # Table output for power values
        tags$footer(HTML("<div style='text-align: center; margin-top: 20px;'><p><small>Disclaimer: This application is provided for informational purposes only. It is not intended for professional use. The developer makes no representations or warranties of any kind, express or implied, about the completeness, accuracy, reliability, suitability, or availability with respect to the application or the information, products, services, or related graphics contained on the application for any purpose. Any reliance you place on such information is therefore strictly at your own risk.</small></p></div>"))
      )
    )
  )
)



# Server Logic
server <- function(input, output, session) {
  # Reactive expression for reading the data
  dataInput <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) return(NULL)
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    return(df)
  })
  
  # Render the data preview table
  output$dataPreview <- DT::renderDT({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Dynamic UI for variable selection
  output$varSelectUI <- renderUI({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    fluidRow(
      column(6, selectInput("responseVar", "Response Variable", names(df))),
      column(6, selectInput("predictorVar", "Predictor Variable", names(df), selected = names(df)[2]))
    )
  })
  

  # Logic for Simple Linear Regression
  observeEvent(input$analysisType, {
    if (input$analysisType == "slr") {
        # Reactive expression for running linear regression
        modelResults <- eventReactive(input$goButton, {
          req(dataInput())
          df <- dataInput()
          responseVar <- input$responseVar
          predictorVar <- input$predictorVar
          lmModel <- lm(reformulate(predictorVar, responseVar), data = df)
          return(lmModel)
        })
        
        # Regression Plot
        output$regPlot <- renderPlotly({
          model <- modelResults()
          if (is.null(model)) return()
          df <- dataInput()
          plot_ly(df, x = ~get(input$predictorVar), y = ~get(input$responseVar), type = 'scatter', mode = 'markers') %>%
            add_lines(y = fitted(model), name = 'Fitted Line')
        })
        
        # Residuals vs Fitted Plot
        output$residualPlot <- renderPlot({
          model <- modelResults()
          if (is.null(model)) return()
          ggplot(model, aes_string(x = ".fitted", y = ".resid")) +
            geom_point() +
            geom_hline(yintercept = 0, linetype = "dashed") +
            labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")
        })
        
        output$residualsInterpretation <- renderText({
          "Interpretation of Residuals vs Fitted Plot:\n\nThis plot should show no clear pattern. If there's a distinct pattern (like a curve), it indicates non-linearity in the data. Randomly scattered dots are ideal and suggest that the model's assumptions about homoscedasticity and linearity are met."
        })
        
        # Normal Q-Q Plot
        output$qqPlot <- renderPlot({
          model <- modelResults()
          if (is.null(model)) return()
          ggplot(model, aes_string(sample = ".stdresid")) +
            stat_qq() +
            stat_qq_line() +
            labs(title = "Normal Q-Q Plot")
        })
        
        output$qqPlotInterpretation <- renderText({
          "Interpretation of Normal Q-Q Plot:\n\nThis plot shows if residuals are normally distributed. Points should fall along the straight line. Significant deviations from the line indicate deviations from normality, which affects certain statistical tests."
        })
        
        # Scale-Location Plot
        output$scaleLocPlot <- renderPlot({
          model <- modelResults()
          if (is.null(model)) return()
          plot(model, which = 3)
        })
        
        output$scaleLocInterpretation <- renderText({
          "Interpretation of Scale-Location Plot: This plot checks for homoscedasticity. Points should be randomly dispersed around a horizontal line, indicating constant variance of residuals. A funnel shape indicates heteroscedasticity."
        })
        
        # Residuals vs Leverage Plot
        output$leveragePlot <- renderPlot({
          model <- modelResults()
          if (is.null(model)) return()
          plot(model, which = 5)
        })
        
        output$leverageInterpretation <- renderText({
          "Interpretation of Residuals vs Leverage Plot: This plot helps identify influential observations. Points with high leverage and large residuals can disproportionately influence the model fit."
        })
        
        # Model Summary and Interpretation
        output$modelDiagnostics <- renderPrint({
          model <- modelResults()
          if (is.null(model)) return()
          summary(model)
        })
        
        output$modelInterpretation <- renderText({
          "Model Interpretation:\n\nThe summary provides coefficients (estimates), their standard errors, t-values, and p-values. Significant p-values (typically < 0.05) suggest a significant relationship between predictor and response. The R-squared value indicates the proportion of variance explained by the model. Closer to 1 means a better fit. However, a high R-squared value is not a guarantee of a good model, especially if it's not accompanied by significant predictors or if the model violates regression assumptions."
        })
    }
  })

  
  # Render the data preview table
  output$dataPreviewSampleSize <- DT::renderDT({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  # Logic for Power Analysis for Minimum Sample Size
  observeEvent(input$goButton, {
    if (input$analysisType == "powerSampleSize") {
      # Start Progress Indicator
      withProgress(message = 'Analysis running...', value = 0, {
        for (k in 1:100) {
          setProgress(value = k/100)
          Sys.sleep(0.1)  # Simulates long-running analysis
        }
      # Power Analysis for Minimum Sample Size
      set.seed(input$seed)
      
      # Directly use the input values
      ss <- seq(input$ssfrom, input$ssto, by = input$ssby)
      print(input$ssfrom)
      nsim <- input$nsim
      beta <- input$betaValue
        
      df <- dataInput()  # Assuming dataInput is your reactive data reading function
      if (is.null(df)) return(NULL)
        
      # Replace 'x' and 'y' with appropriate columns from your data
      x <- df[[input$predictorVar]]
      y <- df[[input$responseVar]]
      fit <- lm(y ~ x, data = df)
      int <- coef(fit)[1]
        
      
      pvalue <- data.frame()
      betahat <- data.frame()
      
      #Run the simulation
      for(j in 1:length(ss)){
        for(i in 1:nsim){
          Xs <- rnorm(n=ss[j],mean=mean(x),sd=sd(x))
          Xs
          beta
          eps <- rnorm(n=ss[j], mean = 0, sd = summary(fit)$sigma)
          Ys <- int + beta*Xs +eps
          Ys
          fits <- lm(Ys ~ Xs)
          summary(fits)
          pvalue[i,j] <- summary(fits)$coefficients[2,4]
          betahat[i,j] <- summary(fits)$coefficients[2,1]
        }
      }
      # Plotting the power curve
      power <- colSums(pvalue < input$alphaValueSampleSize) / nsim
      names(power) <- ss
      # Store power values in a reactive value for rendering in table
      powerValues <- reactive({ data.frame(SampleSize = names(power), Power = power) })
      

      output$powerSampleSizePlot <- renderPlot({
        plot(ss, power, xlab="Sample Size", ylab="Power")
          abline(h = input$targetPowerSampleSize, lwd = 2, col = "red")
      })
      
      output$powerSampleSizeText <- renderText({
        paste("Power analysis for sample sizes from", input$ssfrom, "to", input$ssto,"\n\n",
              "Interpretation:\n",
              "This plot shows the estimated power of the test for different sample sizes. ",
              "Power is the probability that the test correctly rejects the null hypothesis when it is false. ",
              "The red line indicates the target power level ", input$targetpowerSampleSize, " (commonly set at 0.8 or 0.9). ",
              "Sample sizes above which the curve crosses this line are generally considered sufficient ",
              "to achieve the desired power. Increasing the sample size above this point results in diminishing returns.")
      })
      
      # Render power values in a table
      output$powerTable <- renderDataTable({
        powerValues()
      })
      })
    }
    # ... (Code for other analysis types)
  })
  
  
  # Render the data preview table
  output$dataPreviewMDE <- DT::renderDT({
    df <- dataInput()
    if (is.null(df)) return(NULL)
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Logic for Power Analysis for Minimum Detectable Effect
  observeEvent(input$goButton, {
    if (input$analysisType == "powerMDE") {
      # Start Progress Indicator
      withProgress(message = 'Analysis running...', value = 0, {
        for (k in 1:100) {
          setProgress(value = k/100)
          Sys.sleep(0.1)  # Simulates long-running analysis
        }
      # Power Analysis for Minimum Detectable Effect
      set.seed(input$seed)
      ss <- input$sampleSize
      nsim <- input$nsim
      beta_seq <- seq(input$betafrom, input$betato, by = input$betaby)
      
      df <- dataInput()
      if (is.null(df)) return(NULL)
      
      # Assuming 'x' and 'y' are columns in df
      x <- df[[input$predictorVar]]
      y <- df[[input$responseVar]]
      fit <- lm(y ~ x, data = df)
      int <- coef(fit)[1]
      
      pvalue1 <- matrix(nrow = nsim, ncol = length(beta_seq))
      betahat1 <- data.frame()
      
      #Run the simulation
      for(j in 1:length(beta)){
        for(i in 1:nsim){
          Xs <- rnorm(n=ss,mean=mean(x),sd=sd(x))
          Xs
          beta1<-beta[j]
          eps <- rnorm(n=ss, mean = 0, sd = summary(fit)$sigma)
          Ys <- int + beta1*Xs +eps
          Ys
          fits <- lm(Ys ~ Xs)
          summary(fits)
          pvalue1[i,j] <- summary(fits)$coefficients[2,4]
          betahat1[i,j] <- summary(fits)$coefficients[2,1]
        }
      }
      # Plotting the power curve
      power <- colSums(pvalue1 < input$alphaValueSampleSize) / nsim
      names(power) <- beta_seq
      # Store power values in a reactive value for rendering in table
      powerValues <- reactive({ data.frame(Beta = names(power), Power = power) })
      
      output$powerMDEPlot <- renderPlot({
        plot(beta_seq, power,xlab="Beta", ylab="Power")
        abline(h = input$targetPowerMDE, lwd = 2, col = "red")
      })
      
      output$powerMDEText <- renderText({
        paste("Power analysis for Minimum Detectable Effect - Beta from", input$betafrom, "to", input$betato, " by = ", input$betaby, "\n\n",
              "Interpretation:\n",
              "This plot shows how the power of the test changes with different effect sizes (Beta values). ",
              "The power is the probability of detecting an effect of a given size, with larger effects generally being easier to detect. ",
              "The red line represents the target power level ", input$targetPowerMDE, " (commonly set at 0.8 or 0.9). ",
              "Values of Beta to the right of the point where the curve crosses this line ",
              "indicate effect sizes that are likely to be detectable with the chosen sample size and at the desired power level. ",
              "This analysis helps in understanding the smallest effect size that is likely to be detectable with the study design.")
      })
      
      # Render power values in a table
      output$powerTableMDE <- renderDataTable({
        powerValues()
      })
      })
    }
    # ... (Code for other analysis types)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

