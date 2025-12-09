# app.R

library(shiny)
library(survival)
library(dplyr)
library(readr)
library(DT)

ui <- fluidPage(
  titlePanel("Cox vs Censored Linear Regression (Practice App)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "data_file", 
        "Upload CSV file",
        accept = c(".csv")
      ),
      
      checkboxInput("use_example", "Use example lung data (for Cox)", value = FALSE),
      hr(),
      
      radioButtons(
        "model_type",
        "Model type",
        choices = c("Cox regression" = "cox",
                    "Censored linear regression" = "cens_lin"),
        inline = FALSE
      ),
      
      uiOutput("var_select_ui"),
      
      actionButton("fit_model", "Fit model")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data preview", DTOutput("data_preview")),
        tabPanel("Model summary", verbatimTextOutput("model_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data ----
  raw_data <- reactive({
    # Option 1: example data for Cox
    if (isTRUE(input$use_example)) {
      
      # clean it a bit
      df <- df %>%
        mutate(
          status = ifelse(status == 2, 1, 0)  # 1 = event, 0 = censored per survival standard
        )
      return(df)
    }
    # browser()
    # Option 2: user-uploaded CSV
    req(input$data_file)
    ext <- tools::file_ext(input$data_file$name)
    validate(
      need(ext == "csv", "Please upload a .csv file")
    )
    readr::read_csv(input$data_file$datapath, show_col_types = FALSE)
  })
  
  # Update variable selectors when data changes ----
  observeEvent(raw_data(), {
    df <- raw_data()
    cols <- names(df)
    
    # For Cox
    updateSelectInput(session, "time_var", choices = cols)
    updateSelectInput(session, "status_var", choices = cols)
    
    # For censored linear
    updateSelectInput(session, "response_var", choices = cols)
    updateSelectInput(session, "censor_var", choices = cols)
    
    # Covariates (common)
    updateSelectInput(session, "covariates", choices = cols)
  })
  
  # Dynamic UI depending on model type ----
  output$var_select_ui <- renderUI({
    req(raw_data())
    df <- raw_data()
    cols <- names(df)
    
    if (input$model_type == "cox") {
      tagList(
        h4("Cox regression variables"),
        helpText("Status should be coded 1 = event, 0 = censored."),
        selectInput("time_var", "Time variable", choices = cols),
        selectInput("status_var", "Status variable (0/1)", choices = cols),
        selectInput("covariates", "Covariates", choices = cols, multiple = TRUE)
      )
    } else {
      tagList(
        h4("Censored linear regression variables"),
        helpText("Censor indicator: 1 = observed, 0 = right-censored (for example)."),
        selectInput("response_var", "Response variable", choices = cols),
        selectInput("censor_var", "Censor indicator", choices = cols),
        selectInput("covariates", "Covariates", choices = cols, multiple = TRUE)
      )
    }
  })
  
  # Data preview ----
  output$data_preview <- renderDT({
    req(raw_data())
    datatable(head(raw_data(), 20), options = list(pageLength = 20))
  })
  
  # Fit model ----
  fitted_model <- eventReactive(input$fit_model, {
    df <- raw_data()
    req(df, input$covariates)
    validate(
      need(length(input$covariates) > 0, "Please select at least one covariate.")
    )
    
    if (input$model_type == "cox") {
      # Cox model: Surv(time, status) ~ covariates
      req(input$time_var, input$status_var)
      
      time_vec   <- df[[input$time_var]]
      status_vec <- df[[input$status_var]]
      covars     <- df[, input$covariates, drop = FALSE]
      
      # Basic checks
      validate(
        need(is.numeric(time_vec), "Time variable must be numeric."),
        need(all(status_vec %in% c(0, 1, NA), na.rm = TRUE),
             "Status variable should be coded 0/1 (with possible NAs).")
      )
      
      surv_obj <- Surv(time_vec, status_vec)
      model_df <- data.frame(surv_obj = surv_obj, covars)
      
      survival::coxph(surv_obj ~ ., data = model_df)
      
    } else {
      # Censored linear regression via survreg with Gaussian dist
      req(input$response_var, input$censor_var)
      
      y_vec    <- df[[input$response_var]]
      censor   <- df[[input$censor_var]]
      covars   <- df[, input$covariates, drop = FALSE]
      
      validate(
        need(is.numeric(y_vec), "Response variable must be numeric."),
        need(all(censor %in% c(0, 1, NA), na.rm = TRUE),
             "Censor indicator should be coded 0/1 (with possible NAs).")
      )
      
      # Here we treat 1 = observed, 0 = right-censored
      surv_y   <- Surv(time = y_vec, event = censor, type = "right")
      model_df <- data.frame(surv_y = surv_y, covars)
      
      survival::survreg(surv_y ~ ., data = model_df, dist = "gaussian")
    }
  })
  
  # Show model summary ----
  output$model_summary <- renderPrint({
    req(fitted_model())
    summary(fitted_model())
  })
}

shinyApp(ui, server)
