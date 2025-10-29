# app.R
library(shiny)

# ----- load model(s) from files in this folder -----
models <- list()
for (nm in c("lprice1","lprice2","lprice3")) {
  r <- paste0(nm, ".rds")
  if (file.exists(r)) models[[nm]] <- readRDS(r)
}
stopifnot(length(models) > 0)
# pick lprice3 if present, else first available
model_obj <- if ("lprice3" %in% names(models)) models[["lprice3"]] else models[[1]]
xlev <- model_obj$xlevels

# ----- helper -----
get_price_prediction_report <- function(model,
                                        aspiration, carbody, carheight, carManufacturer,
                                        curbweight, enginetype, enginesize, fuelsystem,
                                        peakrpm, wheelbase, level = 0.95,
                                        smearing_correct = TRUE, currency = "$", quiet = FALSE) {
  new_car <- data.frame(
    aspiration, carbody, carheight, carManufacturer, curbweight,
    enginetype, enginesize, fuelsystem, peakrpm, wheelbase,
    stringsAsFactors = FALSE
  )
  if (!is.null(model$xlevels)) {
    for (nm in names(model$xlevels)) {
      new_car[[nm]] <- factor(new_car[[nm]], levels = model$xlevels[[nm]])
    }
    bad <- vapply(names(model$xlevels), function(nm) any(is.na(new_car[[nm]])), logical(1))
    if (any(bad)) stop("Unseen factor level(s): ", paste(names(model$xlevels)[bad], collapse = ", "))
  }
  pred_log <- predict(model, newdata = new_car, interval = "prediction", level = level)
  smear <- if (smearing_correct) mean(exp(residuals(model))) else 1
  fit_price <- exp(pred_log[1,"fit"]) * smear
  lwr_price <- exp(pred_log[1,"lwr"]); upr_price <- exp(pred_log[1,"upr"])
  invisible(data.frame(predicted_price = fit_price, lower = lwr_price, upper = upr_price, level = level))
}

# ----- UI (4-column grid) -----
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .grid-inputs {
      display: grid;
      grid-template-columns: repeat(4, minmax(220px, 1fr));
      gap: 12px 16px;
      align-items: start;
      margin-bottom: 1rem;
    }
    @media (max-width: 1200px) {.grid-inputs { grid-template-columns: repeat(2, minmax(220px, 1fr)); } }
    .pred-wrap { max-width: 900px; }
  "))),
  titlePanel("Price prediction (log-price model: lprice3)"),
  div(class = "grid-inputs",
      selectInput("aspiration","Engine aspiration (aspiration)", choices = xlev$aspiration),
      selectInput("carbody","Car body type (carbody)", choices = xlev$carbody),
      selectInput("carManufacturer", "Car manufacturer (carManufacturer)", choices = xlev$carManufacturer),
      selectInput("enginetype","Engine type (enginetype)", choices = xlev$enginetype),
      selectInput("fuelsystem","Fuel system (fuelsystem)", choices = xlev$fuelsystem),
      numericInput("carheight","Car height (carheight) [inches]", value = 52, step = 0.1),
      numericInput("curbweight","Curb weight (curbweight) [lb]", value = 2500, step = 1),
      numericInput("enginesize","Engine size (enginesize) [cubic inches]", value = 120, step = 1),
      numericInput("peakrpm","Peak RPM [rpm]", value = 5000, step = 50),
      numericInput("wheelbase","Wheel base [inches]", value = 97, step = 0.1)
  ),
  div(class = "pred-wrap",
      tags$h4("Prediction"),
      verbatimTextOutput("pred_text"),
      tags$h4("Details"),
      tableOutput("pred_tbl"),
      # tags$p(tags$a(href = "../MSc_AS-SMM634-Group4-Project.html", "← Back to report"))
      tags$p(
        tags$a(
          href = "../MSc_AS-SMM634-Group4-Project.html",
          target = "_top",
          rel = "external noopener",
          "← Back to report"
        )
      )
  )
)

server <- function(input, output, session) {
  make_pred <- reactive({
    get_price_prediction_report(
      model = model_obj,
      aspiration = input$aspiration, carbody = input$carbody, carheight = input$carheight,
      carManufacturer = input$carManufacturer, curbweight = input$curbweight,
      enginetype = input$enginetype, enginesize = input$enginesize,
      fuelsystem = input$fuelsystem, peakrpm = input$peakrpm, wheelbase = input$wheelbase,
      quiet = TRUE
    )
  })
  fmt_money <- function(x, currency = "$", digits = 0) {
    paste0(currency, formatC(x, format = "f", digits = digits, big.mark = ","))
  }
  output$pred_text <- renderPrint({
    p <- make_pred()
    cat(sprintf(
      "Predicted price: %s\n%d%% prediction interval: [%s, %s]",
      fmt_money(p$predicted_price),
      round(p$level * 100),
      fmt_money(p$lower),
      fmt_money(p$upper)
    ))
  })
  output$pred_tbl <- renderTable(make_pred(), digits = 0)
}

shinyApp(ui, server)
