library(shiny)
library(tidyverse)
library(parsnip)
library(ranger)

# Cargar modelo entrenado
modelo_final_fit <- readRDS("C:/Users/Migue/OneDrive/Documentos/UC3M/TFG/modelo_final.rds")  # Guarda previamente tu modelo con saveRDS
DatosMiguel <- DatosMiguel_sin_respuesta

# Obtener nombres de variables de entrada
feature_names <- names(DatosMiguel)

ui <- fluidPage(
  titlePanel("Predicción con Random Forest"),
  sidebarLayout(
    sidebarPanel(
      lapply(feature_names, function(var) {
        numericInput(inputId = var, label = var, value = DatosMiguel[[var]][1])
      }),
      actionButton("predict", "Predecir")
    ),
    mainPanel(
      verbatimTextOutput("prediction")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$predict, {
    req(modelo_final_fit)
    
    # Crear nuevo individuo con valores ingresados
    new_data <- tibble::tibble(!!!setNames(lapply(feature_names, function(var) input[[var]]), feature_names))
    
    # Obtener predicción
    prediction <- predict(modelo_final_fit, new_data, type = "prob") %>%
      as.data.frame()
    colnames(prediction) <- c("No_Profesional", "Profesional")
    
    output$prediction <- renderText({
      paste("Probabilidad de ser profesional:", round(prediction$Profesional * 100, 2), "%")
    })
  })
}

shinyApp(ui, server)
