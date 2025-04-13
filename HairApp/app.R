library(shiny)
library(shinydashboard)
library(tidymodels)
library(tidyverse)
library(xgboost)




model <- readRDS("hair_mode.RDS")

model$pre$mold$predictors



ui <- dashboardPage(
  dashboardHeader(title = "Hair Fall Prediction"),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .body {
                   background-color: #76d7c4;
            }
        .box {
                      border-radius: 15px;
                      border: 2px solid #9b59b6;
                      color: #c0392b;
                      text-align: center;
             }
        .custom-header{
                           text-align: center;
                           color: #2c3e50;
                           font-size: 32px;
                           padding: 20px;
                    }
        .box-header {
                         background-color: #28a745;
                         color: black;
                         text-align: center;
                         padding: 10px;
                   }
        .content-wrapper{
                            background-color: #f0f8ff;
        }"))
    ),
    

    box(title = "Baldness Prediction",
        solidHeader = TRUE,
        width = 6,
        status = "primary",
        valueBoxOutput("hair_prediction")),
    box(title = "Medical Conditions",
        width = 5, selectInput("v_Medical.Conditions", label = "Select Medical Condition below",
                               choices = c("Eczema", "Dermatosis","Ringworm","Psoriasis","Alopecia Areata","Scalp Infection","Seborrheic Dermatitis","Dermatitis","Thyroid Problems","Androgenetic Alopecia"))),
    box(width = 5, selectInput("v_Genetics", label = "Select Genetic History below",
                               choices = c("Yes", "No"))),
    box(width = 5, selectInput("v_Nutritional.Deficiencies", label = "Select Nutritional Deficiency below",
                               choices = c("Magnesium deficiency", "Protein deficiency","Biotin Deficiency","Iron deficiency","Selenium deficiency","Omega-3 fatty acids","Zinc Deficiency","Vitamin A Deficiency","Vitamin D Deficiency","Vitamin E deficiency"))),
    box(width = 3, selectInput("v_Smoking", label = "Are you smoking?",
                               choices = c("No", "Yes"))),
    box(width = 3, selectInput("v_Poor.Hair.Care.Habits", label = "Do you take Care of your Hair?",
                               choices = c("No", "Yes"))),
    
    fluidRow(
      box(sliderInput("v_age", label = "Age",
                      min = 0, max = 100, value = 45)),
     
    ),
    
  ))

server <- function(input, output) { 
  
  input_df <-       reactive(tibble(
    "Genetics" = input$v_Genetics,
    "Medical.Conditions" = input$v_Medical.Conditions,
    "Nutritional.Deficiencies" = input$v_Nutritional.Deficiencies,
    "Poor.Hair.Care.Habits" = input$v_Poor.Hair.Care.Habits,
    "Smoking" = input$v_Smoking,
    "Age" = input$v_age,
  ))
  
  
  output$hair_prediction <- renderValueBox({
    prediction <- predict(model,input_df())
    
    
    prediction_prob <- predict(model, input_df(), type = "prob") %>% 
      gather() %>% 
      arrange(desc(value)) %>% 
      slice_max(value, n = 1) %>% 
      select(value)
    
    prediction_color <- case_when(prediction$.pred_class == "No" ~ "blue",
                                  TRUE ~ "red")
    
    
    valueBox(
      value = paste0(round(100 * prediction_prob$value, 0), "%"),
      subtitle = paste0("Baldnes Risk: ", prediction$.pred_class),
      color = prediction_color,
      icon = icon("snowflake"),
    )
  })  
}
shinyApp(ui, server)

