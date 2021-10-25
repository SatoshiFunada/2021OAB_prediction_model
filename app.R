library(shiny)
library(tidyverse)
library(Hmisc)
library(foreign)
library(ROCR)
library(pROC)
library(rms)
library(haven)
library(mice)
library(foreign)
library(dplyr)
library(ggplot2)
library(GGally)
library(glmnet)
library(rsq)

data_male <- read.csv("data/LR.L2_male.csv")
data_male <- subset(data_male, s0 != 0)
data_female <- read.csv("data/LR.L1_female.csv")
data_female <- subset(data_female, s0 != 0)

ui <- navbarPage(HTML("<span style='font-size:30px'>Overactive bladder (OAB) Prediction Model after five years</span>"),
                 
                 tabPanel(HTML("<span style='font-size:30px'>Male</span>"),
                          fluidRow(
                              column(3,
                                     style = "background-color:#e9f5fb",
                                     sliderInput(inputId='age', label='Age', value = 30,min = 30, max = 75, step = NA,width = NULL),
                                     radioButtons(inputId='smoke', label='Smoking', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='HT', label='Hypertention', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='HL', label='Hyperlipidemia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='DM', label='Diabetes', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Insomnia', label='Insomnia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Prostate_disease', label='Prostate disease', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     sliderInput(inputId='HbA1c', label='HbA1c', value = 4.0,min = 4.0, max = 12.0, step = 0.1,width = NULL),
                                     sliderInput(inputId='eGFR', label='eGFR', value = 15,min = 15.0, max = 150.0, step = 1,width = NULL),
                                     sliderInput(inputId='BNP', label='BNP', value = 5.0,min = 5.0, max = 100, step = 1,width = NULL),
                                     sliderInput(inputId='PSA', label='PSA', value = 0,min = 0, max = 20.0, step = 0.1,width = NULL)
                                     
                              ),
                              column(4,
                                     style = "background-color:#f4f5f6",
                                     h3("OAB symptom score"), 
                                     radioButtons(inputId='OABSS1_1', label='How many times do you typically urinate from waking in the morning until sleeping at night?', 
                                                  choices = list('less than 7 times'=0,'8 to 14 times'=1, 'more than 15 times'=2), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS2_1', label='How many times do you typically wake up to urinate from sleeping at night until waking in the morning?', 
                                                  choices = list('0 times'=0,'1 time'=1, '2 time'=2, 'more than 3 times'=3), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS3_1', label='How often do you have a sudden desire to urinate, which is difficult to defer?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS4_1', label='How often do you leak urine because you cannot defer the sudden desire to urinate?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL)
                              ),
                              column(5,
                                     mainPanel(
                                         h3("Predicted risk"),
                                         verbatimTextOutput(outputId = "Pred_male")
                                     )
                              )
                          )
                 ),
                 tabPanel(HTML("<span style='font-size:30px'>Female</span>"),
                          fluidRow(
                              
                              column(3,
                                     style = "background-color:#fdedf0",
                                     sliderInput(inputId='age_female', label='Age', value = 30,min = 30, max = 75, step = NA,width = NULL),
                                     sliderInput(inputId='bmi_female', label='BMI', value = 15,min = 15, max = 40, step = NA,width = NULL),
                                     radioButtons(inputId='Delivery_female', label='Delivery', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='alcohol_female', label='Alcohol', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='MI_female', label='Myocardial infarction', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Cancer_female', label='Cancer', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='SAS_female', label='Obstructive sleep apnea', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Insomnia_female', label='Insomnia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL)
                              ),
                              column(4,
                                     style = "background-color:#f4f5f6",
                                     h3("OAB symptom score"),                       
                                     radioButtons(inputId='OABSS1_1_female', label='How many times do you typically urinate from waking in the morning until sleeping at night?', 
                                                  choices = list('less than 7 times'=0,'8 to 14 times'=1, 'more than 15 times'=2), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS2_1_female', label='How many times do you typically wake up to urinate from sleeping at night until waking in the morning?', 
                                                  choices = list('0 times'=0,'1 time'=1, '2 time'=2, 'more than 3 times'=3), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS3_1_female', label='How often do you have a sudden desire to urinate, which is difficult to defer?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS4_1_female', label='How often do you leak urine because you cannot defer the sudden desire to urinate?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                              ),
                              column(5,
                                     mainPanel(
                                         h3("Predicted risk"), 
                                         verbatimTextOutput(outputId = "Pred_female")
                                     )
                              )
                          )
                 )
)



server <- function (input,output) {
    
    LR.L2_male <- reactive({
        df.tmp <- cbind(data_male, "input" = c(1, as.numeric(input$age),
                                               as.numeric(input$smoke),
                                               as.numeric(input$HT), as.numeric(input$HL), as.numeric(input$DM), 
                                               as.numeric(input$Insomnia), as.numeric(input$Prostate_disease),
                                               as.numeric(input$OABSS1_1), as.numeric(input$OABSS2_1),
                                               as.numeric(input$OABSS3_1), as.numeric(input$OABSS4_1),
                                               as.numeric(input$HbA1c), as.numeric(input$eGFR),
                                               as.numeric(input$BNP), as.numeric(input$PSA)))
        df.tmp$cal <- df.tmp$s0 * df.tmp$input
        return(df.tmp)
    })
    
    output$Pred_male <- renderText({
        xb <- sum(LR.L2_male()$cal)
        OAB_total <- sum(as.numeric(input$OABSS1_1), as.numeric(input$OABSS2_1), 
                         as.numeric(input$OABSS3_1), as.numeric(input$OABSS4_1)
        )
        pred_male <- round(1/(1+exp(-xb)), digits = 3)
        if (input$OABSS3_1>=2&OAB_total>=3) {                 
            paste0("You are diagnosed as OAB now.\nPlease go to see a doctor.")                   
        } else {
            paste0(pred_male*100, '%')                
        }
    })
    
    LR.L1_female  <- reactive({
        df.tmp <- cbind(data_female, "input" = c(1, as.numeric(input$age_female), as.numeric(input$bmi_female), as.numeric(input$Delivery_female),
                                                 as.numeric(input$alcohol_female), as.numeric(input$MI_female), as.numeric(input$Cancer_female), as.numeric(input$SAS_female), 
                                                 as.numeric(input$Insomnia_female), as.numeric(input$OABSS1_1_female), 
                                                 as.numeric(input$OABSS2_1_female), as.numeric(input$OABSS3_1_female), 
                                                 as.numeric(input$OABSS4_1_female)))
        df.tmp$cal <- df.tmp$s0 * df.tmp$input
        return(df.tmp)
    })
    
    output$Pred_female <- renderText({
        xb <- sum(LR.L1_female()$cal)
        OAB_total_female <- sum(as.numeric(input$OABSS1_1_female), as.numeric(input$OABSS2_1_female), 
                                as.numeric(input$OABSS3_1_female), as.numeric(input$OABSS4_1_female)
        )
        pred_female <- round(1/(1+exp(-xb)), digits = 3)
        if (input$OABSS3_1_female>=2&OAB_total_female>=3) {                 
            paste0("You are diagnosed as OAB now.\nPlease go to see a doctor.")                   
        } else {
            paste0(pred_female*100, '%')                
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
