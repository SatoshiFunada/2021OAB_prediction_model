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

data_male1 <- read.csv("data/LR.L1_male.csv")
data_male1 <- subset(data_male1, s0 != 0)
data_male2 <- read.csv("data/LR.L2_male.csv")
data_male2 <- subset(data_male2, s0 != 0)
data_female1 <- read.csv("data/LR.L1_female.csv")
data_female1 <- subset(data_female1, s0 != 0)
data_female2 <- read.csv("data/LR.L2_female.csv")
data_female2 <- subset(data_female2, s0 != 0)

ui <- navbarPage(HTML("<span style='font-size:30px'>Overactive bladder (OAB) Prediction Model after five years</span>"),
                 tabPanel(HTML("<span style='font-size:30px'>Male1</span>"),
                          fluidRow(
                              column(3,
                                     style = "background-color:#e9f5fb",
                                     sliderInput(inputId='age_1', label='Age', value = 30,min = 30, max = 75, step = NA,width = NULL),
                                     radioButtons(inputId='smoke_1', label='Smoking', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='DM_1', label='Diabetes', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Prostate_disease_1', label='Prostate disease', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                      
                              ),
                              column(4,
                                     style = "background-color:#f4f5f6",
                                     h3("OAB symptom score"), 
                                     radioButtons(inputId='OABSS1_1_1', label='How many times do you typically urinate from waking in the morning until sleeping at night?', 
                                                  choices = list('less than 7 times'=0,'8 to 14 times'=1, 'more than 15 times'=2), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS2_1_1', label='How many times do you typically wake up to urinate from sleeping at night until waking in the morning?', 
                                                  choices = list('0 times'=0,'1 time'=1, '2 time'=2, 'more than 3 times'=3), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS3_1_1', label='How often do you have a sudden desire to urinate, which is difficult to defer?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS4_1_1', label='How often do you leak urine because you cannot defer the sudden desire to urinate?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL)
                              ),
                              column(5,
                                     mainPanel(
                                         h3("Predicted risk"),
                                         verbatimTextOutput(outputId = "Pred_male_1")
                                     )
                              )
                          )
                 ),
                 tabPanel(HTML("<span style='font-size:30px'>Male2</span>"),
                          fluidRow(
                              column(3,
                                     style = "background-color:#e9f5fb",
                                     sliderInput(inputId='age_2', label='Age', value = 30,min = 30, max = 75, step = NA,width = NULL),
                                     radioButtons(inputId='smoke_2', label='Smoking', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='HT_2', label='Hypertention', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='HL_2', label='Hyperlipidemia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='DM_2', label='Diabetes', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Insomnia_2', label='Insomnia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Prostate_disease_2', label='Prostate disease', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     sliderInput(inputId='HbA1c_2', label='HbA1c', value = 4.0,min = 4.0, max = 12.0, step = 0.1,width = NULL),
                                     sliderInput(inputId='eGFR_2', label='eGFR', value = 15,min = 15.0, max = 150.0, step = 1,width = NULL),
                                     sliderInput(inputId='BNP_2', label='BNP', value = 5.0,min = 5.0, max = 100, step = 1,width = NULL),
                                     sliderInput(inputId='PSA_2', label='PSA', value = 0,min = 0, max = 20.0, step = 0.1,width = NULL)
                                     
                              ),
                              column(4,
                                     style = "background-color:#f4f5f6",
                                     h3("OAB symptom score"), 
                                     radioButtons(inputId='OABSS1_1_2', label='How many times do you typically urinate from waking in the morning until sleeping at night?', 
                                                  choices = list('less than 7 times'=0,'8 to 14 times'=1, 'more than 15 times'=2), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS2_1_2', label='How many times do you typically wake up to urinate from sleeping at night until waking in the morning?', 
                                                  choices = list('0 times'=0,'1 time'=1, '2 time'=2, 'more than 3 times'=3), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS3_1_2', label='How often do you have a sudden desire to urinate, which is difficult to defer?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS4_1_2', label='How often do you leak urine because you cannot defer the sudden desire to urinate?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL)
                              ),
                              column(5,
                                     mainPanel(
                                         h3("Predicted risk"),
                                         verbatimTextOutput(outputId = "Pred_male_2")
                                     )
                              )
                          )
                 ),
                 tabPanel(HTML("<span style='font-size:30px'>Female1</span>"),
                          fluidRow(
                              
                              column(3,
                                     style = "background-color:#fdedf0",
                                     sliderInput(inputId='age_3', label='Age', value = 30,min = 30, max = 75, step = NA,width = NULL),
                                     sliderInput(inputId='bmi_3', label='BMI', value = 15,min = 15, max = 40, step = NA,width = NULL),
                                     radioButtons(inputId='Delivery_3', label='Delivery', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='alcohol_3', label='Alcohol', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='MI_3', label='Myocardial infarction', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='CANCER_3', label='Cancer', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='SAS_3', label='Obstructive sleep apnea', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Insomnia_3', label='Insomnia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL)
                              ),
                              column(4,
                                     style = "background-color:#f4f5f6",
                                     h3("OAB symptom score"),                       
                                     radioButtons(inputId='OABSS1_1_3', label='How many times do you typically urinate from waking in the morning until sleeping at night?', 
                                                  choices = list('less than 7 times'=0,'8 to 14 times'=1, 'more than 15 times'=2), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS2_1_3', label='How many times do you typically wake up to urinate from sleeping at night until waking in the morning?', 
                                                  choices = list('0 times'=0,'1 time'=1, '2 time'=2, 'more than 3 times'=3), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS3_1_3', label='How often do you have a sudden desire to urinate, which is difficult to defer?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS4_1_3', label='How often do you leak urine because you cannot defer the sudden desire to urinate?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                              ),
                              column(5,
                                     mainPanel(
                                         h3("Predicted risk"), 
                                         verbatimTextOutput(outputId = "Pred_female_3")
                                     )
                              )
                          )
                 ),
                 tabPanel(HTML("<span style='font-size:30px'>Female2</span>"),
                          fluidRow(
                              
                              column(3,
                                     style = "background-color:#fdedf0",
                                     sliderInput(inputId='age_4', label='Age', value = 30,min = 30, max = 75, step = NA,width = NULL),
                                     sliderInput(inputId='bmi_4', label='BMI', value = 15,min = 15, max = 40, step = NA,width = NULL),
                                     radioButtons(inputId='Delivery_4', label='Delivery', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='menopause_4', label='Menopause', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='smoke_4', label='Smoking', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='alcohol_4', label='Alcohol', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='walking_4', label='Walking', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='HT_4', label='Hypertention', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='HL_4', label='Hyperlipidemia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='DM_4', label='Diabetes', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='MI_4', label='Myocardial infarction', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='STROKE_4', label='Stroke', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Renal_disease_4', label='Kidney disease', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='CANCER_4', label='Cancer', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Depression_4', label='Depression', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='Insomnia_4', label='Insomnia', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='SAS_4', label='Obstructive sleep apnea', choices = list('yes'=1,'no'=0), selected = NULL, inline = FALSE,width = NULL),
                                     sliderInput(inputId='HbA1c_4', label='HbA1c', value = 4.0,min = 4.0, max = 12.0, step = 0.1,width = NULL),
                                     sliderInput(inputId='eGFR_4', label='eGFR', value = 15,min = 15.0, max = 150.0, step = 1,width = NULL),
                                     sliderInput(inputId='BNP_4', label='BNP', value = 5.0,min = 5.0, max = 100, step = 1,width = NULL)
                                     
                              ),
                              column(4,
                                     style = "background-color:#f4f5f6",
                                     h3("OAB symptom score"),                       
                                     radioButtons(inputId='OABSS1_1_4', label='How many times do you typically urinate from waking in the morning until sleeping at night?', 
                                                  choices = list('less than 7 times'=0,'8 to 14 times'=1, 'more than 15 times'=2), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS2_1_4', label='How many times do you typically wake up to urinate from sleeping at night until waking in the morning?', 
                                                  choices = list('0 times'=0,'1 time'=1, '2 time'=2, 'more than 3 times'=3), selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS3_1_4', label='How often do you have a sudden desire to urinate, which is difficult to defer?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                                     radioButtons(inputId='OABSS4_1_4', label='How often do you leak urine because you cannot defer the sudden desire to urinate?', 
                                                  choices = list('Not at all'=0,'Less than once a week'=1, 'Once a week or more'=2, 
                                                                 'About once a day'=3, '2 to 4 times a day'=4, '5 times a day or more'=5), 
                                                  selected = NULL, inline = FALSE,width = NULL),
                              ),
                              column(5,
                                     mainPanel(
                                         h3("Predicted risk"), 
                                         verbatimTextOutput(outputId = "Pred_female_4")
                                     )
                              )
                          )
                 )
)



server <- function (input,output) {
    
    LR.L1_male <- reactive({
        df.tmp_1 <- cbind(data_male1, "input" = c(1, 
                                                as.numeric(input$age_1),
                                                as.numeric(input$smoke_1),
                                                as.numeric(input$DM_1), 
                                                as.numeric(input$Prostate_disease_1),
                                                as.numeric(input$OABSS1_1_1), 
                                                as.numeric(input$OABSS2_1_1),
                                                as.numeric(input$OABSS3_1_1), 
                                                as.numeric(input$OABSS4_1_1)))
        df.tmp_1$cal <- df.tmp_1$s0 * df.tmp_1$input
        return(df.tmp_1)
    })
    
    output$Pred_male_1 <- renderText({
        xb_1 <- sum(LR.L1_male()$cal)
        OAB_total_1 <- sum(as.numeric(input$OABSS1_1_1), as.numeric(input$OABSS2_1_1), 
                         as.numeric(input$OABSS3_1_1), as.numeric(input$OABSS4_1_1)
        )
        pred_male_1 <- round(1/(1+exp(-xb_1)), digits = 3)
        if (input$OABSS3_1_1>=2&OAB_total_1>=3) {                 
            paste0("You are diagnosed as OAB now.\nPlease go to see a doctor.")                   
        } else {
            paste0(pred_male_1*100, '%')                
        }
    })
    
    LR.L2_male <- reactive({
        df.tmp_2 <- cbind(data_male2, "input" = c(1, 
                                                as.numeric(input$age_2),
                                                as.numeric(input$smoke_2),
                                                as.numeric(input$HT_2), 
                                                as.numeric(input$HL_2), 
                                                as.numeric(input$DM_2), 
                                                as.numeric(input$Insomnia_2), 
                                                as.numeric(input$Prostate_disease_2),
                                                as.numeric(input$OABSS1_1_2), 
                                                as.numeric(input$OABSS2_1_2),
                                                as.numeric(input$OABSS3_1_2), 
                                                as.numeric(input$OABSS4_1_2),
                                                as.numeric(input$HbA1c_2), 
                                                as.numeric(input$eGFR_2),
                                                as.numeric(input$BNP_2), 
                                                as.numeric(input$PSA_2)))
        df.tmp_2$cal <- df.tmp_2$s0 * df.tmp_2$input
        return(df.tmp_2)
    })
    
    output$Pred_male_2 <- renderText({
        xb_2 <- sum(LR.L2_male()$cal)
        OAB_total_2 <- sum(as.numeric(input$OABSS1_1_2), as.numeric(input$OABSS2_1_2), 
                         as.numeric(input$OABSS3_1_2), as.numeric(input$OABSS4_1_2)
        )
        pred_male_2 <- round(1/(1+exp(-xb_2)), digits = 3)
        if (input$OABSS3_1_2>=2&OAB_total_2>=3) {                 
            paste0("You are diagnosed as OAB now.\nPlease go to see a doctor.")                   
        } else {
            paste0(pred_male_2*100, '%')                
        }
    })
    
    LR.L1_female  <- reactive({
        df.tmp_3 <- cbind(data_female1, "input" = c(1, 
                                                  as.numeric(input$age_3), 
                                                  as.numeric(input$bmi_3), 
                                                  as.numeric(input$Delivery_3),
                                                  as.numeric(input$alcohol_3), 
                                                  as.numeric(input$MI_3), 
                                                  as.numeric(input$CANCER_3), 
                                                  as.numeric(input$SAS_3), 
                                                  as.numeric(input$Insomnia_3), 
                                                  as.numeric(input$OABSS1_1_3), 
                                                  as.numeric(input$OABSS2_1_3), 
                                                  as.numeric(input$OABSS3_1_3), 
                                                  as.numeric(input$OABSS4_1_3)))
        df.tmp_3$cal <- df.tmp_3$s0 * df.tmp_3$input
        return(df.tmp_3)
    })
    
    output$Pred_female_3 <- renderText({
        xb_3 <- sum(LR.L1_female()$cal)
        OAB_total_3 <- sum(as.numeric(input$OABSS1_1_3), as.numeric(input$OABSS2_1_3), 
                                as.numeric(input$OABSS3_1_3), as.numeric(input$OABSS4_1_3)
        )
        pred_female_3 <- round(1/(1+exp(-xb_3)), digits = 3)
        if (input$OABSS3_1_3>=2&OAB_total_3>=3) {                 
            paste0("You are diagnosed as OAB now.\nPlease go to see a doctor.")                   
        } else {
            paste0(pred_female_3*100, '%')                
        }
    })
    
    LR.L2_female  <- reactive({
        df.tmp_4 <- cbind(data_female2, "input" = c(1, 
                                                  as.numeric(input$age_4), 
                                                  as.numeric(input$bmi_4), 
                                                  as.numeric(input$Delivery_4),
                                                  as.numeric(input$menopause_4), 
                                                  as.numeric(input$smoke_4), 
                                                  as.numeric(input$alcohol_4), 
                                                  as.numeric(input$walking_4),
                                                  as.numeric(input$HT_4),
                                                  as.numeric(input$HL_4),
                                                  as.numeric(input$DM_4),
                                                  as.numeric(input$MI_4), 
                                                  as.numeric(input$STROKE_4),
                                                  as.numeric(input$Renal_disease_4),
                                                  as.numeric(input$CANCER_4), 
                                                  as.numeric(input$Depression_4),
                                                  as.numeric(input$Insomnia_4),
                                                  as.numeric(input$SAS_4), 
                                                  as.numeric(input$OABSS1_1_4), 
                                                  as.numeric(input$OABSS2_1_4), 
                                                  as.numeric(input$OABSS3_1_4), 
                                                  as.numeric(input$OABSS4_1_4),
                                                  as.numeric(input$HbA1c_4), 
                                                  as.numeric(input$eGFR_4),
                                                  as.numeric(input$BNP_4)))
        df.tmp_4$cal <- df.tmp_4$s0 * df.tmp_4$input
        return(df.tmp_4)
    })
    
    output$Pred_female_4 <- renderText({
        xb_4 <- sum(LR.L2_female()$cal)
        OAB_total_4 <- sum(as.numeric(input$OABSS1_1_4), as.numeric(input$OABSS2_1_4), 
                                as.numeric(input$OABSS3_1_4), as.numeric(input$OABSS4_1_4)
        )
        Pred_female_4 <- round(1/(1+exp(-xb_4)), digits = 3)
        if (input$OABSS3_1_4>=2&OAB_total_4>=3) {                 
            paste0("You are diagnosed as OAB now.\nPlease go to see a doctor.")                   
        } else {
            paste0(Pred_female_4*100, '%')                
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
