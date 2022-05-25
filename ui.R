library(shiny)
library(shinythemes)

# Define UI 
ui<-fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Healthy Planet",
    tabPanel("Introduction",),
    tabPanel("Calorie Calculator",
        titlePanel("Calorie Calculator"),
        sidebarLayout(
            sidebarPanel( 
                  sliderInput(inputId="age",label="Age:",value=21,min=1,max=100),
                  radioButtons(inputId = "gender",label="Gender: ",c("Male","Female")),
                  numericInput(inputId = "height",label="Height (m): ",value=1.50,min=1,max=300),
                  numericInput(inputId = "weight",label="Weight (kg): ",value=40, min=1,max=200),
                  selectInput(inputId = "activity",label = "Activity: ",c("Sedentary: little or no exercise"="0",
                                                                          "Light: exercise 1-3 times/week"="1",
                                                                          "Moderate: exercise 4-5 times/ week"="2",
                                                                          "Active: exercise 6-7 times/week"="3",
                                                                          "Very Active: very intense exercise (twice per day)"="4")),
                  submitButton("Submit")
            ),
    
    
            mainPanel(
                  #tags$head(tags$style('h4 {color:blue;}')),
                  h4("Your Body Mass Index (BMI) is"),
                  verbatimTextOutput("bmi"),
      
                  h4("Your ideal weight (in kg) is"),
                  verbatimTextOutput("goalweight"),
                  verbatimTextOutput("bmiStatus"),
                  
                  h4("Your Basal Metabolic Rate (BMR) is"),
                  verbatimTextOutput("bmr"),
      
                  h4("Your Total Daily Energy Expenditure (TDEE) is"),
                  verbatimTextOutput("tdee"),
      
                  h4("Your daily calorie should be"),
                  verbatimTextOutput("dailyCalorie"),
      
                  h4("Recommended macros:"),
                  plotOutput("macros"),
      
                  plotOutput("progress")
            )
        )
    ),
    tabPanel("Nutrition Calculator",
             
    ),
  ),
)