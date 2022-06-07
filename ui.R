library(shiny)
library(shinythemes)
library(dplyr)
library(shinydashboard)
library(plotly)
library(DT)
library(ggplot2)
library(shinyjs)

nutrition<-read.csv("nutrition.csv")

food_name_list<-nutrition[[1]]
names(food_name_list)<- nutrition$name

# Define UI 
shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  navbarPage("Healthy Planet",
             tabPanel("Introduction",
                      mainPanel(
                        img(src='1.png',width=1300,height=640),
                        img(src='2.png',width=1300,height=640),
                        img(src='3.png',width=1300,height=640),
                        img(src='4.png',width=1300,height=360),
                        img(src='5.png',width=1300,height=640),
                      )
              ),
             tabPanel("Calorie Calculator",
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
                          actionButton("submit","Submit")
                        ),
                        
                        
                        mainPanel(
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
                          
                          h4("Recommended macronutrients:"),
                          plotOutput("macros"),
                          
                          plotOutput("progress")
                        )
                      )
             ),
             tabPanel("Nutrition Calculator",
                      sidebarLayout(
                        sidebarPanel(
                          selectizeInput(
                            inputId = 'food_id',
                            label = 'Search Food',
                            choices = food_name_list,
                            selected= NULL,
                            multiple = FALSE,
                            
                            options = list(
                              placeholder = 'e.g. orange',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          ),
                          
                          numericInput(inputId = 'no_of_serving',
                                       label = 'Servings(per 100g)',
                                       min=1,max=10000,1),
                          
                          actionButton("add","Add Food"),
                          actionButton("delete","Delete Food")
                          
                        ),
                        
                        mainPanel(
                          fluidRow(
                            box(title = "Total Calories",
                                solidHeader = T,
                                witdh = 5,
                                valueBoxOutput("calories"),
                                background_color = "navy"),
                            
                            box(title = "Nutrition Table",
                                solidHeader = T,
                                width = 10,
                                collapsible = T,
                                collapsed = F,
                                tags$p(textOutput("serving", inline = T)),
                                div(DT::DTOutput("nutrient_table"), style = "font-size: 70%;")),
                            
                            box(title = "Macronutrients", solidHeader = T,
                                width = 5, collapsible = T,
                                plotlyOutput("macro_plot")),
                            
                            box(title = "Vitamins", solidHeader=T,
                                width = 5, collapsible = T,
                                plotlyOutput("vitamin_plot"))
                          )
                        )
                      )
             ),
  ),
))
