library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(shinyjs)

#Function to calculate BMI
bmi <- function(weight, height){
  format(round(weight/(height*height),2))
}

#Function to calculate BMR
bmr <- function(age, gender, weight, height){
  if(gender=="Male"){
    round(66 + (13.7*weight) + (5*(height*100)) - (6.8*age),2)
  }else if(gender=="Female"){
    round(655 + (9.6*weight) + (1.8*(height*100)) - (4.7*age),2)
  }
}

#Function to calculate TDEE
tdee <- function(activity,bmr){
  if(activity=="0"){
    round(bmr*1.2,2)
  }else if(activity=="1"){
    round(bmr*1.375,2)
  }else if(activity=="2"){
    round(bmr*1.55,2)
  }else if(activity=="3"){
    round(bmr*1.725,2)
  }else if(activity=="4"){
    round(bmr*1.9,2)
  }
}

#Function to calculate range of ideal weight
minIdealWeight <- function(height){
  round(18.5*(height*height), digits=2)
}

maxIdealWeight <- function(height){
  round(24.9*(height*height), digits=2)
}

idealWeight <- function(minIdealWeight, maxIdealWeight){
  print(minIdealWeight,"to", maxIdealWeight)
}

##Function to display BMI status 
bmiStatus <- function(bmi){
  
  if(bmi<18.5){
    print("You are underweight. You need to gain weight.")
  }else if(bmi>18.5 && bmi<24.9){
    print("Your weight are normal. Keep on maintaining!")
  }else if(bmi>24.9){
    print("You are overweight. You have to lose weight.")
  }
}

#Function to display daily calories needed
dailyCalorie <- function(tdee, bmi){
  if(bmi<18.5){   #to gain weight
    tdee + 1000
  }else if(bmi>18.5 && bmi<24.9){
    tdee
  }else if(bmi>24.9){
    tdee-1000
  }
}

#Function to calculate proportion of macronutrients needed
macros <- function(bmi, dailyCalorie){
  if(bmi<18.5){
    carbsCalorie1 <- (55/100) * dailyCalorie
    proteinCalorie1 <- (30/100) * dailyCalorie
    fatCalorie1 <- (15/100) * dailyCalorie
    
    df <- data.frame(value = c(carbsCalorie1, proteinCalorie1, fatCalorie1), 
                     group = c("Carbohydrates", "Protein", "Fats"))
    
    ggplot(df, aes(x = "", y = value, fill = group)) +
      geom_col(color = "black") +
      geom_label(aes(label = value),
                 color = "white",
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      coord_polar(theta = "y")
    
  }else if(bmi>18.5 && bmi<24.9){
    carbsCalorie2 <- (50/100) * dailyCalorie
    proteinCalorie2 <- (30/100) * dailyCalorie
    fatCalorie2 <- (20/100) * dailyCalorie
    
    df <- data.frame(value = c(carbsCalorie2, proteinCalorie2, fatCalorie2), 
                     group = c("Carbohydrates", "Protein", "Fats"))
    
    ggplot(df, aes(x = "", y = value, fill = group)) +
      geom_col(color = "black") +
      geom_label(aes(label = value),
                 color = "white",
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      coord_polar(theta = "y")
    
  }else if(bmi>24.9){
    carbsCalorie3 <- (40/100) * dailyCalorie
    proteinCalorie3 <- (30/100) * dailyCalorie
    fatCalorie3 <- (30/100) * dailyCalorie
    
    df <- data.frame(value = c(carbsCalorie3, proteinCalorie3, fatCalorie3), 
                     group = c("Carbohydrates", "Protein", "Fats"))
    
    
    ggplot(df, aes(x = "", y = value, fill = group)) +
      geom_col(color = "black") +
      geom_label(aes(label = value),
                 color = "white",
                 position = position_stack(vjust = 0.5),
                 show.legend = FALSE) +
      coord_polar(theta = "y")
  }
}

#Function to calculate the progress to achieve ideal weight
progress <- function(weight, minIdealWeight, maxIdealWeight){
  if(weight < minIdealWeight){
    progress1 <- abs(((weight - minIdealWeight)/minIdealWeight)*100)
    percentage1 <- 100-progress1
    
    data <- data.frame(category=c("Achieved", "Not Achieved"), count=c(percentage1, progress1))
    
    #compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # compute percentage
    data$percentage <- round(data$fraction*100, digits=2)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$percentage, "%")
    
    # colour for the chart
    colour<-c("#006400", "#D3D3D3")
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() + ggtitle("Progress to achieve ideal weight:")+
      scale_fill_manual(values=colour) + # chart colour
      coord_polar(theta="y") +
      xlim(c(-4, 4)) +
      theme_void() +
      theme(legend.position = "none")+
      theme(plot.title = element_text(size=28))+
      annotate(geom='text', x=-4, y=0.25, size=13, colour="dark green", label=paste0(data$percentage[data$category=="Achieved"],"%"))
    
  }else if (weight > maxIdealWeight){
    progress2 <- abs(((weight-maxIdealWeight)/maxIdealWeight)*100)
    percentage2 <- 100-progress2
    
    data <- data.frame(category=c("Achieved", "Not Achieved"), count=c(percentage2, progress2))
    
    #compute fraction
    data$fraction <- data$count / sum(data$count)
    
    # compute the cumulative percentages (top of each rectangle)
    data$ymax <- cumsum(data$fraction)
    
    # compute percentage
    data$percentage <- round(data$fraction*100, digits=2)
    
    # Compute the bottom of each rectangle
    data$ymin <- c(0, head(data$ymax, n=-1))
    
    # Compute label position
    data$labelPosition <- (data$ymax + data$ymin) / 2
    
    # Compute a good label
    data$label <- paste0(data$percentage, "%")
    
    # colour for the chart
    colour<-c("#006400", "#D3D3D3")
    
    # Make the plot
    ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
      geom_rect() + ggtitle("Progress to achieve ideal weight:")+
      scale_fill_manual(values=colour) + # chart colour
      coord_polar(theta="y") +
      xlim(c(-4, 4)) +
      theme_void() +
      theme(legend.position = "none")+
      theme(plot.title = element_text(size=28))+
      annotate(geom='text', x=-4, y=0.25, size=13, colour="dark green", label=paste0(data$percentage[data$category=="Achieved"],"%"))
  }
}

#Read dataset
nutrition<-read.csv("nutrition.csv")

#Create data frame for nutrient table
food_list <- data.frame(matrix(ncol=6,nrow=0))
x <- c("Food","Calories_Per_Serving","Fat_Per_Serving","Quantity","Total_Fat","Total_Calories")
colnames(food_list) <- x

#Create vector to store macro-nutrients and vitamins amount
macro_list <- c("Calcium", "Carbohydrate","Fiber","Iron","Magnesium","Potassium","Protein","Sodium","Water")
vitamin_list <- c("Vitamin A", "Vitamin B6","Vitamin B12","Vitamin C", "Vitamin D", "Vitamin E", "Vitamin K")
sum_macro <- c()
sum_vitamin <- c()
sum_calories <- 0;

shinyServer(function(input, output) {
  observeEvent(input$submit, {
    output$bmi <- renderText(isolate({bmi(input$weight, input$height)}))
    output$bmr <- renderText(isolate({bmr(input$age, input$gender, input$weight, input$height)}))
    output$tdee <- renderText(isolate({tdee(input$activity, bmr(input$age, input$gender, input$weight, input$height))}))
    output$goalweight <- renderText(isolate({noquote(paste0(minIdealWeight(input$height), " to ", (maxIdealWeight(input$height))))}))
    output$bmiStatus <- renderText(isolate({bmiStatus(bmi(input$weight, input$height))}))
    output$dailyCalorie <- renderText(isolate({dailyCalorie(tdee(input$activity, bmr(input$age, input$gender, input$weight, input$height)),bmi(input$weight, input$height))}))
    output$macros <- renderPlot(isolate({macros(bmi(input$weight, input$height),dailyCalorie(tdee(input$activity, bmr(input$age, input$gender, input$weight, input$height)),bmi(input$weight, input$height)))}))
    output$progress <- renderPlot(isolate({progress(input$weight,minIdealWeight(input$height),maxIdealWeight(input$height))}))
  })
  
  #Nutrient table
  this_table<-reactiveVal(food_list)
  observeEvent(input$add, {
  newRow <- rbind(data.frame("Food" = nutrition[[input$food_id,2]] ,
                             "Calories_Per_Serving"= nutrition[[input$food_id,4]],
                             "Fat_Per_Serving" = nutrition[[input$food_id,5]],
                             "Quantity" = input$no_of_serving,
                             "Total_Fat" = input$no_of_serving*as.numeric(nutrition[[input$food_id,5]]),
                             "Total_Calorie" = input$no_of_serving*as.numeric(nutrition[[input$food_id,4]])),this_table())
  this_table(newRow)
  })
  observeEvent(input$delete, {
    this_table(food_list)
  })
  output$nutrient_table<- DT::renderDataTable({
  datatable(this_table(), selection = 'single',editable = TRUE, 
              options = list(dom = 't'))
  })
  
  #Total calories 
  total_calories<-reactiveVal()
  observeEvent(input$add,{
    sum_calories <<- sum_calories + nutrition[[input$food_id,4]]*input$no_of_serving
    total_calories(sum_calories)
  })
  observeEvent(input$delete,{
    sum_calories <<- 0
    total_calories(sum_calories)
  })
  output$calories <- renderValueBox({
    valueBox(total_calories(),"kcal","Calories",icon = icon("fire"), color = "navy")})
  
  #Macro nutrients bar plot
  output$macro_plot <- renderPlotly(total_macro())
  macro <- eventReactive(input$add,{
    if(input$food_id!="" && !is.null(input$no_of_serving&&input$add>0)){
      
      #Macro-nutrients amount of currently selected food
      #value = mg
      newMacro <- c (calcium= nutrition[[input$food_id,16]]*input$no_of_serving,
                     carbohydrate= nutrition[[input$food_id,21]]*100*input$no_of_serving,
                     fiber= nutrition[[input$food_id,22]]*100*input$no_of_serving,
                     iron= nutrition[[input$food_id,17]]*input$no_of_serving,
                     magnesium= nutrition[[input$food_id,18]]*input$no_of_serving,
                     potassium= nutrition[[input$food_id,19]]*input$no_of_serving,
                     protein= nutrition[[input$food_id,20]]*100*input$no_of_serving,
                     sodium = nutrition[[input$food_id,8]]*100*input$no_of_serving,
                     water= nutrition[[input$food_id,25]]*100*input$no_of_serving)
      
      #Add the new food's macro-nutrients info into the vector
      sum_macro <<- cbind(sum_macro,newMacro)
      #Sum up the value of new and initial macro-nutrients amount
      sum_macro <- rowSums(sum_macro)
      #Store the macro-nutrients name with its updated amount in data frame
      total_macro <- data.frame( macro = macro_list, amount = sum_macro)
      
      p <- plot_ly(x = total_macro$macro,
                   y = total_macro$amount,
                   name = "Macronutrients",
                   type = "bar",
                   marker = list(color = "rgb(201, 134, 134)")
      )
      p <- p %>%
        layout( xaxis = list(title = "Nutrients"),
                yaxis = list(title = "Amount (mg)")
        )
    }
  })
  
  #Vitamin bar plot
  output$vitamin_plot <- renderPlotly(vitamin())
  vitamin <- eventReactive(input$add,{
    if(input$food_id!="" && !is.null(input$no_of_serving&&input$add>0)){
      #value = mcg
      newVitamin <- c (A = nutrition[[input$food_id,9]]*input$no_of_serving,
                       B6 = nutrition[[input$food_id,11]]*1000*input$no_of_serving,
                       B12 = nutrition[[input$food_id,10]]*input$no_of_serving,
                       C = nutrition[[input$food_id,12]]*1000*input$no_of_serving,
                       D = nutrition[[input$food_id,13]]*0.025*input$no_of_serving,
                       E = nutrition[[input$food_id,14]]*1000*input$no_of_serving,
                       K = nutrition[[input$food_id,20]]*input$no_of_serving)
      
      sum_vitamin <<- cbind(sum_vitamin,newVitamin)
      sum_vitamin <- rowSums(sum_vitamin)
      total_vitamin <- data.frame( vitamin = vitamin_list, amount = sum_vitamin)
      p <- plot_ly(x = total_vitamin$vitamin,
                   y = total_vitamin$amount,
                   name = "Vitamin",
                   type = "bar",
                   marker = list(color = "rgb(231, 207, 188)")
      )
      p <- p %>%
        layout( xaxis = list(title = "Vitamin"),
                yaxis = list(title = "Amount (mcg) ")
                
        )
    }
  })
  
})

