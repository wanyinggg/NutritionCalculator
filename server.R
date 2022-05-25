library(shiny)
library(ggplot2)

bmi <- function(weight, height){
  weight/(height*height)
}

bmr <- function(age, gender, weight, height){
  if(gender=="Male"){
    bmr1 <- 66 + (13.7*weight) + (5*(height*100)) - (6.8*age)
  }else if(gender=="Female"){
    bmr2 <- 655 + (9.6*weight) + (1.8*(height*100)) - (4.7*age)
  }
}

tdee <- function(activity,bmr){
  if(activity=="0"){
    tdee1 <- bmr*1.2
  }else if(activity=="1"){
    tdee2 <- bmr*1.375
  }else if(activity=="2"){
    tdee3 <- bmr*1.55
  }else if(activity=="3"){
    tdee4 <- bmr*1.725
  }else if(activity=="4"){
    tdee5 <- bmr*1.9
  }
}

minIdealWeight <- function(height){
  minIdealWeight <- round(18.5*(height*height), digits=2)
}

maxIdealWeight <- function(height){
  maxIdealWeight <- round(24.9*(height*height), digits=2)
}

idealWeight <- function(minIdealWeight, maxIdealWeight){
  print(minIdealWeight,"to", maxIdealWeight)
}

bmiStatus <- function(bmi){
  
  if(bmi<18.5){
    print("You are underweight. You need to gain weight.")
  }else if(bmi>18.5 && bmi<24.9){
    print("You are normal. Keep on maintaining!")
  }else if(bmi>24.9){
    print("You are overweight. You have to lose weight.")
  }
}

dailyCalorie <- function(tdee, bmi){
  if(bmi<18.5){   #to gain weight
    dailyCalorie1 <- tdee + 1000
  }else if(bmi>18.5 && bmi<24.9){
    dailyCalorie2 <- tdee
  }else if(bmi>24.9){
    dailyCalorie3 <- tdee-1000
  }
}

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

shinyServer(function(input, output) {
  output$bmi <- renderText({bmi(input$weight, input$height)})
  output$bmr <- renderText({bmr(input$age, input$gender, input$weight, input$height)})
  output$tdee <- renderText({tdee(input$activity, bmr(input$age, input$gender, input$weight, input$height))})
  output$goalweight <- renderText({noquote(paste0(minIdealWeight(input$height), " to ", (maxIdealWeight(input$height))))})
  output$bmiStatus <- renderText({bmiStatus(bmi(input$weight, input$height))})
  output$dailyCalorie <- renderText({dailyCalorie(tdee(input$activity, bmr(input$age, input$gender, input$weight, input$height)),bmi(input$weight, input$height))})
  output$macros <- renderPlot({macros(bmi(input$weight, input$height),dailyCalorie(tdee(input$activity, bmr(input$age, input$gender, input$weight, input$height)),bmi(input$weight, input$height)))})
  output$progress <- renderPlot({progress(input$weight,minIdealWeight(input$height),maxIdealWeight(input$height))})
})

