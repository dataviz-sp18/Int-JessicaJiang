
###############################
#             ui              #
###############################

ui <- fluidPage(
  #header
  headerPanel("Are You Healthier than Your Peers in Mexico?"),
  
  sidebarPanel(
    #add gender bar 
    selectInput(inputId = "gender", 
                label = "Gender:",
                list("female", "male"),
                selected = "male"),
    #add age bar 
    selectInput(inputId = "age", 
                label = "Age:",
                list("0-4","5-9","10-14","15-19","20-24","25-29",
                     "30-34","34-39","40-44","45-49","50-54",
                     "55-59", "60-64","65-69", "70-74","75-79",
                     "80-84","85-89","90-94","95-100"),
                selected = "25-29"),
    
    #add weight input
    sliderInput(inputId = "weight",
                label = "Weight in kg:",
                value = 63, min = 0, max= 200, step = 0.1),
    
    #add height input
    sliderInput(inputId = "height",
                label = "Height in m:",
                value = 1.75, min = 0, max= 2.5, step = 0.01),
    
    #self-image
    selectInput(inputId = "selfimage", 
                label = "How do you portrait yourself:",
                list("underweight", "normal", "overweight", "obese"))
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Visualization", plotOutput("graph")), 
      tabPanel("Profile", verbatimTextOutput("profile")),
      tabPanel("About", verbatimTextOutput("about")))
  )
)




###############################
#           Server            #
###############################

server <- function(input, output){
  
  library(rsconnect)
  library(data.table)
  library(shiny)
  library(dplyr)
  library(plotly)
  library(shinythemes)
  library(readr)
  library(ggplot2)
  library(ggtree)
  library(grid)
  library(magick)
  library(magrittr)
  library(png)
  library(ri)
  
  options(repos = BiocInstaller::biocinstallRepos())
  
  DT <- as.data.table(read.csv("DT.csv"))
  DT$age_range <- as.factor(DT$age_range)
  DT$age_range <- factor(DT$age_range, levels =c("0-4","5-9","10-14","15-19","20-24","25-29",
                            "30-34","34-39","40-44","45-49","50-54",
                            "55-59", "60-64","65-69", "70-74","75-79",
                            "80-84","85-89","90-94","95-100"))

  #requested data set for summary data 
  datasetInput <- reactive({
    if (input$gender == "female") {DT <- DT[DT$sex == "female",]}
    else {DT <- DT[DT$sex == "male",]}
    
  })
  
  #output hist
  output$graph <- renderPlot({
    
    #bring in and imbed images
    legend <- readPNG(paste0("image/legend.png"))
    legend <- rasterGrob(legend, interpolate=TRUE)
    
    if(input$gender == "female"){
      uw <- readPNG(paste0("image/female/uw.png"))
      uw <- rasterGrob(uw, interpolate=TRUE)
      nm <- readPNG(paste0("image/female/nm.png"))
      nm <- rasterGrob(nm, interpolate=TRUE)
      ow <- readPNG(paste0("image/female/ow.png"))
      ow <- rasterGrob(ow, interpolate=TRUE)
      ob <- readPNG(paste0("image/female/ob.png"))
      ob <- rasterGrob(ob, interpolate=TRUE)
    }
    else if(input$gender == "male"){
      #bring in imgaes
      uw <- readPNG(paste0("image/male/uw.png"))
      uw <- rasterGrob(uw, interpolate=TRUE)
      nm <- readPNG(paste0("image/male/nm.png"))
      nm <- rasterGrob(nm, interpolate=TRUE)
      ow <- readPNG(paste0("image/male/ow.png"))
      ow <- rasterGrob(ow, interpolate=TRUE)
      ob <- readPNG(paste0("image/male/ob.png"))
      ob <- rasterGrob(ob, interpolate=TRUE)
    }
    

    #imbed images
    base <-
      ggplot(datasetInput(),aes(x = bmi, y = age_range, label = round(bmi, 2))) +
      annotation_custom(uw,xmin=15,   xmax=18,   ymin=4, ymax=15)  +
      annotation_custom(nm,xmin=19.1, xmax=22.1, ymin=4, ymax=15)  +
      annotation_custom(ow,xmin=24,   xmax=27 ,  ymin=4, ymax=15)  +
      annotation_custom(ob,xmin=28,              ymin=4, ymax=15)  +
      annotation_custom(legend,xmin=24.8,       ymin=0.3, ymax=2.7 )  +
      
      geom_point(size = 3) +
      geom_vline(xintercept = 18.5, linetype="dashed", color = "black") +
      geom_vline(xintercept = 23.0, linetype="dashed", color = "black") +
      geom_vline(xintercept = 27.5, linetype="dashed", color = "black") +
      
      labs(x = "Mean Body Mass Index (Mean BMI)", y = "Age") +
      theme(plot.title = element_text(size = 20, margin = margin(b = 10)))
    
    
    
    
    
    #You are here
    BMI_calculated <- (input$weight)/(input$height*input$height)
    BMI_calculated_show <- BMI_calculated 
    if (BMI_calculated > 32) {BMI_calculated_show = 32}
    else if (BMI_calculated < 15) {BMI_calculated_show = 15}
    base <- base +  
      geom_point(aes(x = BMI_calculated_show, y = input$age),  color="yellow", size = 3) +
      geom_segment(aes(x=min(DT[(DT$age_range == input$age & DT$sex == input$gender),]$bmi,BMI_calculated_show),
                       xend=max(DT[(DT$age_range == input$age & DT$sex == input$gender),]$bmi,BMI_calculated_show),
                       y=input$age,
                       yend=input$age), color = "yellow") 
    
    #self-image 
    if (input$selfimage == "underweight" ) BMI_perceived = 16
    else { 
      if (input$selfimage == "normal") BMI_perceived = 21
      else {
        if (input$selfimage == "overweight") BMI_perceived = 25.5
        else {
          if(input$selfimage == "obese") BMI_perceived = 30
        }
      }
    }
    
    #visualize self-image
    base <- base + 
      geom_point(aes(x = BMI_perceived, y = input$age),  color="green", size = 3) +
      geom_segment(aes(x=min(DT[(DT$age_range == input$age & DT$sex == input$gender),]$bmi,BMI_perceived),
                       xend=min(DT[(DT$age_range == input$age & DT$sex == input$gender),]$bmi,BMI_calculated_show),
                       y=input$age,
                       yend=input$age), color = "green") 
    
    #add BMI group name    
    base <- base + 
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(panel.background = element_blank()) +
      scale_x_continuous(limits = c(15, 32), breaks = seq(15, 32, by = 3)) +
      theme(plot.title = element_text(size = 20, margin = margin(b = 10))) 
    
    
    grob1 <- grobTree(textGrob("underweight", x = 0.032, y = 0.952, hjust = 0,
                               gp=gpar(col="#333333", fontsize=13, fontface="italic")))
    grob2 <- grobTree(textGrob("normal",      x = 0.31,  y = 0.952, hjust = 0,
                               gp=gpar(col="#333333", fontsize=13, fontface="italic")))
    grob3 <- grobTree(textGrob("overweight",  x = 0.52,  y = 0.952, hjust = 0,
                               gp=gpar(col="#333333", fontsize=13, fontface="italic")))
    grob4 <- grobTree(textGrob("obese",       x = 0.84,  y = 0.952, hjust = 0,
                               gp=gpar(col="#333333", fontsize=13, fontface="italic")))
    base <- base +
      annotation_custom(grob1) +
      annotation_custom(grob2) +
      annotation_custom(grob3) +
      annotation_custom(grob4) 
    
    
    #print plot
    base
  })
  
  #profile
  #basic statistics:
  output$profile <- renderPrint({
    
    if (input$selfimage == "underweight" ) BMI_perceived = 16 
    else { 
      if (input$selfimage == "normal") BMI_perceived = 21
      else { 
        if (input$selfimage == "overweight") BMI_perceived = 25.5
        else { if(input$selfimage == "obese") BMI_perceived = 30
        }
      }
    }
    
    BMI_calculated <- (input$weight)/(input$height*input$height)
    
    cat("You portrait yourself as", input$selfimage)
    cat(". Your actual BMI is", BMI_calculated, ". ")
    cat("\n")
    
    if (BMI_calculated > BMI_perceived) cat("You under-estiamte your BMI by", BMI_calculated - BMI_perceived)
    else {
      if(BMI_calculated < BMI_perceived) cat("You over-estiamte your BMI by",  BMI_perceived - BMI_calculated)
      else {
        cat("You correctlt estimate your BMI")
      }
    }
    cat("\n")
    
    #text annotation of when BMI>32 and BMI<15 (out of bound)
    if (BMI_calculated > 32) { cat("Attention: Your BMI is higher than 32, and it is out of the range in the plot.")
      cat("\n")}
    if (BMI_calculated < 15){ cat("Attention: Your BMI is lower than 15, and it is out of the range in the plot.")
      cat("\n")}
    
    ###classification for 4 groups###
    if (BMI_calculated >= 27.5 ) cat("Your BMI indicates that you might be obese.")
    else { 
      if (BMI_calculated >= 23.5) cat("Your BMI indicates that you might over-weight.")
      else {
        if (BMI_calculated >= 18.5) cat("Your BMI indicates that you might be normal.")
        else {
          if(BMI_calculated >= 0) cat("Your BMI indicates that you might under-weight.")
          else cat(" Wrong input.")
        }
      }
    }
    cat("\n")
    cat("The average BMI of your peers of you age and gender in Mexico is", 
        DT[(DT$age_range == input$age & DT$sex == input$gender),]$bmi, ".")
  })
  
  #suggestion 
  output$about <- renderPrint({
    cat("Data represents average BMI by age group for male and female in Mexico during 2012.")
    cat("\n")
    cat("Mexico ranks the most obese country in the world in adult obesity (as of 2012).")
    cat("\n")
    cat("The classification to categorize BMI was the World health Organization's (WHO).")
    cat("\n")
    cat("\n")
    cat("In order to see which group you belong to, you need to choose your gender, age group, weight in kilograms, and height in meters. Meanwhile, in order to provide you better suggestions, we suggest you to choose your self-portrait.")
  })
  
}


shinyApp(ui = ui, server = server)

