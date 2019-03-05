library(shiny)
source("D:/GitHub/Animated-SLO/Inputs.R")

ui <- fluidPage(

      mainPanel(
        sliderInput("pretest",
                    "Select prior achievement score:",
                    min = round(min(train$pretest), digits=2),
                    max = round(max(train$pretest), digits=2),
                    value = 0),
        radioButtons("subgroup",
                     "Select subgroup:",
                     choices = c("None", "ELL", "SPED", "Both")),
         textOutput("goal")
      )
   )

server <- function(input, output) {

   output$goal <- renderText({
     tmp <- data.frame(id = 10005, 
                       pretest = input$pretest, 
                       subgroup = input$subgroup, 
                       ELL = ifelse(input$subgroup=="None" | input$subgroup=="SPED", 0, 1), 
                       SPED = ifelse(input$subgroup=="None" | input$subgroup=="ELL", 0, 1))
     paste("Your student's goal is", round(predict(mod, newdata = tmp), digits = 2))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

