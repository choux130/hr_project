#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source('global.R', local=TRUE)

# Define server logic required to draw a histogram
shinyServer(
  function(input, output){
    output$plot1 <- renderPlot({
      p=all_bygroup(data,input$var_num, "left_or_not",2)
      print(p)
    })
    output$tab_num <-renderTable({
      t_bygroup(d=data, input$var_num, "left_or_not", 2)
    })
})
