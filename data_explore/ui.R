#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source('global.R', local=TRUE)

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    titlePanel("Data Exploration for HRA Data"),
    sidebarPanel(
      tags$div(tags$strong(tags$h4("Response Variable:")),
               tags$ul(
                 tags$li(
                   HTML(paste("Whether the employee has left — ", "(0=”leave”, 1=”not leave”)", sep="<br/>")))),
               tags$strong(tags$h4("Categorical Explanatory Variables:")),
               tags$ul(
                 tags$li("Whether they have had a work accident — (0=”No”, 1=”Yes”)"),
                 tags$li("Whether they have had a promotion in the last 5 years — (0=”No”, 1=”Yes”)"),
                 tags$li(HTML(paste("Department -", "(“accounting”, “hr”, “IT”, “management”, “marketing”, “product_mng”, 
                         “RandD”, “sales”, “support”, “technical”)", sep="<br/>"))), 
                 tags$li("Salary — (“high”, “medium”, “low”)")),
               tags$strong(tags$h4("Numeric Explanatory Variables:")),
               tags$ul(
                 tags$li("Employee satisfaction level— (between 0 and 1)"),
                 tags$li("Last evaluation — (between 0 and 1)"),
                 tags$li("Number of projects — (integer)"),
                 tags$li("Average monthly hours — (integer)"),
                 tags$li("Time spent at the company — (integer)")))),
      mainPanel(
        tabsetPanel( tabPanel("Response Variable vs. Numeric Explanatory Variables", 
                              fluidRow(
                                column(width = 6, offset = 0,
                                       selectInput("var_num", label = h4("Numeric Explanatory Variables"), 
                                                   c("Whether the employee has left (numeric)"="satisf_level",
                                                   "Last evaluation (numeric)"="last_eval",
                                                   "Number of projects (numeric)"="num_proj",
                                                   "Average monthly hours (numeric)"="ave_mon_hrs",
                                                   "Time spent at the company (numeric)"="time_spend")))
                              ),
                              tableOutput('tab_num'),
                              plotOutput('plot1')), 
                     tabPanel("Response Variable vs. Categorical Explanatory Variables", 
                              fluidRow(
                                column(width = 6, offset = 0,
                                       selectInput("cat_num", label = h4("Categorical Explanatory Variables"), 
                                                   c("Work accident or not (categorical)"="work_accid",
                                                     "Had a promotion in the last 5 years or not (categorical)"="promo_last_5yrs",
                                                     "Department (categorical)"="department",
                                                     "Salary (categorical)"="salary")))
                                ),
                              tableOutput('table1')), 
                     tabPanel("Between Numeric Explanatory Variables", plotOutput('plot2')),
                     tabPanel("Between Categorical Explanatory Variables", tableOutput('table2')))
        
    )
  )
)
