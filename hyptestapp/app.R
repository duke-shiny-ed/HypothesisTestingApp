#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  theme = shinytheme("united"),
  
  withMathJax(),
  
  
  h1("Duke Statistics: Hypothesis Testing", align = "center"),
  
  navbarPage("Choose what you'd like to explore:",
             
             # Overview, explanation of data/app
             
             tabPanel("Overview",
                      h2("Our App", align = "center"),
                      h4("Welcome to Duke University's hypothesis testing app! Here, you can learn about the crucial aspects of conducting and interpreting a hypothesis test, including setting your hypotheses, calculating a test statistic and p-value, making your conclusions, and navigating types of possible errors in your test.", align = "center"),
                      br(),
                      h4("Often in research and data analysis, we want to investigate certain new claims that challenge what is currently believed, so we take some sample data and see whether or not it supports our new claim. However, as there can be significant variation in sample data, we may not know if the evidence our data provide is because it truly supports our new claim or if it was just the result of random fluctuation in the sample. Hypothesis testing is a reliable way to take sample data and test how strongly its evidence supports our new claim -- if the evidence is strong, we can reject the old beliefs and support the new; if it is weak, we cannot reject the old beliefs. To explore this procedure, take a look at the images below to see a sample pathway through our app, and make sure to check out the running example we'll be using throughout the app and how to change its parameters!", align = "center"),
                      
                      h2("Our Example", align = "center"),
                      h4("There is a commonly used drug to treat flu patients that generally helps them recover within a certain number of days on average. A new drug is being developed by a competing company, and they believe that flu patients who use their new drug will recover in a different number of days on average than they would with the new drug.", align = "center"),
                      br(),
                      h4("Suppose the current drug makes patients recover in about 5 days on average, and the new company says that their drug will help patients recover in less than 5 days on average. We experiment with giving patients the new drug, and our hypotheses are:", align = "center"),
                      br(),
                      h4("Null hypothesis: Patient recovery time is greater than or equal to five days.", align = "center"),
                      h4("Alternative hypothesis: Patient recovery time is less than five days.", align = "center"),
                      br(),
                      h4("You can test these hypotheses (and find explanations of the hypotheses themselves) in the following tabs. You can also use the sidebar on the tabs to change the parameters -- you can choose the type of test, the hypothesized value, the level of significance, and which hypothesis is true (of course, in a real experiment, you can't 'choose the truth', but we allow it here so you may observe how it affects the results of the hypothesis test!)")
                      
                     
             ),
             
             # Compare by State
             
             tabPanel("Null and Alternative Hypotheses",
                      
                      sidebarPanel(
                        radioButtons("typetest", "Choose a type of alternative hypothesis:",
                                    choices = c("<", ">", "!=")),
                        
                        numericInput("hypval", "Hypothesized Value:", 5),
                        
                        radioButtons("alpha", "Choose a significance level:",
                                     choices = c("0.01", "0.05", "0.1"))
                      ),
                      
                      # Title/subtitle
                      
                      h2("The Null Hypothesis and Alternative Hypothesis", align = "center")
                     
                      
                      # Input Variables
                      
                      
                      ),
             
             tabPanel("Test Statistic and P-Value",
                      
                      sidebarPanel(
                        radioButtons("typetest", "Choose a type of alternative hypothesis:",
                                     choices = c("<", ">", "!=")),
                        
                        numericInput("hypval", "Hypothesized Value:", 5),
                        
                        radioButtons("alpha", "Choose a significance level:",
                                     choices = c("0.01", "0.05", "0.1"))
                      ),
                      
                      # Title/subtitle
                      
                      h2("Test Statistic, P-Value, and Alpha", align = "center")
                      
                      # Input Variables
                      
                      ),
             
             tabPanel("Type I and Type II Errors",
                      
                      sidebarPanel(
                        radioButtons("typetest", "Choose a type of alternative hypothesis:",
                                     choices = c("<", ">", "!=")),
                        
                        numericInput("hypval", "Hypothesized Value:", 5),
                        
                        radioButtons("alpha", "Choose a significance level:",
                                     choices = c("0.01", "0.05", "0.1"))
                      ),
                      
                      # Title/subtitle
                      
                      h2("Type I Error and Type II Error", align = "center"),
                      
                      tabsetPanel(
                        
                        tabPanel("Type I Error"
                                 
                                 ),
                        
                        tabPanel("Type II Error"
                        ),
                        
                        tabPanel("Reducing Error"
                        )
                        
                        
                        
                      ) # close tabsetPanel
                      
                      ),
             
             tabPanel("Review Quiz",
                      
                      # Title/subtitle
                      
                      h2("Let's review!", align = "center")
                      
                      )
             
             
                      
             ) # close navbarPage
     ) # close UI


# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

