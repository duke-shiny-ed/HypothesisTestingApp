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
  
  sidebarLayout(
    
    sidebarPanel(width = 3,
                 radioButtons("typetest", "Choose a type of alternative hypothesis:",
                              choices = c("$$\\huge{<}$$", "$$\\huge{>}$$", "$$\\huge{\\neq}$$")),
                 
                 numericInput("hypval", "Hypothesized Value:", 5),
                 
                 radioButtons("alpha", "Choose a significance level:",
                              choices = c("0.01", "0.05", "0.1")),
                 
                 actionButton("update", "Update Simulation!", align = "center"),
                 br(),
                 h5("Current Description of Scenario:"),
                 textOutput("reactext")
                 
                 ), # Close sidebarPanel
    
    mainPanel(width = 9,
  
  navbarPage("",
             
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
                      helpText("Null hypothesis: Patient recovery time is greater than or equal to five days.     $$\\huge{H_0 \\geq 5}$$", align = "center"),
                      helpText("Alternative hypothesis: Patient recovery time is less than five days.     $$\\huge{H_A < 5}$$", align = "center"),
                      br(),
                      h4("You can test these hypotheses (and find explanations of the hypotheses themselves) in the following tabs. You can also use the sidebar on the tabs to change the parameters -- you can choose the type of test, the hypothesized value, the level of significance, and which hypothesis is true (of course, in a real experiment, you can't 'choose the truth', but we allow it here so you may observe how it affects the results of the hypothesis test!)")
                      
                     
             ),
             
             # Compare by State
             
             tabPanel("Null and Alternative Hypotheses",
                      
                      
                      # Title/subtitle
                      
                      h2("The Null Hypothesis and Alternative Hypothesis", align = "center"),
                     
                      
                      h4("When performing a hypothesis test, we must start with our claims or hypotheses. The two hypotheses must be mutually exclusive (i.e., they cannot both be true) and together they must cover the entire range of possibilities. We can have a one-sided test, in which case we are testing to see if a value is strictly greater than expected or strictly less than expected, or a two-sided test, in which case we are testing to see if a value is simply different from what is expected (could be greater or less than)."),
                      br(),
                      h4('Null hypothesis - The null hypothesis is generally referred to as the "status quo". It is the claim that is currently generally accepted and believed, or it is the claim of no change -- nothing is going on, there are no effects, nothing is out of the ordinary or different from what we expect. The null hypothesis, often denoted H0, usually includes an = sign -- in a two-sided test, H0 states that the value of interest is equal to something (i.e. H0: mean = 5), and in a one-sided test, H0 states that the value of interest is less than/equal to or greater than/equal to something (i.e. H0: mean <= 5 or H0: mean >= 5).'),
                      br(),
                      h4("Alternative hypothesis - The alternative hypothesis is the new claim we are testing. It states that there is some new effect, some change that has occurred in the experiment. There is something out of the ordinary that we wouldn't usually expect if the same-old status quo was being upheld. The alternative hypothesis, often denoted HA or H1, generally does not include an equals sign -- in a two-sided test, HA will state that the value of interest is different from something (i.e. HA: mean != 5), and in a one-sided test, HA will state that the value of interest is less than or greater than something (i.e. HA: mean < 5 or HA: mean > 5)."),
                      br(),
                      h3("Interpreting Conclusions", align = "center"),
                      h4('When we perform the test, it is important to remember that no matter what our conclusions are, we have not "proved" either claim to be indisputably true; we have simply observed a sample dataset that either has strong evidence against the status quo or does not. Our conclusion will either be to reject the null hypothesis, meaning that the evidence was strong enough for us to reject H0 and be in favor of HA, or fail to reject the null hypothesis, meaning that the evidence was not strong enough to support HA and reject H0. Neither hypothesis is definitely proved, confirmed, or disproved -- the test will only tell us which conclusion this dataset supports.'),
                      
                      plotOutput("nullaltplot")
                      
                      ),
             
             tabPanel("Test Statistic and P-Value",
                    
                      # Title/subtitle
                      
                      h2("Test Statistic, P-Value, and Alpha", align = "center"),
                      
                      br(),
                      
                      h4("Test statistic - The test statistic is essentially a summary of your sample data that compares it with what is expected under the null hypothesis. The general form is (sample stat minus null parameter) / (standard deviation of sample stat)."),
                      br(),
                      h4("p-value : the p value is related to the test statistic -- it is the probability of observing a test statistic as or more extreme than the observed one, given that the null hypothesis is true. In other words, it is the probability of seeing data with evidence as strong or stronger than the data you have seen. Using the distribution of the test statistic (normal, f-distribution, etc.), you can find the probability associated with observing that t-statistic or more extreme, which gives the p-value."),
                      
                      fluidRow(
                        column(width = 9,
                               plotOutput("tstatplot")
                               ),
                        column(width = 3,
                               checkboxInput("showtst", "Show Test Statistic", value = TRUE),
                               checkboxInput("showpval", "Show p-value", value = TRUE),
                               checkboxInput("showalpha", "Show alpha", value = TRUE)),
                               textOutput("conclusion")
                      )
                      
                      ),
             
             tabPanel("Type I and Type II Errors",
                      
                      # Title/subtitle
                      
                      h2("Type I Error and Type II Error", align = "center"),
                      
                      tabsetPanel(
                        
                        tabPanel("Type I Error",
                                 h3("Type I Error", align = "center"),
                                 br(),
                                 h4("Type I Error - This is the probability of rejecting the null hypothesis when it is false. This is generally considered the more serious error of the two types. When calculating the probability of this error, we are under the assumption that the null hypothesis is true (if it isn't, a Type I error cannot be made). The researcher generally sets what probability of Type I error they're willing to accept, also known as the alpha value."),
                                 
                                 fluidRow(
                                   column(width = 3,
                                          h5("Choose an alpha level from the sidebar, and see how the Type I Error area changes.")
                                          
                                   ),
                                   column(width = 9,
                                          plotOutput("type1plot")
                                 )
                                 
                                 )
                              ),
                        
                        tabPanel("Type II Error",
                                 h3("Type II Error", align = "center"),
                                 br(),
                                 h4("Type II Error - This is the probability of failing to reject the null hypothesis when it is false. When calculating the probability of this error, we are under the assumption that the alternative hypothesis is true (if it isn't, a Type II error cannot be made). Its probability is assigned the variable beta. This relates to the power of a test:"),
                                 br(),
                                 h4("Power - The power of a test is 1 - beta, or 1 - P(Type II error) or 1 - P(failing to reject null | null is false). This means that the power of a test can be interpreted as the probability of correctly rejecting the null hypothesis when it is false. Power can be manipulated to reduce/increase the probability of errors."),
                                 
                                 fluidRow(
                                   column(width = 3,
                                          h5("Adjust the slider to fix beta, and choose an alpha level from the sidebar to see how the Type II Error area changes."),
                                          sliderInput("type2beta", "$$\\huge{\\beta:}$$", value = 0.5, min = 0, max = 1)
                                   ),
                                   column(width = 9,
                                          plotOutput("type2plot")
                                   )
                                   
                                 )
                        ),
                        
                        tabPanel("Reducing Error",
                                 h3("Reducing Error", align = "center"),
                                 
                                 fluidRow(
                                   column(width = 3,
                                          h5("Adjust the sliders to fix sample size, and choose an alpha level from the sidebar to see how beta, or P(Type II Error), changes."),
                                          sliderInput("redsampsize", "Sample size:", value = 100, min = 1, max = 10000)
                                   ),
                                   column(width = 9,
                                          textOutput("redbeta")
                                   )
                                   
                                 )
                        )
                        
                        
                        
                      ) # close tabsetPanel
                      
                      ),
             
             tabPanel("Review Quiz",
                      
                      # Title/subtitle
                      
                      h2("Wrapping Up with a Review Quiz", align = "center"),
                      
                      sidebarLayout(
                        sidebarPanel(width = 5,
                          h4("Acknowledgements/Further Resources")
                        ),
                        mainPanel(width = 7,
                                  h3("Let's review!", align = "center")),
                        position = "right"
                      ) # close Review Quiz sidebarLayout
                      
                      )
             
                     ) # close navbarPage
  
                 ) # close mainPanel
                      
             ) # close sideBarLayout
     ) # close UI


# Define server logic required to draw a histogram
server <- function(input, output) {
   


}

# Run the application 
shinyApp(ui = ui, server = server)

