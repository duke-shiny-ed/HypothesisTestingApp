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
library(shinyjs)
library(tidyverse)
library(shinyBS)
library(pwr)
library(grid)
library(ggtext)

# Define UI for application 
ui <- fluidPage(
   
  theme = shinytheme("united"),
  
  useShinyjs(),
  withMathJax(),
  
  tags$script("
MathJax.Hub.Config({
              tex2jax: {
              inlineMath: [['$','$']],
              processEscapes: true
              }
              });"
              ),
  

  # Main title  
  h1("Duke Statistics: Hypothesis Testing", align = "center"),
  
  tags$style(".fullwidth { width: 100% !important; }"),
  
  div(id = "sblay",
  sidebarLayout(
    
    # Sidebar
    div(id = "Sidebar", sidebarPanel(width = 3,
                                
                 bsPopover(id = "helpt", "Use this sidebar to select parameters for the scenario. Select a type of test/alternative hypothesis (explained more on Null/Alternative Hypotheses tab), hypothesized value (5 by default), and significance level (available in tabs after Null/Alternative Hypotheses; explained more on Test Statistic/P-Value tab). Press Update Parameters to update the hypothesis test outputs throughout the app, including the text description of the new scenario at the bottom of the sidebar.", trigger = "hover"),
                 textOutput("helpt"),
                 
                 tags$head(tags$style("#helpt{color: cornflowerblue;
                                 font-size: 15px;
                                 text-align: center;
                                      font-style: italic;
                                      }"
                         )
                 ),
                 
                 # Select test parameters
              
                 radioButtons("typetest", "Choose a type of alternative hypothesis:",
                              choices = c("$${H_A: \\mu < \\mu_0}$$", "$${H_A: \\mu > \\mu_0}$$", "$${H_A: \\mu \\neq \\mu_0}$$")),
                 
                 numericInput("hypval", "Hypothesized Value:", 5),
                 
                div(id = "alphachoice", radioButtons("alpha", "Choose a significance level:",
                              choices = c("0.01", "0.05", "0.1"))
                ),
                
                
                # Update parameters (not sample data)
                 div(id = "firstupdate",
                 fluidRow(
                   
                 actionButton("update", "Update Parameters!"),
                 tags$head(tags$style("#update{color: white;
                                      background-color: mediumseagreen;
                                      text-align: center;
                                      } #firstupdate{
                                      text-align: center;
                                      }"
                         )
                 )
                 )
                 ),
                br(),
                
                # Re-draw sample data
                div(id = "updatedata", 
                    fluidRow(
                  
                  actionButton("updatedat", "Re-Simulate Data!"),
                  tags$head(tags$style("#updatedat{color: white;
                                       background-color: mediumseagreen;
                                       text-align: center;
                                       } #updatedata{
                                       text-align: center;
                                       }"
                         )
                  )
                  )
                ),
                 br(),
                
                 # Reactive description of problem scenario
                 h5("Current Description of Scenario:"),
                 uiOutput("reactext"),
                 br(),
                 uiOutput("reach0"),
                 uiOutput("reach01", style = "text-align: center"),
                 br(),
                 uiOutput("reacha"),
                 uiOutput("reacha1", style = "text-align: center"),
                 br(),
                 uiOutput("reacalpha")
                 
                 ) # Close sidebarPanel
    ), # close div for sidebarPanel
    
    
    
    # Main tabs
    mainPanel(width = 9,
  
  navbarPage("", id = "tabs",
             
             # Overview, explanation of data/app
             
             tabPanel("Overview",
                      h2("Our App", align = "center"),
                      
                      # Describe hypothesis testing and app overview
                      h4("Welcome to Duke University's hypothesis testing app! Here, you can learn about the crucial aspects of conducting and interpreting a hypothesis test, including setting your hypotheses, calculating a test statistic and p-value, making your conclusions, and navigating types of possible errors in your test."),
                      br(),
                      h4("Often in research and data analysis, we want to investigate certain new claims that challenge what is currently believed, so we take some sample data and see whether or not it supports our new claim. However, as there can be significant variation in sample data, we may not know if the evidence our data provide is because it truly supports our new claim or if it was just the result of random fluctuation in the sample. Hypothesis testing is a reliable way to take sample data and test how strongly its evidence supports our new claim -- if the evidence is strong, we can reject the old beliefs and support the new; if it is weak, we cannot reject the old beliefs. Make sure to check out the running example we'll be using throughout the app and how to change its parameters!"),
                      h4("The focus of this app is on hypothesis testing of the population mean, but note that hypothesis testing can be done on any population parameter, including a proportion, median, and more."),
                      p(textOutput("tabov1", inline = TRUE), span(textOutput("tabov2", inline = TRUE), style = "color:cornflowerblue; font-size:18px"), textOutput("tabov3", inline = TRUE)),
                      
                      bsPopover("tabov2", "Find extra information here!", trigger = 'hover', placement = 'top'),
                      
                      
                      # Describe running example
                      h2("Our Example", align = "center"),
                      h4("There is a commonly used drug to treat flu patients that generally helps them recover within a certain number of days on average. A new drug is being developed by a competing company. The new company believes that flu patients who use their new drug will recover in a different number of days on average than they would with the current drug."),
                      br(),
                      h4("Suppose the current drug makes patients recover in about 5 days on average, and the new company says that their drug will help patients recover in less than 5 days on average. We experiment with giving 20 patients the new drug, and our hypotheses are:"),
                      br(),
                      div(id = "helptext1", helpText("Null hypothesis: Patient recovery time is equal to five days:     ${H_0: \\mu = 5}$", align = "center")),
                      div(id = "helptext2", helpText("Alternative hypothesis: Patient recovery time is less than five days:     ${H_A:  \\mu < 5}$", align = "center")),
                      br(),
                      h4("You can test these hypotheses (and find explanations of the hypotheses themselves) in the following tabs. You can also use the sidebar on the tabs to change the parameters and randomly simulate new data -- you can choose the type of test, the hypothesized value, and the level of significance to see how these affect the test and your conclusions."),
                      br(),
                      h6("This app was developed by Abbey List as part of the Summer 2020 Duke Shiny Ed program.", align = "center")
                      
                     
             ),
             
             tabPanel("Null/Alternative Hypotheses",
                      
                      # Font size/color/alignment edits for mostly text outputs throughout the app (not just for this tab)
                      tags$head(tags$style("#helptext1{
                                          text-align: center
                                           } #helptext2{
                                           text-align: center
                                           } #tab11{
                                           font-size: 17px;
                                           } #typhyp{
                                           font-size: 15px
                                           } #choose1{
                                           font-size: 17px
                                           } #choosealt{
                                           font-size: 17 px
                                           } #choose3{
                                           font-size: 17px
                                           } #choosenull{
                                           font-size: 17px
                                           } #choose4{
                                           font-size: 17px
                                           } #tab13{
                                           font-size: 23px;
                                           text-align: center
                                           } #tab14{
                                           font-size: 17px
                                           } #conc{
                                           font-size: 15px
                                           } #tab15{
                                           font-size: 17px
                                           } #choose5{
                                           font-size: 17px
                                           } #choose6{
                                           font-size: 17px
                                           } #tab21 {
                                           font-size: 17px
                                           } #tab22 {
                                           font-size: 17px
                                           } #tab2test{
                                           font-size: 17px
                                           } #tab23 {
                                           font-size: 17px
                                           } #tab24{
                                           font-size: 17px
                                           } #tab25{
                                           font-size: 17px
                                           } #tab2testd {
                                           font-size: 17px
                                           } #tab26 {
                                           font-size: 17px
                                           } #tab27{
                                           font-size: 17px
                                           } #tab28{
                                           font-size: 17px
                                           } #tab2a{
                                           font-size: 17px
                                           } #tab29 {
                                           font-size: 23px;
                                           text-align: center
                                           } #tab210{
                                           font-size: 17px
                                           } #concrules {
                                           font-size: 15px
                                           } #tab211{
                                           font-size: 17px
                                           } #tab281{
                                           font-size: 17px
                                           } #results{
                                           font-size: 17px;
                                           text-align: center
                                           } #sampsize{
                                           font-size: 15px;
                                           text-align: center
                                           } #sampmean{
                                           font-size: 15px;
                                           text-align: center
                                           } #sampsd{
                                           font-size: 15px;
                                           text-align: center
                                           } #teststat{
                                           font-size: 15px;
                                           text-align: center
                                           } #pvalue{
                                           font-size: 15px;
                                           text-align: center
                                           } #displayalpha{
                                           font-size: 15px;
                                           text-align: center
                                           } #displaycrit{
                                           font-size: 15px;
                                           text-align: center
                                           } #conclusion{
                                           font-size: 17px;
                                           text-align: center;
                                           color: darkorange
                                           } #answer2{
                                           font-size: 14px;
                                           text-align: center;
                                           color: cornflowerblue
                                           } #tab282{
                                           font-size: 17px
                                           } #tab283{
                                           font-size: 17px
                                           } #tab2831{
                                           font-size: 17px
                                           } #tab2832{
                                           font-size: 17px
                                           } #tab284{
                                           font-size: 17px
                                           } #tab285{
                                           font-size: 17px
                                           } #tabov1{
                                           font-size: 18px
                                           } #tabov3{
                                           font-size: 18px
                                           } #tab3I1{
                                           font-size: 17px
                                           } #tab3I3{
                                           font-size: 17px
                                           } #tab3I4{
                                           font-size: 17px
                                           } #tab3I5{
                                           font-size: 17px
                                           } #tab3I7{
                                           font-size: 17px
                                           } #tab3I52{
                                           font-size: 17px
                                           } #tab3I54{
                                           font-size: 17px
                                           } #tab3II1{
                                           font-size: 17px
                                           } #tab3II3{
                                           font-size: 17px
                                           } #tab3II4{
                                           font-size: 17px
                                           } #tab3II42{
                                           font-size: 17px
                                           } #tab3II5{
                                           font-size: 17px
                                           } #tab3II52{
                                           font-size: 17px
                                           } #tab3II54{
                                           font-size: 17px
                                           } #tab3II7{
                                           font-size: 17px
                                           } #tab3IIand{
                                           font-size: 17px
                                           } #tab3r{
                                           font-size: 17px
                                           } #tab3r2{
                                           font-size: 17px
                                           } #tab3r3{
                                           font-size: 17px
                                           } #tab3rII{
                                           font-size: 17px
                                           } #tab3r4{
                                           font-size: 17px
                                           } #redbeta{
                                           font-size: 24px;
                                           text-align: center;
                                           padding: 40px
                                           } #redpower{
                                           font-size: 24px;
                                           text-align: center;
                                           vertical-align: middle
                                           } #resources{
                                           font-size: 15px
                                           } #tab2813{
                                           font-size: 17px
                                           }"

                         )
                      ),
                      
                      # Title/subtitle
                      
                      h2("The Null Hypothesis (${H_0}$) and Alternative Hypothesis (${H_A}$)", align = "center"),
                     
                      
                      # Plot and plot instructions/description
                      plotOutput("nullaltplot"),
                     
                      p(textOutput("choose1", inline = TRUE), span(textOutput("choosealt", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("choose3", inline = TRUE), span(textOutput("choosenull", inline = TRUE), style = "color:cornflowerblue"), textOutput("choose4", inline = TRUE), span(textOutput("tab1blue", inline = TRUE), style = "color:blue; font-size:17px"), textOutput("choose5", inline = TRUE), span(textOutput("tab1red", inline = TRUE), style = "color:red; font-size:17px"), textOutput("choose6", inline = TRUE) ),
                      br(),
                      
                      
                      # Null/alt hypotheses introductions and hovers (popovers)
                      textOutput("tab11"),
                      br(),
                      uiOutput("typehyp"),
                      
                      
                      bsPopover(id = "choosealt", 'The alternative hypothesis is the new claim we are testing. It states that there is some new effect, some change that has occurred in the experiment. There is something out of the ordinary that we would not usually expect if the same-old status quo was being upheld.', trigger = "hover"),
                      
                      bsPopover(id = "choosenull", 'The null hypothesis is generally referred to as the "status quo". It is the claim that is currently generally accepted and believed, or it is the claim of no change -- nothing is going on, there are no effects, nothing is out of the ordinary or different from what we expect.', trigger = "hover"),

                      
                      
                      # How to interpret a test conclusion
                      textOutput("tab13"),
                      br(),
                      textOutput("tab14"),
                      br(),
                      uiOutput("conc"),
                      br(),
                      textOutput("tab15"),
                      br()
                      
                      ),
             
             tabPanel("Test Statistic/P-Value",
                    
                      # Title/subtitle
                      
                      h2("Test Statistic, P-Value, and Alpha", align = "center"),
                    
                      
                      # Plot and table of test results
                      fluidRow(
                        column(width = 9,
                               
                               tabsetPanel(
                                 tabPanel("Null Distribution",
                                          plotOutput("tstatplot")
                                 ),
                                 
                                 tabPanel("T-Distribution",
                                          plotOutput("tdistplot")
                                 )
                                 
                               )  
                               
                        ),
                        column(width = 3,
                               
                               textOutput("results"),
                               tableOutput("resultvals"),
                               uiOutput("conclusion"),
                               br(),
                               bsPopover(id = "answer2", "Looking at the true mean, is our conclusion what we would expect? If not, take a look at the Type I/Type II Error tab. Note that in a real world scenario, you CANNOT know the true population parameter, so you cannot definitively know if your test has made an error or not -- this knowledge is only possible in this simulation.", trigger = 'hover', placement = 'top'),
                               
                               textOutput("answer2")
                               
                        )
                      ),
                      p(textOutput("tab281", inline = TRUE), span(textOutput("tab2812", inline = TRUE), style = "color:blue; font-size:17px"), textOutput("tab2813", inline = TRUE), span(textOutput("tab2obsm", inline = TRUE), style = "color:blueviolet; font-size:17px"), textOutput("tab282", inline = TRUE), span(textOutput("tab2rej", inline = TRUE), style = "color:palevioletred; font-size:17px"), textOutput("tab283", inline = TRUE), span(textOutput("tab2ptstat", inline = TRUE), style = "color:blueviolet; font-size:17px"), textOutput("tab2831", inline = TRUE), span(textOutput("tab2pcrit", inline = TRUE), style = "color:palevioletred; font-size:17px"), textOutput("tab2832", inline = TRUE), span(textOutput("tab2ppval", inline = TRUE), style = "color:royalblue; font-size:17px"), textOutput("tab284", inline = TRUE), span(textOutput("tab2palpha", inline = TRUE), style = "color: #E38F8F; font-size:17px"), textOutput("tab285", inline = TRUE) ),
                      br(),
                      
                      # Introducing test statistic with hover
                      textOutput("tab21"),
                      br(),
                      p(textOutput("tab22", inline = TRUE), span(textOutput("tab2test", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("tab23", inline = TRUE) ),
                      h5("$\\large{t^{*} = \\frac{\\bar{x}-\\mu_0}{\\frac{s}{\\sqrt{n}}}}$", align = "center"),
                      
                      bsPopover(id = "tab2test", 'The test statistic is essentially a summary of the sample data, telling us how far it falls from the hypothesized distribution. The given formula is for a hypothesis test of a mean when the population standard deviation is unknown, but the general format is always (sample statistic minus hypothesized value) divided by the standard error (or population standard deviation if known).', trigger = "hover", placement = "top"),
                      
                      uiOutput("tab2testd"),
                      
                      br(),
                      
                      # Introducing p-value and significance level with hover
                      p(textOutput("tab24", inline = TRUE), span(textOutput("tab2p", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), uiOutput("tab25", inline = TRUE) ),
                      
                      bsPopover(id = "tab2p", 'The p-value is directly related to the test statistic -- it is the probability of observing a test statistic as or more extreme than the one observed, given that the null hypothesis is true. In other words, it is the probability of seeing data with evidence against the null hypothesis that is as strong or stronger that the data you have seen. A test for a mean with unknown population standard deviation uses the t-distribution to find the p-value, but other types of tests may use the z-distribution, f-distribution, etc.', trigger = 'hover', placement = "top"),
                      
                      br(),
                      
                      p(textOutput("tab26", inline = TRUE), span(uiOutput("tab261", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), uiOutput("tab2a", inline = TRUE), uiOutput("tab27", inline = TRUE), span(textOutput("tab2c", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("tab28", inline = TRUE) ),
                      
                      bsPopover(id = "tab261", "The significance level is also the probability of a Type I Error, or rejecting the null hypothesis when it is true. A smaller alpha is often chosen in situations where a Type I Error could be potentially harmful, such as releasing an unsafe drug or costing a company lots of money. With a one-sided test, we compare the p-value to alpha, but with a two-sided test, we compare the p-value to alpha divided by 2, since alpha (a probability) is split between the two tails of the distribution.", trigger = 'hover', placement = 'top'),
                      
                      bsPopover(id = "tab2c", "The critical value is found from the t-distribution table, which usually has labels for one-sided and two-sided scenarios. Similar to how the p-value is compared to alpha (or alpha divided by 2, for two-tailed tests), the test statistic can be compared to the critical value to make a conclusion.", trigger = 'hover', placement = 'top'),
                      
                      br(),

                      # How to make conclusions from test results
                      textOutput("tab29"),
                      br(),
                      textOutput("tab210"),
                      br(),
                      uiOutput("concrules"),
                      textOutput("tab211"),
                      br()
                      
                      ),
             
             tabPanel("Type I/Type II Errors",
                      
                      # Title/subtitle
                      
                      h2("Type I Error and Type II Error", align = "center"),
                      
                      tabsetPanel(
                        
                        tabPanel("Type I Error",
                                 h3("Type I Error", align = "center"),
                                 
                                 # Type I Error plot
                                 plotOutput("type1plot"),
                                 p(textOutput("tab3I5", inline = TRUE), span(textOutput("tab3I51", inline = TRUE), style = "color:blue; font-size:17px"), textOutput("tab3I52", inline = TRUE), span(textOutput("tab3I53", inline = TRUE), style = "color:red; font-size:17px"), uiOutput("tab3I54", inline = TRUE), span(textOutput("tab3I6", inline = TRUE), style = "color: #E38F8F; font-size:17px"), textOutput("tab3I7", inline = TRUE)),
                                 
                                 
                                 br(),
                                 
                                 # Type I Error description
                                 p(textOutput("tab3I1", inline = TRUE), span(textOutput("tab3I2", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("tab3I3", inline = TRUE)),
                                 
                                 bsPopover(id = "tab3I2", "Type I Error means rejecting the null hypothesis when it is actually true. This is generally regarded as the more serious of the two error types, especially when it could cause potential harm (i.e. the release of an unsafe drug, the use of faulty manufacturing materials, etc.). The researcher generally chooses the chance of a Type I Error that they are willing to accept by choosing a significance level, as shown below.", trigger = 'hover', placement = 'top'),
                                 br(),
                                 
                                 h5("${P(}$Type I Error${) = P(}$reject ${H_0 | H_0}$ true${) = \\alpha}$", align = "center"),
                                 br(),
                                 
                                 uiOutput("tab3I4"),
                                 
                                 br()
                                 
                                 
                                 
                              ),
                        
                        tabPanel("Type II Error",
                                 h3("Type II Error", align = "center"),
                                 br(),
                                 
                                 # Type II Error plot(s)
                                 plotOutput("type2plot"),
                                 plotOutput("type2plot2"),
                                 p(textOutput("tab3II5", inline = TRUE), span(textOutput("tab3IInull", inline = TRUE), style = "color:blue; font-size:17px"), textOutput("tab3IIand", inline = TRUE), span(textOutput("tab3IIalt", inline = TRUE), style = "color:red; font-size:17px"), textOutput("tab3II52", inline = TRUE), span(uiOutput("tab3II53", inline = TRUE), style = "color: #E38F8F; font-size:17px"), textOutput("tab3II54", inline = TRUE), span(uiOutput("tab3II6", inline = TRUE), style = "color: blue; font-size:17px"), textOutput("tab3II7", inline = TRUE)),
                                 br(),
                                 
                                 # Type II Error description
                                 p(textOutput("tab3II1", inline = TRUE), span(textOutput("tab3II2", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("tab3II3", inline = TRUE)),
                                 bsPopover(id = "tab3II2", "Type II Error means failing to reject the null hypothesis when it is false, or concluding that there is no difference from the null hypothesis when a difference does indeed exist. As shown below, P(Type II Error) is the complement of the power of the test, i.e. it is equal to 1 minus the power.", trigger = 'hover', placement = 'top'),
                                 br(),
                                 
                                 h5("${P(}$Type II Error${) = P(}$fail to reject ${H_0 | H_0}$ false${) = \\beta = 1 - power}$", align = "center"),
                                 br(),
                                 
                                 p(uiOutput("tab3II4", inline = TRUE), span(textOutput("tab3IIpow", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("tab3II42", inline = TRUE)),
                                 bsPopover(id = "tab3IIpow", "The power of a test is 1 - P(Type II Error), or 1 - P(failing to reject null | null is false). This means that the power of a test can be interpreted as the probability of correctly rejecting the null hypothesis when it is false. Power can be manipulated to reduce/increase the probability of errors.", trigger = 'hover', placement = 'top'),
                                 
                                 br()
                                 
                                
                                 
                      
                        ),
                        
                        tabPanel("Reducing Error",
                                 h3("Reducing Error", align = "center"),
                                 
                                 # Reducing error explanation
                                 textOutput("tab3r"),
                                 br(),
                                 uiOutput("tab3r2"),
                               
                                 
                                 br(),
                                 
                                 textOutput("tab3r4"),
                                 br(),
                                 
                                 # Sliders to choose sample size and difference of alt mean from null mean, and power/P(Type II Error) outputs
                                 fluidRow(
                                   column(width = 3,
                                          sliderInput("redsampsize", "Sample size:", value = 20, min = 1, max = 150),
                                          sliderInput("redmindev", "Difference between alternative mean and null mean:", value = 2, min = 0.01, max = 5)
                                        
                                          
                                        
                                      
                                   ),
                                   column(width = 9,
                                          uiOutput("redbeta"),
                                          uiOutput("redpower")
                                   )
                                   
                                 ),
                                 
                                
                                 p(textOutput("tab3r3", inline = TRUE), span(textOutput("tab3rII", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), align = 'center'),
                                 bsPopover(id = "tab3rII", "<ul><li>Increase power - Since power and Type II Error are inversely related, higher power means lower Type II Error.</li><br><li>Increase sample size - Higher sample size makes it easier to detect when the distribution truly differs from the null hypothesis and is not just showing random variability.</li><br><li>Increase difference from the null to be detected - When the alternative distribution is further away (larger difference) from the null, we will be able to reject the null more often.</li><br><li>Increase significance level - Increasing P(Type I Error) results in more correct rejections of the null, but it also yields more incorrect rejections, so this method should be used with caution.</li></ul>", trigger = 'hover', placement = 'right')
                        )
                        
                      ) # close tabsetPanel for Type I/Type II/Reduce Error
                      
                      ),
             
             tabPanel("Quiz",
                      
                      # Review Quiz
                      
                      h2("Wrapping Up with a Review Quiz!", align = "center"),
                      
                     
                       
                                  h3("Let's review!", align = "center"),
                                  
                                  tags$iframe(src = "https://grace-shiny.shinyapps.io/Quiz_HypothesisTesting/",
                                              width = "1000", height = "515",
                                              frameBorder="0")
                                  
                                
                      
                      ),
             tabPanel("Resources",
                      
                      fluidRow(
                        column(width = 7,
                                      
                               # Links to resources
                               h2("Acknowledgements and Further Resources"),
                               uiOutput("resources")
                                      ),
                        column(width = 5,
                               
                               # Project logo
                               div(img(src = "DukeShinyLogo2.png", height = 265, width = 235), style="text-align: center;")
                               
                               
                               )
                               
                               
                               ),
                      
                      # Concluding footer things
                      br(),
                      br(),
                      br(),
                      
                      h4("Thank you for using and supporting this statistical education app -- we hope you enjoyed learning about hypothesis testing!", align = "center"),
                      
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      h6("This app was developed by Abbey List as part of the Summer 2020 Duke Shiny Ed program.", align = "center")
                      
                      
                      
                      
                      )
             
                     ) # close navbarPage
  
                 ) # close mainPanel
                      
             ) # close sideBarLayout
        ) # close div for sideBarLayout
     ) # close UI



server <- function(input, output) {
  
  # Overview Tab Text
  output$tabov1 <- renderText({ c("In the rest of the tabs, be sure to hover your mouse over anything highlighted in ") })
  output$tabov2 <- renderText({ c("cornflower blue")})
  output$tabov3 <- renderText({ c(" for more information!")})
  
  # Sidebar Instruction Text
  output$helpt <- renderText({ c("Hover for Instructions!") })
  
  
  # Reactive values for sidebar test parameters, update with observeEvent when Update Parameters is clicked
  param <- reactiveValues(type = "$${H_A: \\mu < \\mu_0}$$",
                          hyp = 5,
                          alpha = 0.01)
  observeEvent(input$update, {
    param$type <- "dummy"
    param$type <- input$typetest
  })
  observeEvent(input$update, {
    param$hyp <- -300000
    param$hyp <- as.numeric(input$hypval)
  })
  observeEvent(input$update, {
    param$alpha <- -300000
    param$alpha <- as.numeric(input$alpha)
  })
  
  
  
  # Render first part of reactive scenario description, changes based on hypothesis type
  output$reactext <- renderUI({
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
    withMathJax(
    paste("The current drug makes patients recover in ", param$hyp, " days on average, and the new company says that their drug will help patients recover in less than ", param$hyp, " days on average. We experiment by giving 20 patients the new drug, and our hypotheses are:")
    )
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      withMathJax(
        paste("The current drug makes patients recover in ", param$hyp, " days on average, and the new company says that their drug will help patients recover in more than ", param$hyp, " days on average. We experiment by giving 20 patients the new drug, and our hypotheses are:")
      )
    } else{
      
      withMathJax(
        paste("The current drug makes patients recover in ", param$hyp, " days on average, and the new company says that their drug will help patients recover in a different number of days than ", param$hyp, " days on average. We experiment by giving 20 patients the new drug, and our hypotheses are:")
      )
    }
  })
  
  
  # Render null hypothesis (in words) part of reactive scenario description, changes based on hypothesis type
  output$reach0 <- renderUI({
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
      withMathJax(
        paste("Null hypothesis: Patient recovery time is greater than or equal to ", param$hyp, " days.")
      )
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      withMathJax(
        paste("Null hypothesis: Patient recovery time is less than or equal to ", param$hyp, " days.")
      )
    } else{
      
      withMathJax(
        paste("Null hypothesis: Patient recovery time is equal to ", param$hyp, " days.")
      )
    }
  })
  
  
  # Render null hypothesis (in mathematical notation) part of reactive scenario description, changes based on hypothesis type
  output$reach01 <- renderUI({
    
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
      withMathJax(
        paste("${H_0: \\mu =}$", param$hyp)
      )
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      withMathJax(
        paste("${H_0: \\mu =}$", param$hyp)
      )
    } else{
      
      withMathJax(
        paste("${H_0: \\mu =}$", param$hyp)
      )
    }
    
  })
  
  
  # Render alternative hypothesis (in words) part of reactive scenario description, changes based on hypothesis type
  output$reacha <- renderUI({
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
      withMathJax(
        paste("Alternative hypothesis: Patient recovery time is less than ", param$hyp, " days.")
      )
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      withMathJax(
        paste("Alternative hypothesis: Patient recovery time is greater than ", param$hyp, " days.")
      )
    } else{
      
      withMathJax(
        paste("Alternative hypothesis: Patient recovery time is not equal to ", param$hyp, " days.")
      )
    }
  })
  
  
  # Render alternative hypothesis (in mathematical notation) part of reactive scenario description, changes based on hypothesis type
  output$reacha1 <- renderUI({
    
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
      withMathJax(
        paste("${H_A: \\mu <}$", param$hyp)
      )
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      withMathJax(
        paste("${H_A: \\mu >}$", param$hyp)
      )
    } else{
      
      withMathJax(
        paste("${H_A: \\mu \\neq}$", param$hyp)
      )
    }
    
    
  })
  
  # Render significance level part of reactive scenario description
  output$reacalpha <- renderUI({

      withMathJax(
        paste("We have chosen a significance level (${\\alpha}$), or P(Type I Error), of ", param$alpha, ".")
      )

  })
  
  # Text for Null and Alternative Hypotheses Tab 
  output$tab11 <- renderText({ c("When performing a hypothesis test, we must start with our claims, or hypotheses. Generally, there are three possible sets of hypotheses, where ${\\mu_0}$ is the hypothesized value: ")})
  output$typehyp <- renderUI(withMathJax(HTML("<ul><li>${H_0}$: ${\\mu = \\mu_0}$, ${H_A}$: ${\\mu > \\mu_0}$</li><li>${H_0}$: ${\\mu = \\mu_0}$, ${H_A}$: ${\\mu < \\mu_0}$</li><li>${H_0}$: ${\\mu = \\mu_0}$, ${H_A}$: ${\\mu \\neq \\mu_0}$</li></ul>")))
  output$choose1 <- renderText({ c("For the plot above, use the sidebar to choose an ")})
  output$choosealt <- renderText({ c("alternative hypothesis")})
  output$choose3 <- renderText({ c(", which also determines the ")})
  output$choosenull <- renderText({ c("null hypothesis")})
  output$choose4 <- renderText({ c(", and click Update Parameters see how the plotted null (")})
  output$tab1blue <- renderText({ c("blue")})
  output$choose5 <- renderText({ c(") and alternative (")})
  output$tab1red <- renderText({ c("red")})
  output$choose6 <- renderText({ c(") distributions change.")})
  output$tab13 <- renderText({ c("Meaning of the Hypothesis Test")})
  output$tab14 <- renderText({ c('When we perform the test, it is important to remember that no matter what our conclusions are, we have not "proved" either claim to be indisputably true; we have simply observed a sample dataset that either has strong evidence against the status quo or does not. We will either: ')})
  output$conc <- renderUI(withMathJax(HTML("<ul><li>Reject the null hypothesis, meaning that the evidence was strong enough for us to reject ${H_0}$ and be in favor of ${H_A}$, or</li><li>Fail to reject the null hypothesis, meaning that the evidence was not strong enough to support ${H_A}$ and reject ${H_0}$.</li></ul>")))
  output$tab15 <- renderText({ c("Neither hypothesis is definitely proved, confirmed, or disproved -- the test will only tell us which conclusion this dataset supports.")})
  
  
  # Text for Test Statistic/P-Value Tab (Explanatory, about test statistic/p-value/alpha level/critical value)
  output$tab21 <- renderText({ c("When performing the hypothesis test, the calculations below are done with the assumption that the null distribution is the true distribution.")})
  output$tab22 <- renderText({ c("In a hypothesis test for a mean, the formula for the ")})
  output$tab2test <- renderText({ c("test statistic")})
  output$tab23 <- renderText({ c(" takes this general formula:")})
  output$tab24 <- renderText({ c("To find the ")})
  output$tab2p <- renderText({ c("p-value")})
  output$tab25 <- renderUI({ withMathJax(c(", we will need to use the t-distribution with ${n-1}$ degrees of freedom. Generally, a t-distribution calculator is used for this (link in Resources), although you can estimate ranges for the p-value from the t-table."))})
  output$tab2testd <- renderUI(withMathJax(c("where ${\\bar{x}}$ is the sample mean, ${\\mu_0}$ is the hypothesized value, ${s}$ is the sample standard deviation, and ${n}$ is the sample size.")))
  output$tab26 <- renderText({c("The ")})
  output$tab261 <- renderText({ c("significance level")})
  output$tab2a <- renderUI( withMathJax(c(", or alpha (${\\alpha}$)")))
  output$tab27 <- renderUI( withMathJax( c(", is chosen by the researcher before performing the test. It gives the expected proportion of tests that will result in a false positive, or rejecting the null hypothesis when it is true. From ${\\alpha}$, we can find the ")))
  output$tab2c <- renderText({ c("critical value")})
  output$tab28 <- renderText({ c(" from the t-distribution, which is analogous to the test statistic -- it is the point where the probability of seeing a value as or more extreme is equal to the significance level.")})
  
  # Text for Test Statistic/P-Value Tab (Description of plot)
  output$tab281 <- renderText({ c("Above is a plot of the ")})
  output$tab2812 <- renderText({ c("null distribution")})
  output$tab2813 <- renderText({ c(" with the ")})
  output$tab2obsm <- renderText({ c("observed mean")})
  output$tab282 <- renderText({ c(" and ")})
  output$tab2rej <- renderText({ c("rejection region")})
  output$tab283 <- renderText({ c(" positions denoted by lines (corresponding to the ")})
  output$tab2ptstat <- renderText({ c("test statistic")})
  output$tab2831 <- renderText({ c(" and the ")})
  output$tab2pcrit <- renderText({ c("critical value(s)")})
  output$tab2832 <- renderText({ c(" on the t-distribution plot), and the ")})
  output$tab2ppval <- renderText({ c("p-value")})
  output$tab284 <- renderText({ c(" and ")})
  output$tab2palpha <- renderText({ c("alpha (${\\alpha}$)")})
  output$tab285 <- renderText({ c(" shaded in. We can make a conclusion based on our randomly simulated data, with parameters from the sidebar.")})
  
  # Text for Test Statistic/P-Value Tab (How to make conclusions)
  output$tab29 <- renderText({ c("Making Conclusions")})
  output$tab210 <- renderText({ c("To make a conclusion for our hypothesis test, we can either use the p-value and compare it to the significance level, or we can use the test statistic and compare it to the critical value. For any given hypothesis test, both methods will yield the same conclusion. The decision rules are as follows:")})
  output$concrules <- renderUI(withMathJax(HTML("<ul><li>If the p-value is <b>less than</b> ${\\alpha}$ or the absolute value of the test statistic is <strong>greater than</strong> the critical value, <strong>reject</strong> ${H_0}$ in favor of ${H_A}$.</li><li>If the p-value is <strong>greater than</strong> ${\\alpha}$ or the absolute value of the test statistic is <strong>less than</strong> the critical value, <strong>fail to reject</strong> ${H_0}$ in favor of ${H_A}$.</li></ul>")))
  output$tab211 <- renderText({ c("For practical interpretations of these conclusions, see the Null and Alternative Hypotheses tab.")})
  
  
  # Hide sidebar on Overview/Review Quiz/Resources tabs, and hide alpha choice on Null/Alt tab
   observeEvent(input$tabs, {
     if(input$tabs == "Overview" || input$tabs == "Quiz" || input$tabs == "Resources"){
         shinyjs::hide(id = "Sidebar")

         } else if(input$tabs == "Null/Alternative Hypotheses"){
           shinyjs::show(id = "Sidebar")
           shinyjs::hide(id = "alphachoice")
         } else {
           shinyjs::show(id = "alphachoice")
           shinyjs::show(id = "Sidebar")
         }
   })
  
  # Ensure that mainPanel expands to full screen when sidebar is hidden in Overview/Review Quiz/Resources tabs
  observe({
    if (input$tabs %in% c("Type I/Type II Errors", "Test Statistic/P-Value", "Null/Alternative Hypotheses")) {
      shinyjs::show(select = "#sblay > div > div:nth-child(1)")
      shinyjs::removeClass(class = "fullwidth",
                           select = "#sblay > div > div:nth-child(2)")
    } else {
      shinyjs::hide(select = "#sblay > div > div:nth-child(1)")
      shinyjs::addClass(class = "fullwidth",
                        select = "#sblay > div > div:nth-child(2)")
    }
    
  })
  
  # Need second plot on Type II Error tab when alternative hypothesis is !=
  observe({
    if (param$type == "$${H_A: \\mu \\neq \\mu_0}$$") {
      shinyjs::show(id = "type2plot2")

    } else {
      shinyjs::hide(id = "type2plot2")
    }
    
  })
  
  
  # Allow a choice to update data (re-sample the data) on Test Statistic/P-Value page (only needed for this tab, since it's the only tab dependent on the actual sample rather than just the test parameters)
  observeEvent(input$tabs, {
    if(input$tabs == "Test Statistic/P-Value"){
      shinyjs::show(id = "updatedata")
      
    } else {
      shinyjs::hide(id = "updatedata")
      
    }
  })
  
  # Plot theme
  theme_bluewhite <- function (base_size = 11, base_family = "") {
    theme_bw() %+replace% 
      theme(
        panel.grid.major  = element_line(color = "white"),
        panel.background = element_rect(fill = "lightcyan2"),
        panel.border = element_rect(color = "lightblue", fill = NA),
        axis.line = element_line(color = "lightblue"),
        axis.ticks = element_line(color = "lightblue"),
        axis.text = element_text(color = "steelblue", size = 20),
        plot.title = element_text(size=17)
      )
  }
  
  # Set standard deviation, sample size, and difference between alternative/null means
  oursd <- 2
  oursamp <- 20
  ournulldev <- 2
  
  # Sample values for t-distribution plot
  tvals <- rt(1000, oursamp-1)
  tdens <- dt(tvals, oursamp-1)
  tdist <- data.frame(tvals = tvals, tdens = tdens)
  
  # Create initial null distribution values (initial hypothesized value is set to 5 in the app)
  # Note: using standard error rather than standard deviation, due to sampling?
  nullvals <- rnorm(1000, 5, oursd/sqrt(oursamp))
  nulldens <- dnorm(nullvals, 5, oursd/sqrt(oursamp))
  nulldistlite <- data.frame(vals = nullvals, dens = nulldens)
  
  # Create initial alternative (<) distribution values (initial hypothesized value is set to 5 in the app)
  # Note: using standard error rather than standard deviation, due to sampling?
  altlessvals <- rnorm(1000, 5 - ournulldev, oursd/sqrt(oursamp))
  altlessdens <- dnorm(altlessvals, 5 - ournulldev, oursd/sqrt(oursamp))
  altlessdistlite <- data.frame(vals = altlessvals, dens = altlessdens)
  
  # Create initial alternative (>) distribution values (initial hypothesized value is set to 5 in the app)
  # Note: using standard error rather than standard deviation, due to sampling?
  altmorevals <- rnorm(1000, 5 + ournulldev, oursd/sqrt(oursamp))
  altmoredens <- dnorm(altmorevals, 5 + ournulldev, oursd/sqrt(oursamp))
  altmoredistlite <- data.frame(vals = altmorevals, dens = altmoredens)
  
  # Create initial dataset of null and alternative values
  plotdatalite <- data.frame(nullvals = nulldistlite$vals, nulldens = nulldistlite$dens, altlessvals = altlessdistlite$vals, altlessdens = altlessdistlite$dens, altmorevals = altmoredistlite$vals, altmoredens = altmoredistlite$dens)
  
  
  # Create REACTIVE null distribution values, to be used when user updates the sidebar parameters 
  # Note: using standard error rather than standard deviation, due to sampling?
  nulldist <- eventReactive(input$update, {
    nullvals <- rnorm(1000, as.numeric(input$hypval), oursd/sqrt(oursamp))
    nulldens <- dnorm(nullvals, as.numeric(input$hypval), oursd/sqrt(oursamp))
    data.frame(vals = nullvals, dens = nulldens)
  })
  
  # Create REACTIVE alternative (<) distribution values, to be used when user updates the sidebar parameters 
  # Note: using standard error rather than standard deviation, due to sampling?
  altlessdist <- eventReactive(input$update, {
    
      altlessvals <- rnorm(1000, (as.numeric(input$hypval) - ournulldev), oursd/sqrt(oursamp))
      altlessdens <- dnorm(altlessvals, (as.numeric(input$hypval) - ournulldev), oursd/sqrt(oursamp))
      altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)
    
  })
  
  # Create REACTIVE alternative (>) distribution values, to be used when user updates the sidebar parameters 
  # Note: using standard error rather than standard deviation, due to sampling?
  altmoredist <- eventReactive(input$update, {
    
      altmorevals <- rnorm(1000, (as.numeric(input$hypval) + ournulldev), oursd/sqrt(oursamp))
      altmoredens <- dnorm(altmorevals, (as.numeric(input$hypval) + ournulldev), oursd/sqrt(oursamp))
      altmoredist <- data.frame(vals = altmorevals, dens = altmoredens)
    
  })
  
  # Create REACTIVE dataset of null and alternative values
  plotdata <- reactive({
    data.frame(nullvals = nulldist()$vals, nulldens = nulldist()$dens, altlessvals = altlessdist()$vals, altlessdens = altlessdist()$dens, altmorevals = altmoredist()$vals, altmoredens = altmoredist()$dens)
  })

  # Render plot on Null/Alt Hypotheses tab
  output$nullaltplot <- renderPlot({
    
    
    # First plot to be created when app is opened, before user has changed anything in the sidebar
    if(input$update[1] == 0){
      
      # Initial parameters are hypothesized_value = 5 and alternative = < (less than)
      # Doing sampling for initial parameters, could also just use nulldistlite and altlessdistlite from above 
      nullvals <- rnorm(1000, 5, oursd/sqrt(oursamp))
      nulldens <- dnorm(nullvals, 5, oursd/sqrt(oursamp))
      nulldist <- data.frame(vals = nullvals, dens = nulldens)
      
      altlessvals <- rnorm(1000, 5 - ournulldev, oursd/sqrt(oursamp))
      altlessdens <- dnorm(altlessvals, 5 - ournulldev, oursd/sqrt(oursamp))
      altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)
      
      plotdata1 <- data.frame(nullvals = nulldist$vals, nulldens = nulldist$dens, altlessvals = altlessdist$vals, altlessdens = altlessdist$dens)
      
      
      # Make initial plot
      ggplot(data = plotdata1, aes(x = nullvals, y = nulldens)) +
        geom_line(col = "blue", size = 1.5) +
        geom_line(data = plotdata1, aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
        labs(title = "Plotting The <span style='color:blue;'>Null</span> and <span style='color:red;'>Alternative</span> Distributions", x = "", y = "") +
        scale_x_discrete(limits = c(seq(5-4, 5+2))) +
        theme_bluewhite() +
        theme(axis.text.y = element_blank(),
              plot.title = element_markdown(lineheight = 1.1)) 
   
      
      # Make plot using reactive distribution values (in plotdata()) when user has updated the sidebar and alternative is < (less than)
       } else if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){

       ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
         geom_line(col = "blue", size = 1.5) +
         geom_line(data = plotdata(), aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
           labs(title = "Plotting The <span style='color:blue;'>Null</span> and <span style='color:red;'>Alternative</span> Distributions", x = "", y = "") +
           scale_x_discrete(limits = c(seq(isolate({as.numeric(input$hypval)})-4, isolate({as.numeric(input$hypval)})+4))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank(),
                plot.title = element_markdown(lineheight = 1.1))

         
      # Make plot using reactive distribution values (in plotdata()) when user has updated the sidebar and alternative is > (greater than)
      } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){

        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          geom_line(data = plotdata(), aes(x = altmorevals, y = altmoredens), col = "red", linetype = "dashed") +
          labs(title = "Plotting The <span style='color:blue;'>Null</span> and <span style='color:red;'>Alternative</span> Distributions", x = "", y = "") +
          scale_x_discrete(limits = c(seq(isolate({as.numeric(input$hypval)})-4, isolate({as.numeric(input$hypval)})+4))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank(),
                plot.title = element_markdown(lineheight = 1.1)) 

        
      # Make plot using reactive distribution values (in plotdata()) when user has updated the sidebar and alternative is != (not equal to)
      } else{
        
        # Create note to mention that although two alt. distributions are plotted, these are only two possibilities -- only one or the other (or a different (singular) bell curve) is the alternative
        grob <- grobTree(textGrob("With a two-sided test, \n we are hypothesizing that \n the alternative distribution \n could be EITHER above \n OR below the null \n distribution, not both -- \n these are just \n two possibilities.", x=0.03,  y=0.8, hjust=0,
                                  gp=gpar(col="darkred", fontsize=11, fontface="italic")))
        
        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          geom_line(data = plotdata(), aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
          geom_line(data = plotdata(), aes(x = altmorevals, y = altmoredens), col = "red", linetype ="dashed") +
          labs(title = "Plotting The <span style='color:blue;'>Null</span> and <span style='color:red;'>Alternative</span> Distributions", x = "", y = "") +
          scale_x_discrete(limits = c(seq(isolate({as.numeric(input$hypval)})-4, isolate({as.numeric(input$hypval)})+4))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank(),
                plot.title = element_markdown(lineheight = 1.1)) +
          annotation_custom(grob)
      }

  })
  
  # Test Statistic and P-Value tab
  
  # Create reactive sample data that only updates when Re-Simulate Data is clicked
  sampledata <- reactiveValues(data = rnorm(oursamp, 5, oursd/sqrt(oursamp)),
                               realmean = 5)
  
  # When Re-Simulate Data is clicked, we randomly choose a true population mean based on which alternative hypothesis is chosen, and save the new sample data and true mean
  observeEvent(input$updatedat, {
    
    # If alternative is < (less than), choose between hypothesized value and a value less than hypothesized value for true mean
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
      
      realmean <- sample(c(as.numeric(param$hyp), as.numeric(param$hyp) - ournulldev), 1)
      sampledata$data <- rnorm(oursamp, realmean, oursd/sqrt(oursamp))
      sampledata$realmean = realmean
      
    # If alternative is > (greater than), choose between hypothesized value and a value more than hypothesized value for true mean
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      
      realmean <- sample(c(as.numeric(param$hyp), as.numeric(param$hyp) + ournulldev), 1)
      sampledata$data <- rnorm(oursamp, realmean, oursd/sqrt(oursamp))
      sampledata$realmean = realmean
      
      
    # If alternative is != (not equal to), choose between hypothesized value, a value less than hypothesized value, and a value more than hypothesized value for true mean
    } else{
      
      realmean <- sample(c(as.numeric(param$hyp), as.numeric(param$hyp) - ournulldev, as.numeric(param$hyp) + ournulldev), 1)
      sampledata$data <- rnorm(oursamp, realmean, oursd/sqrt(oursamp))
      sampledata$realmean = realmean
      
    }
    
  })
  
  # Create reactive results that respond to updates in both the test parameters and the sample data
  results <- reactive({
    if(param$type == "$${H_A: \\mu < \\mu_0}$$"){
      
      # Take mean and standard deviation of sample data
      sampmean <- mean(sampledata$data)
      sampsd <- sd(sampledata$data)
      
      # Calculate test statistic, p-value, critical value
      tstat <- (sampmean - as.numeric(param$hyp)) / (sampsd / sqrt(oursamp))
      pval <- pt(tstat, oursamp-1)
      critval <- qt(as.numeric(param$alpha), oursamp-1)
      tpos <- qnorm(pval, as.numeric(param$hyp), oursd/sqrt(oursamp))
      critpos <- qnorm(as.numeric(param$alpha), as.numeric(param$hyp), oursd/sqrt(oursamp))
      
      data.frame(sampmean = sampmean, sampsd = sampsd, tstat = tstat,
                 pval = pval, critval = critval, tpos = tpos, critpos = critpos,
                 alpha = param$alpha, hyp = param$hyp, realmean = sampledata$realmean)
      
    } else if(param$type == "$${H_A: \\mu > \\mu_0}$$"){
      
      # Take mean and standard deviation of sample data 
      sampmean <- mean(sampledata$data)
      sampsd <- sd(sampledata$data)
      
      # Calculate test statistic, critical value, p-value
      tstat <- (sampmean - as.numeric(param$hyp)) / (sampsd / sqrt(oursamp))
      pval <- pt(tstat, oursamp-1, lower.tail = FALSE)
      critval <- qt(as.numeric(param$alpha), oursamp-1, lower.tail = FALSE)
      tpos <- qnorm(pval, as.numeric(param$hyp), oursd/sqrt(oursamp), lower.tail = FALSE)
      critpos <- qnorm(as.numeric(param$alpha), as.numeric(param$hyp), oursd/sqrt(oursamp), lower.tail = FALSE)
      
      data.frame(sampmean = sampmean, sampsd = sampsd, tstat = tstat,
                 pval = pval, critval = critval, tpos = tpos, critpos = critpos,
                 alpha = param$alpha, hyp = param$hyp, realmean = sampledata$realmean)
      
    } else{
      
      # (Slightly different procedure for when alternative is != (not equal to))
      
      # Take mean and standard deviation of sample data
      sampmean <- mean(sampledata$data)
      sampsd <- sd(sampledata$data)
      
      # Calculate test statistic
      tstat <- (sampmean - as.numeric(param$hyp)) / (sampsd / sqrt(oursamp))
     
      # Calculate area below test statistic for p-value if test statistic is negative
       if(tstat < 0){
        pval <- pt(tstat, oursamp-1)
        
      } else{
        # Calculate area above test statistic for p-value if test statistic is positive
        pval <- pt(tstat, oursamp-1, lower.tail = FALSE)
      }
      
      
      # Calculate the two critical values
      critval1 <- qt(as.numeric(param$alpha)/2, oursamp-1)
      critval2 <- qt(as.numeric(param$alpha)/2, oursamp-1, lower.tail = FALSE)
      
      # Calculate positions of test statistic and critical values on null distribution, for later use
      if(tstat < 0){
        tpos <- qnorm(pval, as.numeric(param$hyp), oursd/sqrt(oursamp))
      } else{
        tpos <- qnorm(pval, as.numeric(param$hyp), oursd/sqrt(oursamp), lower.tail = FALSE)
      }
      critpos1 <- qnorm(as.numeric(param$alpha)/2, as.numeric(param$hyp), oursd/sqrt(oursamp))
      critpos2 <- qnorm(as.numeric(param$alpha)/2, as.numeric(param$hyp), oursd/sqrt(oursamp), lower.tail = FALSE)
      
      data.frame(sampmean = sampmean, sampsd = sampsd, tstat = tstat,
                 pval = pval, critval1 = critval1, critval2 = critval2, 
                 tpos = tpos, critpos1 = critpos1, critpos2 = critpos2,
                 alpha = param$alpha, hyp = param$hyp, realmean = sampledata$realmean)
      
    }
    
  })
  
  
    # Render Results table
    output$results <- renderText({ c("Results")})
    
    output$resultvals <- renderTable({
      
      if(param$type == "$${H_A: \\mu \\neq \\mu_0}$$"){
        
        # Select and display sample mean/standard deviation, test statistic, p-value, critical values, and alpha for two-sided test
        
        res_table <- results() %>% select(sampmean, sampsd, tstat, pval, critval1, critval2, alpha)
        res_table$sampsize <- oursamp
        
        varnames <- c("Sample Size $n$:", "Observed Mean $\\bar{x}$:", "Sample SD $s$:", "Test Statistic:", "P-Value:", "Critical Values:", "$\\alpha$:")
        
        # Check if rounding of p-value is needed
        if(as.numeric(res_table$pval) >= 0.0001){
        
          varvals <- c(paste("$", round(res_table$sampsize, 0), "$"), paste("$", round(res_table$sampmean, 4), "$"), paste("$", round(res_table$sampsd, 4), "$"), paste("$", round(res_table$tstat, 4), "$"), paste("$", round(res_table$pval, 4), "$"), paste("$", round(res_table$critval1, 4), "$ and $", round(res_table$critval2, 4), "$"), paste("$", res_table$alpha, "$"))
        
          }else{
        
            varvals <- c(paste("$", round(res_table$sampsize, 0), "$"), paste("$", round(res_table$sampmean, 4), "$"), paste("$", round(res_table$sampsd, 4), "$"), paste("$", round(res_table$tstat, 4), "$"), "$<0.0001$", paste("$", round(res_table$critval1, 4), "$ and $", round(res_table$critval2, 4), "$"), paste("$", res_table$alpha, "$"))
            
        }
        
        # Create table to be displayed
        data.frame(Result = varnames, Value = varvals)
        
      } else{
        
        # Select and display sample mean/standard deviation, test statistic, p-value, critical value, and alpha for one-sided test
        
        res_table <- results() %>% select(sampmean, sampsd, tstat, pval, critval, alpha)
        res_table$sampsize <- oursamp
        
        varnames <- c("Sample Size $n$:", "Observed Mean $\\bar{x}$:", "Sample SD $s$:", "Test Statistic:", "P-Value:", "Critical Value:", "$\\alpha$:")
        
        # Check if rounding of p-value is needed
        if(as.numeric(res_table$pval) >= 0.0001){
          
          varvals <- c(paste("$", round(res_table$sampsize, 0), "$"), paste("$", round(res_table$sampmean, 4), "$"), paste("$", round(res_table$sampsd, 4), "$"), paste("$", round(res_table$tstat, 4), "$"), paste("$", round(res_table$pval, 4), "$"), paste("$", round(res_table$critval, 4), "$"), paste("$", res_table$alpha, "$"))
          
        }else{
          
          varvals <- c(paste("$", round(res_table$sampsize, 0), "$"), paste("$", round(res_table$sampmean, 4), "$"), paste("$", round(res_table$sampsd, 4), "$"), paste("$", round(res_table$tstat, 4), "$"), "$<0.0001$", paste("$", round(res_table$critval, 4), "$"), paste("$", res_table$alpha, "$"))
          
        }
        
        # Create table to be displayed
        data.frame(Result = varnames, Value = varvals)
        
        
      }
      
      
      
    })
    
    
    # Render test conclusion
    output$conclusion <- renderUI({
      
      # Conclusion for != test is compared to p-value divided by 2
      if(param$type == "$${H_A: \\mu \\neq \\mu_0}$$"){
        
        if(as.numeric(results()$pval) < (as.numeric(results()$alpha) / 2)){
          withMathJax( c("Conclusion: Reject ${H_0}$"))
        } else{
          withMathJax( c("Conclusion: Fail to reject ${H_0}$"))
        }
        
        
      # Conclusions for < or > tests are compared to p-value
      } else{
      
      if(as.numeric(results()$pval) < as.numeric(results()$alpha)){
        withMathJax( c("Conclusion: Reject ${H_0}$"))
      } else{
        withMathJax( c("Conclusion: Fail to reject ${H_0}$"))
      }
   }
    })
    
    # Output the true mean chosen by the random simulation, for exploration purposes (can't know this value in real life)
    output$answer2 <- renderText({ paste("(True mean, chosen by simulation: ${\\mu = ", results()$realmean, "}$)")})

    # Render plot of results
    output$tstatplot <- renderPlot({
      
      
      # Create initial plot before user has changed anything in the sidebar, using nulldistlite from above
      if(input$update[1] == 0){
   
        ggplot(data = nulldistlite, aes(x = vals, y = dens)) +
          geom_line(col = "blue", size = 1.5) +
          labs(title = "Null Distribution: Plotting the Observed Value, P-Value, Rejection Region, and Alpha", x = "", y = "") +
          scale_x_discrete(limits = c(5)) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) +
          geom_vline(xintercept = results()$tpos, color = "blueviolet", size = 1.5) +
          geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
          geom_area(data = subset(nulldistlite, vals < results()$tpos), colour = "blue", fill = "blue",  alpha = 0.4)+
          geom_area(data = subset(nulldistlite, vals < results()$critpos), colour = "red", fill = "red", alpha = 0.4)+
          geom_text(aes(x=results()$tpos, label="observed mean", y=0.69), colour="blueviolet", angle=90, vjust = 1, size = 7)+
          geom_text(aes(x=results()$critpos, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7)
        
        
      # Create reactive plot for when user has changed parameters and alternative is < (less than)
      } else if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){
        
        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          labs(title = "Null Distribution: Plotting the Observed Value, P-Value, Rejection Region, and Alpha", x = "", y = "") +
          scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) +
          geom_vline(xintercept = results()$tpos, color = "blueviolet", size = 1.5) +
          geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
          geom_area(data = subset(plotdata(), nullvals < results()$tpos), colour = "blue", fill = "blue",  alpha = 0.4)+
          geom_area(data = subset(plotdata(), nullvals < results()$critpos), colour = "red", fill = "red", alpha = 0.4)+
          geom_text(aes(x=results()$tpos, label="observed mean", y=0.69), colour="blueviolet", angle=90, vjust = 1, size = 7)+
          geom_text(aes(x=results()$critpos, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7)
        
        
        
      # Create reactive plot for when user has changed parameters and alternative is > (greater than)
      } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){
        
        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          labs(title = "Null Distribution: Plotting the Observed Value, P-Value, Rejection Region, and Alpha", x = "", y = "") +
          scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) +
          geom_vline(xintercept = results()$tpos, color = "blueviolet", size = 1.5) +
          geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
          geom_area(data = subset(plotdata(), nullvals > results()$tpos), colour = "blue", fill = "blue",  alpha = 0.4)+
          geom_area(data = subset(plotdata(), nullvals > results()$critpos), colour = "red", fill = "red", alpha = 0.4)+
          geom_text(aes(x=results()$tpos, label="observed mean", y=0.69), colour="blueviolet", angle=90, vjust = -1, size = 7)+
          geom_text(aes(x=results()$critpos, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = 1, size = 7)
        
      # Create reactive plot for when user has changed parameters and alternative is != (not equal to)
      } else{
        
        # If test statistic is above mean, adjust shading direction
        if(results()$tstat > 0){
        
        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          labs(title = "Null Distribution: Plotting the Observed Value, P-Value, Rejection Region, and Alpha", x = "", y = "") +
          scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) +
          geom_vline(xintercept = results()$tpos, color = "blueviolet", size = 1.5) +
          geom_vline(xintercept = results()$critpos1, color = "palevioletred", size = 1.5) +
          geom_vline(xintercept = results()$critpos2, color = "palevioletred", size = 1.5) +
          geom_area(data = subset(plotdata(), nullvals > results()$tpos), colour = "blue", fill = "blue",  alpha = 0.4)+
          geom_area(data = subset(plotdata(), nullvals < results()$critpos1), colour = "red", fill = "red", alpha = 0.4)+
          geom_area(data = subset(plotdata(), nullvals > results()$critpos2), colour = "red", fill = "red", alpha = 0.4)+
          geom_text(aes(x=results()$tpos, label="observed mean", y=0.69), colour="blueviolet", angle=90, vjust = -1, size = 7)+
          geom_text(aes(x=results()$critpos1, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7) +
          geom_text(aes(x=results()$critpos2, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = 1, size = 7)
        
        # If test statistic is below mean, adjust shading direction
        } else {
          
          ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
            geom_line(col = "blue", size = 1.5) +
            labs(title = "Null Distribution: Plotting the Observed Value, P-Value, Rejection Region, and Alpha", x = "", y = "") +
            scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_vline(xintercept = results()$tpos, color = "blueviolet", size = 1.5) +
            geom_vline(xintercept = results()$critpos1, color = "palevioletred", size = 1.5) +
            geom_vline(xintercept = results()$critpos2, color = "palevioletred", size = 1.5) +
            geom_area(data = subset(plotdata(), nullvals < results()$tpos), colour = "blue", fill = "blue",  alpha = 0.4)+
            geom_area(data = subset(plotdata(), nullvals < results()$critpos1), colour = "red", fill = "red", alpha = 0.4)+
            geom_area(data = subset(plotdata(), nullvals > results()$critpos2), colour = "red", fill = "red", alpha = 0.4)+
            geom_text(aes(x=results()$tpos, label="observed mean", y=0.69), colour="blueviolet", angle=90, vjust = 1, size = 7)+
            geom_text(aes(x=results()$critpos1, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7) + 
          geom_text(aes(x=results()$critpos2, label="rejection region", y=0.35, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7)
          
          
          
        }
          
      }
    })
    
    
    # Render t-distribution plot
    output$tdistplot <- renderPlot({

      # Create reactive plot for when user has changed parameters and alternative is < (less than)
      if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){
        
        ggplot(data = tdist, aes(x = tvals, y = tdens)) +
          geom_line(col = "black", size = 1.5) +
          labs(title = "T-Distribution: Plotting the Test Statistic, P-Value, Critical Value, and Alpha", x = "", y = "") +
          scale_x_discrete(limits = c(0)) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) +
          geom_vline(xintercept = results()$tstat, color = "blueviolet", size = 1.5) +
          geom_vline(xintercept = results()$critval, color = "palevioletred", size = 1.5) +
          geom_area(data = subset(tdist, tvals < results()$tstat), colour = "blue", fill = "blue",  alpha = 0.4)+
          geom_area(data = subset(tdist, tvals < results()$critval), colour = "red", fill = "red", alpha = 0.4)+
          geom_text(aes(x=results()$tstat, label="test statistic", y=0.3), colour="blueviolet", angle=90, vjust = 1, size = 7)+
          geom_text(aes(x=results()$critval, label="critical value", y=0.27, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7) 
          
      # Create reactive plot for when user has changed parameters and alternative is > (greater than)
      } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){
        
        ggplot(data = tdist, aes(x = tvals, y = tdens)) +
          geom_line(col = "black", size = 1.5) +
          labs(title = "T-Distribution: Plotting the Test Statistic, P-Value, Critical Value, and Alpha", x = "", y = "") +
          scale_x_discrete(limits = c(0)) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) +
          geom_vline(xintercept = results()$tstat, color = "blueviolet", size = 1.5) +
          geom_vline(xintercept = results()$critval, color = "palevioletred", size = 1.5) +
          geom_area(data = subset(tdist, tvals > results()$tstat), colour = "blue", fill = "blue",  alpha = 0.4)+
          geom_area(data = subset(tdist, tvals > results()$critval), colour = "red", fill = "red", alpha = 0.4)+
          geom_text(aes(x=results()$tstat, label="test statistic", y=0.3), colour="blueviolet", angle=90, vjust = -1, size = 7)+
          geom_text(aes(x=results()$critval, label="critical value", y=0.27, size = 20), colour="palevioletred", angle=90, vjust = 1, size = 7) 
        
      # Create reactive plot for when user has changed parameters and alternative is != (not equal to)
      } else {
      
        # If test statistic is above mean, adjust shading direction
        if(results()$tstat > 0){
          
          ggplot(data = tdist, aes(x = tvals, y = tdens)) +
            geom_line(col = "black", size = 1.5) +
            labs(title = "T-Distribution: Plotting the Test Statistic, P-Value, Critical Value, and Alpha", x = "", y = "") +
            scale_x_discrete(limits = c(0)) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_vline(xintercept = results()$tstat, color = "blueviolet", size = 1.5) +
            geom_vline(xintercept = results()$critval1, color = "palevioletred", size = 1.5) +
            geom_vline(xintercept = results()$critval2, color = "palevioletred", size = 1.5) +
            geom_area(data = subset(tdist, tvals > results()$tstat), colour = "blue", fill = "blue",  alpha = 0.4)+
            geom_area(data = subset(tdist, tvals < results()$critval1), colour = "red", fill = "red", alpha = 0.4)+
            geom_area(data = subset(tdist, tvals > results()$critval2), colour = "red", fill = "red", alpha = 0.4)+
            geom_text(aes(x=results()$tstat, label="test statistic", y=0.3), colour="blueviolet", angle=90, vjust = -1, size = 7)+
            geom_text(aes(x=results()$critval1, label="critical value", y=0.27, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7) +
            geom_text(aes(x=results()$critval2, label="critical value", y=0.27, size = 20), colour="palevioletred", angle=90, vjust = 1, size = 7)
        
        # If test statistic is below mean, adjust shading direction
        } else{
          
          ggplot(data = tdist, aes(x = tvals, y = tdens)) +
            geom_line(col = "black", size = 1.5) +
            labs(title = "T-Distribution: Plotting the Test Statistic, P-Value, Critical Value, and Alpha", x = "", y = "") +
            scale_x_discrete(limits = c(0)) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_vline(xintercept = results()$tstat, color = "blueviolet", size = 1.5) +
            geom_vline(xintercept = results()$critval1, color = "palevioletred", size = 1.5) +
            geom_vline(xintercept = results()$critval2, color = "palevioletred", size = 1.5) +
            geom_area(data = subset(tdist, tvals < results()$tstat), colour = "blue", fill = "blue",  alpha = 0.4)+
            geom_area(data = subset(tdist, tvals < results()$critval1), colour = "red", fill = "red", alpha = 0.4)+
            geom_area(data = subset(tdist, tvals > results()$critval2), colour = "red", fill = "red", alpha = 0.4)+
            geom_text(aes(x=results()$tstat, label="test statistic", y=0.3), colour="blueviolet", angle=90, vjust = 1, size = 7)+
            geom_text(aes(x=results()$critval1, label="critical value", y=0.27, size = 20), colour="palevioletred", angle=90, vjust = -1, size = 7) +
            geom_text(aes(x=results()$critval2, label="critical value", y=0.27, size = 20), colour="palevioletred", angle=90, vjust = 1, size = 7)
          
        }
        
        
        
      }
      
    })
    
    #Type I/Type II Error Tab
      
       # Type I Sub-Tab
    
      # Informative text about Type I Error
      output$tab3I1 <- renderText({ c("When performing a hypothesis test, there is always a possibility for an erroneous result. Although we would like to be fairly confident in our conclusions, there is always chance due to random sampling, and this will occasionally lead us to make incorrect decisions. The first kind of error is called ")})
      output$tab3I2 <- renderText({ c("Type I Error")})
      output$tab3I3 <- renderText({ c(", where we reject the null hypothesis when it is actually true.")})
      output$tab3I4 <- renderUI({ withMathJax(c("Type I Error is always conditional on the null hypothesis being true (if ${H_0}$ is not true, a Type I Error, by definition, could not occur). When ${H_0}$ is true, due to random sampling, the probability we will observe data that leads us to reject ${H_0}$ anyway is equal to ${\\alpha}$."))})
      output$tab3I5 <- renderText({ c("For the above plot of the ")})
      output$tab3I51 <- renderText({c("null distribution")})
      output$tab3I52 <- renderText({ c(" and the ")})
      output$tab3I53 <- renderText({ c("alternative distribution")})
      output$tab3I54 <- renderUI({ withMathJax(c(", choose a significance level (${\\alpha}$) from the sidebar, and see how the probability of a Type I Error (shaded in "))})
      output$tab3I6 <- renderText({c("light red")})
      output$tab3I7 <- renderText({c(") changes.")})
      
      
      # Render Type I Error plot
      output$type1plot <- renderPlot({
        
        # Create initial plot before the user has updated any parameters, useing plotdatalite from above
        if(input$update[1] == 0){

          ggplot(data = plotdatalite, aes(x = nullvals, y = nulldens)) +
            geom_line(col = "blue", size = 1.5) +
            geom_line(data = plotdatalite, aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
            labs(title = "Plotting the Type I Error", x = "", y = "") +
            scale_x_discrete(limits = c(5)) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdatalite, nullvals < results()$critpos), colour = "red", fill = "red", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos, label="rejection threshold", y=0.6, size = 20), colour= "black", angle=90, vjust = -1, size = 7)

          
        # Create reactive plot for when user has changed parameters and alternative is < (less than)
        } else if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){
          
          ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
            geom_line(col = "blue", size = 1.5) +
            geom_line(data = plotdata(), aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
            labs(title = "Plotting the Type I Error", x = "", y = "") +
            scale_x_discrete(limits = c(c(isolate({as.numeric(input$hypval)})))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), nullvals < results()$critpos), colour = "red", fill = "red", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos, label="rejection threshold", y=0.6, size = 20), colour= "black", angle=90, vjust = -1, size = 7)
        
          
        # Create reactive plot for when user has changed parameters and alternative is > (greater than)
        } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){
          
          ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
            geom_line(col = "blue", size = 1.5) +
            geom_line(data = plotdata(), aes(x = altmorevals, y = altmoredens), col = "red", linetype = "dashed") +
            labs(title = "Plotting the Type I Error", x = "", y = "") +
            scale_x_discrete(limits = c(c(isolate({as.numeric(input$hypval)})))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), nullvals > results()$critpos), colour = "red", fill = "red", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos, label="rejection threshold", y=0.6, size = 20), colour= "black", angle=90, vjust = -1, size = 7)
        
        # Create reactive plot for when user has changed parameters and alternative is != (not equal to)
        } else{
          
          ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
            geom_line(col = "blue", size = 1.5) +
            geom_line(data = plotdata(), aes(x = altmorevals, y = altmoredens), col = "red", linetype = "dashed") +
            geom_line(data = plotdata(), aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
            labs(title = "Plotting the Type I Error", x = "", y = "") +
            scale_x_discrete(limits = c(c(isolate({as.numeric(input$hypval)})))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), nullvals < results()$critpos1), colour = "red", fill = "red", alpha = 0.4)+
            geom_area(data = subset(plotdata(), nullvals > results()$critpos2), colour = "red", fill = "red", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos1, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos1, label="rejection threshold", y=0.6, size = 20), colour= "black", angle=90, vjust = -1, size = 7) +
            geom_vline(xintercept = results()$critpos2, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos2, label="rejection threshold", y=0.6, size = 20), colour= "black", angle=90, vjust = -1, size = 7)
          
          
        }
        
        
      })
      
      
      # Type II Sub-Tab

      # Informative text about Type II Error
      output$tab3II1 <- renderText({ c("In addition to Type I Error, there is another kind of error called ")})
      output$tab3II2 <- renderText({ c("Type II Error")})
      output$tab3II3 <- renderText({ c(", where we fail to reject the null hypothesis when it is actually false.")})
      output$tab3II4 <- renderUI({ withMathJax(c("Type II Error is always conditional on the alternative hypothesis being true (if $H_A$ is not true, a Type II Error, by definition, could not occur). When $H_A$ is true, due to random sampling, the probability we will observe data that does not lead us to reject $H_0$ (even though we should) is equal to $\\beta$. As seen in the equation, $\\beta$ is also equal to 1 minus the "))})
      output$tab3IIpow <- renderText({ c("power")})
      output$tab3II42 <- renderText({ c(" of the test.")})
      output$tab3II5 <- renderText({ c("For the above plot of the ")})
      output$tab3IInull <- renderText({ c("null distribution")})
      output$tab3IIand <- renderText({ c(" and the ")})
      output$tab3IIalt <- renderText({ c("alternative distribution")})
      output$tab3II52 <- renderText({ c(" , observe how the shaded-in ")})
      output$tab3II53 <- renderUI({ withMathJax(c("$P($Type II Error$)$ ($\\beta$)"))})
      output$tab3II54 <- renderText({ c(" and ")})
      output$tab3II6 <- renderUI({ withMathJax(c("power ($1-\\beta$)"))})
      output$tab3II7 <- renderText({ c("appear on the plot.")})
      
      # Render Type II Error plot
      output$type2plot <- renderPlot({
 
        # Create initial plot before the user has updated any parameters, useing plotdatalite from above
        if(input$update[1] == 0){
          
          ggplot(data = plotdatalite, aes(x = altlessvals, y = altlessdens)) +
            geom_line(col = "red", size = 1.5) +
            geom_line(data = plotdatalite, aes(x = nullvals, y = nulldens), col = "blue", linetype = "dashed") +
            labs(title = "Plotting the Type II Error", x = "", y = "") +
            scale_x_discrete(limits = c(5)) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdatalite, altlessvals > results()$critpos), colour = "red", fill = "red", alpha = 0.4) +
            geom_area(data = subset(plotdatalite, altlessvals < results()$critpos), colour = "blue", fill = "blue", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos, label="rejection threshold", y=0.6, size = 20), colour= "black", angle=90, vjust = -1, size = 7)
          
            
        # Create reactive plot for when user has changed parameters and alternative is < (less than)
        } else if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){
          
          ggplot(data = plotdata(), aes(x = altlessvals, y = altlessdens)) +
            geom_line(col = "red", size = 1.5) +
            geom_line(data = plotdata(), aes(x = nullvals, y = nulldens), col = "blue", linetype = "dashed") +
            labs(title = "Plotting the Type II Error", x = "", y = "") +
            scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), altlessvals > results()$critpos), colour = "red", fill = "red", alpha = 0.4) +
            geom_area(data = subset(plotdata(), altlessvals < results()$critpos), colour = "blue", fill = "blue", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos, label="rejection threshold", y=0.6, size = 20), colour="black", angle=90, vjust = -1, size = 7)
        
        # Create reactive plot for when user has changed parameters and alternative is > (greater than)
        } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){
          
          ggplot(data = plotdata(), aes(x = altmorevals, y = altmoredens)) +
            geom_line(col = "red", size = 1.5) +
            geom_line(data = plotdata(), aes(x = nullvals, y = nulldens), col = "blue", linetype = "dashed") +
            labs(title = "Plotting the Type II Error", x = "", y = "") +
            scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), altmorevals < results()$critpos), colour = "red", fill = "red", alpha = 0.4) +
            geom_area(data = subset(plotdata(), altmorevals > results()$critpos), colour = "blue", fill = "blue", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos, label="rejection threshold", y=0.6, size = 20), colour="black", angle=90, vjust = -1, size = 7)

        # Create reactive plot for when user has changed parameters and alternative is != (not equal to)
        } else{
          
          # Create note about using two plots to represent Type II Error for two-sided test
          grob <- grobTree(textGrob("For a two-sided test, there are two \n possibilities for the alternative \n distribution: less than or greater than \n the null distribution. This plot shows \n less than; see the plot below for \n greater than.", x=0.01,  y=0.80, hjust=0,
                                    gp=gpar(col="darkred", fontsize=11, fontface="italic")))
          
          ggplot(data = plotdata(), aes(x = altlessvals, y = altlessdens)) +
            geom_line(col = "red", size = 1.5) +
            geom_line(data = plotdata(), aes(x = nullvals, y = nulldens), col = "blue", linetype = "dashed") +
            labs(title = "Plotting the Type II Error - Two Sided Less Than", x = "", y = "") +
            scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), altlessvals > results()$critpos1), colour = "red", fill = "red", alpha = 0.4) +
            geom_area(data = subset(plotdata(), altlessvals < results()$critpos1), colour = "blue", fill = "blue", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos1, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos1, label="rejection threshold", y=0.6, size = 20), colour="black", angle=90, vjust = -1, size = 7) +
            annotation_custom(grob)
          
          
        }
        
        
        
      })
      
      # Creating second Type II Error plot if alternative is != (not equal to)
      output$type2plot2 <- renderPlot({
        
        # Just a way to check if it is two-sided, there are two critical values so `critpos1` will be a variable in results dataframe
        if("critpos1" %in% names(results())){
          
          ggplot(data = plotdata(), aes(x = altmorevals, y = altmoredens)) +
            geom_line(col = "red", size = 1.5) +
            geom_line(data = plotdata(), aes(x = nullvals, y = nulldens), col = "blue", linetype = "dashed") +
            labs(title = "Plotting the Type II Error - Two Sided Greater Than", x = "", y = "") +
            scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
            theme_bluewhite() +
            theme(axis.text.y = element_blank()) +
            geom_area(data = subset(plotdata(), altmorevals < results()$critpos2), colour = "red", fill = "red", alpha = 0.4) +
            geom_area(data = subset(plotdata(), altmorevals > results()$critpos2), colour = "blue", fill = "blue", alpha = 0.4) +
            geom_vline(xintercept = results()$critpos2, color = "palevioletred", size = 1.5) +
            geom_text(aes(x=results()$critpos2, label="rejection threshold", y=0.6, size = 20), colour="black", angle=90, vjust = -1, size = 7)
          
        }
        
        
      })
      
      # Reducing Error sub-tab
      
      # Informative text about reducing error
      output$tab3r <- renderText({ c("When performing a hypothesis test, we generally want to reduce the probability of error as much as possible. There are several ways to reduce error.")})
      output$tab3r2 <- renderUI({ withMathJax(c("For Type I Error, the main way to reduce its probability is to choose a lower $\\alpha$, as this value can generally be fixed by the researcher."))})
      output$tab3r3 <- renderText({ c("As you can see, there are multiple methods to ")})
      output$tab3rII <- renderText({ c("reduce Type II Error.")})
      output$tab3r4 <- renderText({ c("What about Type II Error? Using your parameters from the sidebar and the sliders below, adjust the values to see how P(Type II Error) changes.")})
      
      # Calculating power using sliders for sample size and difference of alternative mean from null mean
      output$redpower <- renderUI({
        
        # Calculate power for alternative of < (less than)
        if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){
          
          test <- pwr.t.test(d = -as.numeric(input$redmindev)/(oursd), n = as.numeric(input$redsampsize), sig.level = as.numeric(results()$alpha), type = "one.sample", alternative = "less")
          
          power <- round(test$power, 5)
          
          withMathJax(paste("Power, or $1 - \\beta$: ", round(power, 5)))
          
        # Calculate power for alternative of > (greater than)
        } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){ 
        
          test <- pwr.t.test(d = as.numeric(input$redmindev)/(oursd), n = as.numeric(input$redsampsize), sig.level = as.numeric(results()$alpha), type = "one.sample", alternative = "greater")
          
          power <- round(test$power, 5)
          
          withMathJax(paste("Power, or $1 - \\beta$: ", round(power, 5)))
        
        # Calculate power for alternative of != (not equal to)
        } else{
          
          test <- pwr.t.test(d = as.numeric(input$redmindev)/(oursd), n = as.numeric(input$redsampsize), sig.level = as.numeric(results()$alpha), type = "one.sample", alternative = "two.sided")
          
          power <- round(test$power, 5)
          
          withMathJax(paste("Power, or $1 - \\beta$: ", round(power, 5)))
          
          
        }
        
      })
      
      
      # Calculating P(Type II Error) using sliders for sample size and difference of alternative mean from null mean
      output$redbeta <- renderUI({

        # Calculate power for alternative of < (less than)
        if(isolate({input$typetest == "$${H_A: \\mu < \\mu_0}$$"})){
          
          test <- pwr.t.test(d = -as.numeric(input$redmindev)/(oursd), n = as.numeric(input$redsampsize), sig.level = as.numeric(results()$alpha), type = "one.sample", alternative = "less")
          
          power <- round(test$power, 5)
          
          withMathJax(paste("P(Type II Error), or $\\beta$: ", round(1-power, 5)))
         
        # Calculate power for alternative of > (greater than) 
        } else if(isolate({input$typetest == "$${H_A: \\mu > \\mu_0}$$"})){ 
          
          test <- pwr.t.test(d = as.numeric(input$redmindev)/(oursd), n = as.numeric(input$redsampsize), sig.level = as.numeric(results()$alpha), type = "one.sample", alternative = "greater")
          
          power <- round(test$power, 5)
          
          withMathJax(paste("P(Type II Error), or $\\beta$: ", round(1-power, 5)))
        
        # Calculate power for alternative of != (not equal to)  
        } else{
          
          test <- pwr.t.test(d = as.numeric(input$redmindev)/(oursd), n = as.numeric(input$redsampsize), sig.level = as.numeric(results()$alpha), type = "one.sample", alternative = "two.sided")
          
          power <- round(test$power, 5)
          
          withMathJax(paste("P(Type II Error), or $\\beta$: ", round(1-power, 5)))
          
          
        }
        
      })
      
      # Further Resources
      
      output$resources <- renderUI(HTML('<ul><li><i>OpenIntro Statistics</i> by Christopher Barr, David M. Diez, and Mine Cetinkaya-Rundel: Section 5</li><li><em>Probability and Statistics for Engineering and the Sciences</em> by Jay Devore: Chapter 8</li><li><a href = "https://statisticsbyjim.com/hypothesis-testing/types-errors-hypothesis-testing/">Types of Errors in Hypothesis Testing</a> by Jim Frost</li><li><a href = "https://online.stat.psu.edu/statprogram/reviews/statistical-concepts/hypothesis-testing/critical-value-approach">S.3.1 Hypothesis Testing (Critical Value Approach)</a></li><li><a href = "https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/basic-statistics/inference/supporting-topics/basics/what-is-a-test-statistic/#:~:text=A%20test%20statistic%20is%20a,expected%20under%20the%20null%20hypothesis.">What is a test statistic?</a></li><li><a href = "https://online.stat.psu.edu/statprogram/reviews/statistical-concepts/proportions">S.6 Hypothesis Test of Proportion</a></li><li><a href = "http://www.statskingdom.com/130MeanT1.html">One-Sample T-Test (T Distribution) Calculator</a></li></ul>'))
      
      
      
      }

# Run the application 
shinyApp(ui = ui, server = server)

