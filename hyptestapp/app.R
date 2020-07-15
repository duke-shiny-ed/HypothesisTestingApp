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

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  theme = shinytheme("united"),
  
  useShinyjs(),
  withMathJax(),
  
  
  # ['\\(','\\)']
  tags$script("
MathJax.Hub.Config({
              tex2jax: {
              inlineMath: [['$','$']],
              processEscapes: true
              }
              });"
              ),
  
  
  h1("Duke Statistics: Hypothesis Testing", align = "center"),
  
  tags$style(".fullwidth { width: 100% !important; }"),
  
  div(id = "sblay",
  sidebarLayout(
    
    div(id = "Sidebar", sidebarPanel(width = 3,
                                
                 bsPopover(id = "helpt", "Use this sidebar to select parameters for the scenario. Select a type of test/alternative hypothesis (explained more on Null/Alternative Hypotheses tab), hypothesized value (5 by default), and significance level (explained more on Test Statistic/P-Value tab). Press Update Simulation to update the hypothesis test outputs throughout the app, including the text description of the new scenario at the bottom of the sidebar.", trigger = "hover"),
                 textOutput("helpt"),
                 
                 tags$head(tags$style("#helpt{color: cornflowerblue;
                                 font-size: 15px;
                                 text-align: center;
                                      font-style: italic;
                                      }"
                         )
                 ),
              
                 radioButtons("typetest", "Choose a type of alternative hypothesis:",
                              choices = c("$$\\huge{<}$$", "$$\\huge{>}$$", "$$\\huge{\\neq}$$")),
                 
                 numericInput("hypval", "Hypothesized Value:", 5),
                 
                 radioButtons("alpha", "Choose a significance level:",
                              choices = c("0.01", "0.05", "0.1")),
                 
                 fluidRow(
                   column(1),
                 actionButton("update", "Update Simulation!"),
                 tags$head(tags$style("#update{color: white;
                                      background-color: mediumseagreen;
                                      text-align: center;
                                      }"
                         )
                 ),
                 column(1)
                 ),
                 br(),
                 h5("Current Description of Scenario:"),
                 uiOutput("reactext"),
                 br(),
                 uiOutput("reach0"),
                 br(),
                 uiOutput("reacha"),
                 br(),
                 uiOutput("reacalpha")
                 
                 ) # Close sidebarPanel
    ), # close div for sidebarPanel
    
    mainPanel(width = 9,
  
  navbarPage("", id = "tabs",
             
             # Overview, explanation of data/app
             
             tabPanel("Overview",
                      h2("Our App", align = "center"),
            
                      h4("Welcome to Duke University's hypothesis testing app! Here, you can learn about the crucial aspects of conducting and interpreting a hypothesis test, including setting your hypotheses, calculating a test statistic and p-value, making your conclusions, and navigating types of possible errors in your test.", align = "center"),
                      br(),
                      h4("Often in research and data analysis, we want to investigate certain new claims that challenge what is currently believed, so we take some sample data and see whether or not it supports our new claim. However, as there can be significant variation in sample data, we may not know if the evidence our data provide is because it truly supports our new claim or if it was just the result of random fluctuation in the sample. Hypothesis testing is a reliable way to take sample data and test how strongly its evidence supports our new claim -- if the evidence is strong, we can reject the old beliefs and support the new; if it is weak, we cannot reject the old beliefs. To explore this procedure, take a look at the images below to see a sample pathway through our app, and make sure to check out the running example we'll be using throughout the app and how to change its parameters!", align = "center"),
                      
                      h2("Our Example", align = "center"),
                      h4("There is a commonly used drug to treat flu patients that generally helps them recover within a certain number of days on average. A new drug is being developed by a competing company, and they believe that flu patients who use their new drug will recover in a different number of days on average than they would with the current drug.", align = "center"),
                      br(),
                      h4("Suppose the current drug makes patients recover in about 5 days on average, and the new company says that their drug will help patients recover in less than 5 days on average. We experiment with giving patients the new drug, and our hypotheses are:", align = "center"),
                      br(),
                      helpText("Null hypothesis: Patient recovery time is greater than or equal to five days.     $$\\huge{H_0 \\geq 5}$$", align = "center"),
                      helpText("Alternative hypothesis: Patient recovery time is less than five days.     $$\\huge{H_A < 5}$$", align = "center"),
                      br(),
                      h4("You can test these hypotheses (and find explanations of the hypotheses themselves) in the following tabs. You can also use the sidebar on the tabs to change the parameters -- you can choose the type of test, the hypothesized value, and the level of significance to see how these affect the test and your conclusions."),
                      br()
                     
             ),
             
             # Compare by State
             
             tabPanel("Null and Alternative Hypotheses",
                      
                      tags$head(tags$style("#tab11{font-size: 17px;
                                           } #conditions{
                                           font-size: 15px;
                                           } #tab12{
                                           font-size: 17px
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
                                           }"
                         )
                      ),
                      
                      # Title/subtitle
                      
                      h2("The Null Hypothesis ($\\huge{H_0}$) and Alternative Hypothesis ($\\huge{H_A}$)", align = "center"),
                     
                      
                      textOutput("tab11"),
                      br(),
                      uiOutput("conditions"),
                      br(),
                      textOutput("tab12"),
                      br(),
                      uiOutput("typehyp"),
                      
                      br(),
                      
                      p(textOutput("choose1", inline = TRUE), span(textOutput("choosealt", inline = TRUE), style = "color:cornflowerblue; font-size:17px"), textOutput("choose3", inline = TRUE), span(textOutput("choosenull", inline = TRUE), style = "color:cornflowerblue"), textOutput("choose4", inline = TRUE), span(textOutput("tab1blue", inline = TRUE), style = "color:blue; font-size:17px"), textOutput("choose5", inline = TRUE), span(textOutput("tab1red", inline = TRUE), style = "color:red; font-size:17px"), textOutput("choose6", inline = TRUE) ),
                      
                      bsPopover(id = "choosealt", 'The alternative hypothesis is the new claim we are testing. It states that there is some new effect, some change that has occurred in the experiment. There is something out of the ordinary that we would not usually expect if the same-old status quo was being upheld.', trigger = "hover"),
                      
                      bsPopover(id = "choosenull", 'The null hypothesis is generally referred to as the "status quo". It is the claim that is currently generally accepted and believed, or it is the claim of no change -- nothing is going on, there are no effects, nothing is out of the ordinary or different from what we expect.', trigger = "hover"),

                      plotOutput("nullaltplot"),
                      
                      textOutput("tab13"),
                      br(),
                      textOutput("tab14"),
                      br(),
                      uiOutput("conc"),
                      br(),
                      textOutput("tab15"),
                      br()
                      
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
        ) # close div for sideBarLayout
     ) # close UI



server <- function(input, output) {
  
  # Sidebar Text
  output$helpt <- renderText({ c("Hover for Instructions!") })
  
  param <- reactiveValues(type = "$$\\huge{<}$$",
                          hyp = 5,
                          alpha = 0.01)
  observeEvent(input$update, {
    param$type <- input$typetest
  })
  observeEvent(input$update, {
    param$hyp <- as.numeric(input$hypval)
  })
  observeEvent(input$update, {
    param$alpha <- as.numeric(input$alpha)
  })
  output$reactext <- renderUI({
    if(param$type == "$$\\huge{<}$$"){
    withMathJax(
    paste("The current drug makes patients recover in ", param$hyp, " days on average, and the new company says that their drug will help patients recover in less than ", param$hyp, " days on average. Our hypotheses are:")
    )
    } else if(param$type == "$$\\huge{>}$$"){
      withMathJax(
        paste("The current drug makes patients recover in ", param$hyp, " days on average, and the new company says that their drug will help patients recover in more than ", param$hyp, " days on average. Our hypotheses are:")
      )
    } else{
      
      withMathJax(
        paste("The current drug makes patients recover in ", param$hyp, " days on average, and the new company says that their drug will help patients recover in a different number of days than ", param$hyp, " days on average. Our hypotheses are:")
      )
    }
  })
  
  output$reach0 <- renderUI({
    if(param$type == "$$\\huge{<}$$"){
      withMathJax(
        paste("Null hypothesis: Patient recovery time is greater than or equal to ", param$hyp, " days. $\\huge{H_0: \\mu \\geq}$", param$hyp)
      )
    } else if(param$type == "$$\\huge{>}$$"){
      withMathJax(
        paste("Null hypothesis: Patient recovery time is less than or equal to ", param$hyp, " days. $\\huge{H_0: \\mu \\leq}$", param$hyp)
      )
    } else{
      
      withMathJax(
        paste("Null hypothesis: Patient recovery time is equal to ", param$hyp, " days. $\\huge{H_0: \\mu =}$", param$hyp)
      )
    }
  })
  
  output$reacha <- renderUI({
    if(param$type == "$$\\huge{<}$$"){
      withMathJax(
        paste("Alternative hypothesis: Patient recovery time is less than ", param$hyp, " days. $\\huge{H_A: \\mu <}$", param$hyp)
      )
    } else if(param$type == "$$\\huge{>}$$"){
      withMathJax(
        paste("Alternative hypothesis: Patient recovery time is greater than ", param$hyp, " days. $\\huge{H_A: \\mu >}$", param$hyp)
      )
    } else{
      
      withMathJax(
        paste("Alternative hypothesis: Patient recovery time is not equal to ", param$hyp, " days. $\\huge{H_A: \\mu \\neq}$", param$hyp)
      )
    }
  })
  
  output$reacalpha <- renderUI({

      withMathJax(
        paste("We have chosen a significance level ($\\huge{\\alpha}$), or P(Type I Error), of ", param$alpha, ".")
      )

  })
  
  # Text for Null and Alternative Hypotheses Tab 
  output$tab11 <- renderText({ c("When performing a hypothesis test, we must start with our claims, or hypotheses. There are two main criteria to form proper hypotheses: ")})
  output$conditions <- renderUI(HTML("<ul><li>The two hypotheses must be mutually exclusive (i.e., they cannot both be true at the same time)</li><li>Together, they must cover the entire range of possibilities</li></ul>"))
  output$tab12 <- renderText({ c("Thus, there are three possible sets of hypotheses, where $\\huge{h}$ is the hypothesized value: ")})
  output$typehyp <- renderUI(withMathJax(HTML("<ul><li>$\\huge{H_0}$: $\\huge{\\mu \\leq h}$, $\\huge{H_A}$: $\\huge{\\mu > h}$</li><li>$\\huge{H_0}$: $\\huge{\\mu \\geq h}$, $\\huge{H_A}$: $\\huge{\\mu < h}$</li><li>$\\huge{H_0}$: $\\huge{\\mu = h}$, $\\huge{H_A}$: $\\huge{\\mu \\neq h}$</li></ul>")))
  output$choose1 <- renderText({ c("Use the sidebar to choose an ")})
  output$choosealt <- renderText({ c("alternative hypothesis")})
  output$choose3 <- renderText({ c(", which also determines the ")})
  output$choosenull <- renderText({ c("null hypothesis")})
  output$choose4 <- renderText({ c(", and see how the plotted null (")})
  output$tab1blue <- renderText({ c("blue")})
  output$choose5 <- renderText({ c(") and alternative (")})
  output$tab1red <- renderText({ c("red")})
  output$choose6 <- renderText({ c(") distributions change.")})
  output$tab13 <- renderText({ c("Interpreting Conclusions")})
  output$tab14 <- renderText({ c('When we perform the test, it is important to remember that no matter what our conclusions are, we have not "proved" either claim to be indisputably true; we have simply observed a sample dataset that either has strong evidence against the status quo or does not. We will either: ')})
  output$conc <- renderUI(withMathJax(HTML("<ul><li>Reject the null hypothesis, meaning that the evidence was strong enough for us to reject $\\huge{H_0}$ and be in favor of $\\huge{H_A}$, or</li><li>Fail to reject the null hypothesis, meaning that the evidence was not strong enough to support $\\huge{H_A}$ and reject $\\huge{H_0}$.</li></ul>")))
  output$tab15 <- renderText({ c("Neither hypothesis is definitely proved, confirmed, or disproved -- the test will only tell us which conclusion this dataset supports.")})
  
  
  # Hige sidebar on Overview and Review Quiz tabs
   observeEvent(input$tabs, {
     if(input$tabs == "Overview" || input$tabs == "Review Quiz"){
         shinyjs::hide(id = "Sidebar")

         } else{
           shinyjs::show(id = "Sidebar")
         }
   })
  
  # Ensure that mainPanel expands to full screen when sidebar is hidden in Overview/Review Quiz tabs
  observe({
    if (input$tabs %in% c("Type I and Type II Errors", "Test Statistic and P-Value", "Null and Alternative Hypotheses")) {
      shinyjs::show(select = "#sblay > div > div:nth-child(1)")
      shinyjs::removeClass(class = "fullwidth",
                           select = "#sblay > div > div:nth-child(2)")
    } else {
      shinyjs::hide(select = "#sblay > div > div:nth-child(1)")
      shinyjs::addClass(class = "fullwidth",
                        select = "#sblay > div > div:nth-child(2)")
    }
    
  })
  
  theme_bluewhite <- function (base_size = 11, base_family = "") {
    theme_bw() %+replace% 
      theme(
        panel.grid.major  = element_line(color = "white"),
        panel.background = element_rect(fill = "lightcyan2"),
        panel.border = element_rect(color = "lightblue", fill = NA),
        axis.line = element_line(color = "lightblue"),
        axis.ticks = element_line(color = "lightblue"),
        axis.text = element_text(color = "steelblue", size = 20)
      )
  }
  
  nullvals <- rnorm(1000, 5, 1)
  nulldens <- dnorm(nullvals, 5, 1)
  nulldist <- data.frame(vals = nullvals, dens = nulldens)
  
  altlessvals <- rnorm(1000, 3, 1)
  altlessdens <- dnorm(altlessvals, 3, 1)
  altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)
  
  altmorevals <- rnorm(1000, 7, 1)
  altmoredens <- dnorm(altmorevals, 7, 1)
  altmoredist <- data.frame(vals = altmorevals, dens = altmoredens)
  
  
  
  nulldist <- eventReactive(input$update, {
    nullvals <- rnorm(1000, as.numeric(input$hypval), 1)
    nulldens <- dnorm(nullvals, as.numeric(input$hypval), 1)
    data.frame(vals = nullvals, dens = nulldens)
  })
  
  altlessdist <- eventReactive(input$update, {
    
      altlessvals <- rnorm(1000, (as.numeric(input$hypval) - 3), 1)
      altlessdens <- dnorm(altlessvals, (as.numeric(input$hypval) - 3), 1)
      altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)
    
  })
  
  altmoredist <- eventReactive(input$update, {
    
      altmorevals <- rnorm(1000, (as.numeric(input$hypval) + 3), 1)
      altmoredens <- dnorm(altmorevals, (as.numeric(input$hypval) + 3), 1)
      altmoredist <- data.frame(vals = altmorevals, dens = altmoredens)
    
  })
  
  plotdata <- reactive({
    data.frame(nullvals = nulldist()$vals, nulldens = nulldist()$dens, altlessvals = altlessdist()$vals, altlessdens = altlessdist()$dens, altmorevals = altmoredist()$vals, altmoredens = altmoredist()$dens)
  })

  output$nullaltplot <- renderPlot({
    
    if(input$update[1] == 0){
      
      nullvals <- rnorm(1000, 5, 1)
      nulldens <- dnorm(nullvals, 5, 1)
      nulldist <- data.frame(vals = nullvals, dens = nulldens)
      
      altlessvals <- rnorm(1000, 3, 1)
      altlessdens <- dnorm(altlessvals, 3, 1)
      altlessdist <- data.frame(vals = altlessvals, dens = altlessdens)
      
      plotdata1 <- data.frame(nullvals = nulldist$vals, nulldens = nulldist$dens, altlessvals = altlessdist$vals, altlessdens = altlessdist$dens)
      
      ggplot(data = plotdata1, aes(x = nullvals, y = nulldens)) +
        geom_line(col = "blue", size = 1.5) +
        geom_line(data = plotdata1, aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
        labs(title = "Plotting The Null and Alternative Distributions", x = "", y = "") +
        scale_x_discrete(limits = c(5)) +
        theme_bluewhite() +
        theme(axis.text.y = element_blank()) 
   
       } else if(isolate({input$typetest == "$$\\huge{<}$$"})){

       ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
         geom_line(col = "blue", size = 1.5) +
         geom_line(data = plotdata(), aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
          labs(title = "Plotting The Null and Alternative Distributions", x = "", y = "") +
          scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank())

      } else if(isolate({input$typetest == "$$\\huge{>}$$"})){

        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          geom_line(data = plotdata(), aes(x = altmorevals, y = altmoredens), col = "red", linetype = "dashed") +
          labs(title = "Plotting The Null and Alternative Distributions", x = "", y = "") +
          scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank()) 

      } else{

        ggplot(data = plotdata(), aes(x = nullvals, y = nulldens)) +
          geom_line(col = "blue", size = 1.5) +
          geom_line(data = plotdata(), aes(x = altlessvals, y = altlessdens), col = "red", linetype = "dashed") +
          geom_line(data = plotdata(), aes(x = altmorevals, y = altmoredens), col = "red", linetype ="dashed") +
          labs(title = "Plotting The Null and Alternative Distributions", x = "", y = "") +
          scale_x_discrete(limits = c(isolate({as.numeric(input$hypval)}))) +
          theme_bluewhite() +
          theme(axis.text.y = element_blank())
      }

  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

