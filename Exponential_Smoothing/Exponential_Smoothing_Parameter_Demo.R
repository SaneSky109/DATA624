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
library(fpp3)
library(ggfortify)
library(forecast)


aus_economy <- global_economy %>%
    filter(Code == "AUS") %>%
    mutate(Pop = Population / 1e6)


aus_holidays <- tourism %>%
    filter(Purpose == "Holiday") %>%
    summarise(Trips = sum(Trips)/1e3)


ui <- fluidPage(

    navbarPage(theme = shinytheme("flatly"),
               "Exponetial Smoothing",
               # tab containing project information
        tabPanel("Parameters",
            fluidPage(
                includeMarkdown("Parameters.rmd")
            )
        ),
    
    
        tabPanel("Methods with Trends", 
            fluidPage(
                
                sliderInput(inputId = "alpha_value",
                            label = "Alpha:",
                            min = 0.01,
                            max = 0.99,
                            value = 0.5
                ),
                
                sliderInput(inputId = "beta_value",
                            label = "Beta (Must be less than Alpha):",
                            min = 0.01,
                            max = 0.99,
                            value = 0.5
                ),
                
                sliderInput(inputId = "phi_value",
                            label = "Phi:",
                            min = 0.01,
                            max = 0.99,
                            value = 0.5
                     
                ),
                
            plotOutput(outputId = "Trend_Plot")
                
            )
        ),
            
            
            tabPanel("Methods with Seasonality", 
                     fluidPage(
                         
                         sliderInput(inputId = "alpha_value2",
                                     label = "Alpha:",
                                     min = 0.01,
                                     max = 0.99,
                                     value = 0.5
                         ),
                         
                         sliderInput(inputId = "beta_value2",
                                     label = "Beta (Must be less than Alpha):",
                                     min = 0.01,
                                     max = 0.99,
                                     value = 0.5
                         ),
                         
                         sliderInput(inputId = "phi_value2",
                                     label = "Phi:",
                                     min = 0.01,
                                     max = 0.99,
                                     value = 0.5
                                     
                         ),
                         
                         sliderInput(inputId = "gamma_value",
                                     label = "Gamma (Must be less than 1 - Alpha):",
                                     min = 0.01,
                                     max = 0.99,
                                     value = 0.5
                                     
                         ),
                         
                         plotOutput(outputId = "Season_Plot")
                         
                     )
             
             
            ),
        
        tabPanel("Takeaways",
                 fluidPage(
                     includeMarkdown("Takeaways.rmd")
                 )
        )
        
        )
    
)





server <- function(input, output) {
    
    
    # Trend Methods
    
    aus_economy_reactive <- reactive(
        aus_economy 
            )
    
    models_reactive <- reactive({aus_economy %>%
            model(
                `Simple Exponential Smoothing` = ETS(Pop ~ error("A") +
                                          trend("N", alpha = input$alpha_value) + season("N")),
                
                `Holt's method` = ETS(Pop ~ error("A") +
                                          trend("A", alpha = input$alpha_value, beta = input$beta_value) + season("N")),
                `Damped Holt's method` = ETS(Pop ~ error("A") +
                                                 trend("Ad", alpha = input$alpha_value, beta = input$beta_value, phi = input$phi_value) + season("N"))) %>%
                    forecast(h = 15)
    })
    
    output$Trend_Plot <- renderPlot({
        ggplot(aus_economy_reactive(), aes(x=Year, y=Pop)) +
            geom_line() +
            geom_line(data = models_reactive(), aes(x=Year, y=.mean, color = as.factor(.model))) +
            ylim(10,35)
    })
    
    
    # Seasonality Methods
    
    aus_holidays <- tourism %>%
        filter(Purpose == "Holiday") %>%
        summarise(Trips = sum(Trips)/1e3)
    
    aus_holidays_reactive <- reactive({tourism %>%
        filter(Purpose == "Holiday") %>%
        summarise(Trips = sum(Trips)/1e3)})
    
    
    fc_reactive <- reactive({aus_holidays %>%
        model(
            `Additive Holt-Winters` = ETS(Trips ~ error("A") + trend("A", alpha = input$alpha_value2, beta = input$beta_value2) +
                               season("A", gamma = input$gamma_value)),
            `Multiplicative Holt-Winters` = ETS(Trips ~ error("M") + trend("A", alpha = input$alpha_value2, beta = input$beta_value2) +
                                     season("M", gamma = input$gamma_value)),

            `Holt-Winters’ damped` = ETS(Trips ~ error("M") + trend("Ad", alpha = input$alpha_value2, beta = input$beta_value2, phi = input$phi_value2) +
                             season("M", gamma = input$gamma_value))) %>% 
        forecast(h = "3 years")})
   
    output$Season_Plot <- renderPlot({
        ggplot(aus_holidays_reactive(), aes(x=Quarter, y=Trips)) +
            geom_line() +
            geom_line(data = fc_reactive(), aes(x=Quarter, y=.mean, color = as.factor(.model)))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
