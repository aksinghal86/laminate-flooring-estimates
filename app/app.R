
#### Libraries ####
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(minpack.lm)

#### Data ####
p1 <- read.csv('data/Phase1select.csv', stringsAsFactors = FALSE) %>% mutate(Time = 0, Phase = "Phase I")
p2 <- read.csv('data/Phase2select.csv', stringsAsFactors = FALSE) %>% mutate(Time = Time * 365.25, Phase = "Phase II")
all <- p1 %>% select(Time, Phase, ER) %>% bind_rows(p2 %>% select(Time, Phase, ER))


#### Stats and Calcs #### 
# Default parameter values from the published model
p <- 0.7079399664092242
a <- 0.01637946082640082
b <- 0.00014477698872096014

decay_model <- function(t) {p * exp(-a * t) +  (1 - p) * exp(-b * t)}

get_decay_factor <- function(t) {
  integrate(decay_model, lower = 0, upper = t)$value/t
}

home_conc <- function(ffloor, er, aer, h, decay_factor = 1) {
  return(decay_factor * ffloor * er / (aer * h))
}

#### Plotting Functions ####
default_theme <- theme_minimal() +  
  theme(
    legend.position = "bottom", 
    legend.text = element_text(size = 16), 
    axis.text = element_text(size = 16), 
    axis.title = element_text(size = 20), 
    plot.title = element_text(size = 32)
  )


decay_plot <- function(p_er0 = NA, p_tinst = NA) {
  # Duration of installation. Default is the max in the Phase II database
  default_tinst = max(all$Time)
  # Use the max of user-provided vs default duration of installation
  tinst <- 0:(max(p_tinst, default_tinst, na.rm = TRUE))
  
  default_er0 <- 61.3
  default_er_ests <- default_er0 * decay_model(tinst)
  p_er_ests <- p_er0 * decay_model(tinst)
  
  estimates <- data.frame(Tinst = tinst, Default.ER.Ests = default_er_ests, Plaintiff.ER.Ests = p_er_ests)

  ggplot() + 
    geom_point(data = all, aes(x = Time, fill = Phase, y = ER), shape = 21, size = 4, alpha = 0.2) + 
    geom_line(data = estimates, aes(x = Tinst, y = Default.ER.Ests), size = 1.1) + 
    geom_line(data = estimates, aes(x = Tinst, y = Plaintiff.ER.Ests), size = 1.1, linetype = "dashed") +
    # scale_fill_manual(values = c("turquoise2", "orange2")) + 
    # geom_point(aes(x = p_tinst, y = p_er_ests[p_tinst]), shape = 8, size = 5, color = "blue") + 
    scale_y_continuous(limits = c(0, max(150, p_er0, na.rm = TRUE))) + 
    scale_x_continuous(labels = scales::comma, breaks = seq(0, max(tinst), 500)) +
    labs(x = "Time (in days)", y = "ER (ug/m2-h)", title = "Bi-exponential decay model") + 
    default_theme + 
    theme(legend.title = element_blank())
}

#### UI ####
ui <- fluidPage(
  theme = shinytheme('readable'), 
  
  sidebarLayout(
    
    sidebarPanel(
      tags$h4("Please provide residence-specific parameters below if different from default values:"),
      tags$br(),
      numericInput('p_er0', 'Initial ER (ug/m2-h)', 61.26, min = 1, max = 1000), 
      numericInput('p_tinst', 'Duration of Installation (years)', 7, min = 1),
      numericInput('aer', 'AER (per hour)', 0.6, min = 0), 
      numericInput('ffloor', 'Fraction of flooring', 1, min = 0, max = 1), 
      numericInput('h', 'Height (m)', 2.59)
    ),
    
    mainPanel(
      # Decay model
      tags$h2("Decay Model"),
      tags$div("The model below is reproduced from Sheehan et al. (2018), which was fitted to Phase I and Phase II data, as detailed in the paper. 
               Solid line is the original model provided in Sheehan et al. (2018) based on a mean initial emission rate (ER) of 61.3 ug/m2-h. 
               Dashed line, if visible, is scaled using the residence-specific initial ER provided on the left."),
      tags$br(), 
      plotOutput(outputId = 'decay_plot'), 
      tags$br(), 
      
      # Estimates at the time of installation 
      tags$h2("Estimates at the Time of Installation"), 
      textOutput('at_inst_text'),
      tags$br(),
      tags$h4("Table 1. Home formaldehyde concentration the time of installation"),
      tableOutput('at_inst_table'),
      
      # Estimates at the time of removal or present time
      tags$h2("Estimates at the Time of Removal"), 
      textOutput('at_rem_text'), 
      tags$br(),
      tags$h4("Table 2. Home formaldehyde concentration at the time of removal"),
      tableOutput('at_rem_table'),
      
      # Time-weighted Average Estimates
      tags$h2("Time-weighted Average (TWA) Concentration"), 
      textOutput('twa_text'), 
      tags$br(),
      tags$h4("Table 3. TWA Home formaldehyde concentration"),
      tableOutput('twa_table'),
      

      tags$br(),
      tags$br(), 
      
      tags$footer("-----------------------------------------------------------------------------------------", tags$br(), 
                  tags$em("Created by:"), tags$br(), 
                  "Ankur Singhal", tags$br(), 
                  "Empirical Solutions Consulting, LLC", tags$br(), 
                  "asinghal@theempiricalsolutions.com"
                  )
      
    )
  )
)


#### Server ####
server <- function(input, output) {
  
  plaintiff_params <- reactive({
    er_0 <- input$p_er0
    ffloor <- input$ffloor
    aer <- input$aer
    h <- input$h
    conc_0 <- round(home_conc(ffloor, er_0, aer, h), 2)
    
    tinst <- input$p_tinst * 365.25
    er_t <- round(er_0 * decay_model(tinst), 2)
    conc_t <- round(home_conc(ffloor, er_t, aer, h), 2)
    
    decay_factor <- get_decay_factor(tinst)
    conc_twa <- round(home_conc(decay_factor = decay_factor, ffloor, er_0, aer, h), 2)
    
    return(list(er_0 = er_0, ffloor = ffloor, aer = aer, h = h, conc_0 = conc_0, 
                tinst = tinst, er_t = er_t, conc_t = conc_t, 
                decay_factor = decay_factor, conc_twa = conc_twa))
  })

  output$decay_plot <- renderPlot({
    decay_plot(plaintiff_params()$er_0, plaintiff_params()$tinst)
    })
  
  output$at_inst_text <- renderText({
    paste0("Formaldehyde concentration in the home at the time of installation (Table 1) ",
          "is estimated to be ", plaintiff_params()$conc_0, " ug/m3  (", round(plaintiff_params()$conc_0/1230, 4), "ppm).")
  })
  
  output$at_inst_table <- renderTable({
    data.frame(
      Parameter =  c("Initial ER", "Fraction of flooring", "AER", "Height", "Initial Concentration" ),
      Value = c(plaintiff_params()$er_0, plaintiff_params()$ffloor, plaintiff_params()$aer, plaintiff_params()$h, plaintiff_params()$conc_0), 
      Unit = c("ug/m2-h", "unitless", "per hour", "m", "ug/m3"))
  })
  
  output$at_rem_text <- renderText({
    paste0("Based on the initial ER of ", plaintiff_params()$er_0, " ug/m2-h and duration of installation of ", input$p_tinst, " years, ", 
      "ER at the time of removal (or at present time) is expected to be ", plaintiff_params()$er_t, " ug/m2-h (Table 2). Using this ER, 
      concentration in the home at the time of removal (or at present time) is estimated to be ", plaintiff_params()$conc_t, " ug/m3 (", 
      round(plaintiff_params()$conc_t/1230 , 4), "ppm).")
  })
  
  output$at_rem_table <- renderTable({
    data.frame(Parameter = c("Duration of Installation", "Final ER", "Fraction of flooring", "AER", "Height", "Final Concentration"), 
               Value = c(input$p_tinst, plaintiff_params()$er_t, plaintiff_params()$ffloor, plaintiff_params()$aer, 
                         plaintiff_params()$h, plaintiff_params()$conc_t), 
               Units = c("years", "ug/m2-h", "unitless", "per hour", "m", "ug/m3")
               )
  })
  
  output$twa_text <- renderText({
    paste0("TWA average concentrations can be estimated by calculating the area under the curve and dividing by the total duration of 
           installation. TWA concentration in this home is expected to be ", plaintiff_params()$conc_twa, " ug/m3 (", 
           round(plaintiff_params()$conc_twa/1230, 4), " ppm).")
  })
  
  output$twa_table <- renderTable({
    data.frame(Parameter = c("Duration of Installation", "Decay Factor", "TWA Concentration"), 
               Value = c(input$p_tinst, plaintiff_params()$decay_factor, plaintiff_params()$conc_twa), 
               Units = c("years", "unitless", "ug/m3")
    )
  })
  
 
}

#### Run the application ####
shinyApp(ui = ui, server = server)