library(shiny)
# renderInputs <- function(prefix) {
#     wellPanel(
#       fluidRow(
#         column(6,
#                sliderInput(paste0(prefix, "_", "n_obs"), "Number of observations (in Years):", min = 0, max = 40, value = 20),
#                sliderInput(paste0(prefix, "_", "start_capital"), "Initial capital invested :", min = 100000, max = 10000000, value = 2000000, step = 100000, pre = "$", sep = ","),
#                sliderInput(paste0(prefix, "_", "annual_mean_return"), "Annual investment return (in %):", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
#                sliderInput(paste0(prefix, "_", "annual_ret_std_dev"), "Annual investment volatility (in %):", min = 0.0, max = 25.0, value = 7.0, step = 0.1)
#         ),
#         column(6,
#                sliderInput(paste0(prefix, "_", "annual_inflation"), "Annual inflation (in %):", min = 0, max = 20, value = 2.5, step = 0.1),
#                sliderInput(paste0(prefix, "_", "annual_inf_std_dev"), "Annual inflation volatility. (in %):", min = 0.0, max = 5.0, value = 1.5, step = 0.05),
#                sliderInput(paste0(prefix, "_", "monthly_withdrawals"), "Monthly capital withdrawals:", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ","),
#                sliderInput(paste0(prefix, "_", "n_sim"), "Number of simulations:", min = 0, max = 2000, value = 200)
#         )
#       ),
#       p(actionButton(paste0(prefix, "_", "recalc"),
#                      "Re-run simulation", icon("random")
#       ))
#     )
#   }
shinyUI(navbarPage("Two-Patch Model",
                   tabPanel("Parameters",
                            fluidRow(
                              column(6,h3("Population 1")),
                              column(6,h3("Population 2"))
                            ),
                            wellPanel(
                              fluidRow(
                                column(6,'params1'),
                                column(6,'params2')
                              )
                            )),
                   tabPanel("Growth and Mortality",
                            navlistPanel(
                              "Choose Population",
                              "-----",
                              widths=c(2,10),
                              tabPanel("Population 1",
                                       fluidRow(
                                         column(4,"Length",offset=1),
                                         column(4,"Select",offset=2)
                                       ),
                                       fluidRow(
                                         column(4,"Surv"),
                                         column(4,"Fecun"),
                                         column(4,"Age-W")
                                       )
                              ),
                              tabPanel("Population 2",
                                       h3("This is the second pop"))
                            )),
                   tabPanel("Simulation Outputs",
                            navlistPanel(
                              "Simulation Results",
                              "-----",
                              widths=c(2,10),
                              tabPanel("Net Reproductive Rate",
                                       h3("This is Radj")),
                              tabPanel("Eigen Values",
                                       h3("This is Eigen")),
                              tabPanel("Yield",
                                       h3("this is yield")),
                              tabPanel("Abundance",
                                       h3("This is abund")),
                              "-----",
                              tabPanel("Entire Population",
                                       h3("this is everything"))
                            ))
))

# shinyUI(navbarPage("My Application",
#                    tabPanel("Component 1"),
#                    tabPanel("Component 2"),
#                    navbarMenu("More",
#                               tabPanel("Sub-Component A"),
#                               tabPanel("Sub-Component B"))
# ))
# renderInputs <- function(prefix) {
#   wellPanel(
#     fluidRow(
#       column(6,
#              sliderInput(paste0(prefix, "_", "n_obs"), "Number of observations (in Years):", min = 0, max = 40, value = 20),
#              sliderInput(paste0(prefix, "_", "start_capital"), "Initial capital invested :", min = 100000, max = 10000000, value = 2000000, step = 100000, pre = "$", sep = ","),
#              sliderInput(paste0(prefix, "_", "annual_mean_return"), "Annual investment return (in %):", min = 0.0, max = 30.0, value = 5.0, step = 0.5),
#              sliderInput(paste0(prefix, "_", "annual_ret_std_dev"), "Annual investment volatility (in %):", min = 0.0, max = 25.0, value = 7.0, step = 0.1)
#       ),
#       column(6,
#              sliderInput(paste0(prefix, "_", "annual_inflation"), "Annual inflation (in %):", min = 0, max = 20, value = 2.5, step = 0.1),
#              sliderInput(paste0(prefix, "_", "annual_inf_std_dev"), "Annual inflation volatility. (in %):", min = 0.0, max = 5.0, value = 1.5, step = 0.05),
#              sliderInput(paste0(prefix, "_", "monthly_withdrawals"), "Monthly capital withdrawals:", min = 1000, max = 100000, value = 10000, step = 1000, pre = "$", sep = ","),
#              sliderInput(paste0(prefix, "_", "n_sim"), "Number of simulations:", min = 0, max = 2000, value = 200)
#       )
#     ),
#     p(actionButton(paste0(prefix, "_", "recalc"),
#                    "Re-run simulation", icon("random")
#     ))
#   )
# }
# 
# # Define UI for application that plots random distributions
# shinyUI(fluidPage(theme="simplex.min.css",
#                   tags$style(type="text/css",
#                              "label {font-size: 12px;}",
#                              ".recalculating {opacity: 1.0;}"
#                   ),
#                   
#                   # Application title
#                   tags$h2("Retirement: simulating wealth with random returns, inflation and withdrawals"),
#                   p("An adaptation of the",
#                     tags$a(href="http://glimmer.rstudio.com/systematicin/retirement.withdrawal/", "retirement app"),
#                     "from",
#                     tags$a(href="http://systematicinvestor.wordpress.com/", "Systematic Investor"),
#                     "to demonstrate the use of Shiny's new grid options."),
#                   hr(),
#                   
#                   fluidRow(
#                     column(6, tags$h3("Scenario A")),
#                     column(6, tags$h3("Scenario B"))
#                   ),
#                   fluidRow(
#                     column(6, renderInputs("a")),
#                     column(6, renderInputs("b"))
#                   ),
#                   fluidRow(
#                     column(6,
#                            plotOutput("a_distPlot", height = "600px")
#                     ),
#                     column(6,
#                            plotOutput("b_distPlot", height = "600px")
#                     )
#                   )
# ))
