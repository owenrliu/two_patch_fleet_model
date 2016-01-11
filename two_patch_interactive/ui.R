library(shiny)

shinyUI(navbarPage("Two-Patch Model",
                   #Parameter entry tab
                   tabPanel("Parameters",
                            fluidRow(
                              column(6,h3("Population 1")),
                              column(6,h3("Population 2"))
                            ),
                            column(6,
                              wellPanel(
                                fluidRow(
                                  column(6,
                                         numericInput("max.age.1",label="Maximum Age",value=14),
                                         helpText(h4("Von-Bertalanffy Growth Parameters")),
                                         numericInput("Linf.1", label = ("Asymptotic length"), value = 52.2),
                                         numericInput("K.1", label = ("Growth Rate K"), value = 0.354),
                                         numericInput("t0.1", label = ("t0"), value = -0.766),
                                         helpText(h4("Mortality and Survival")),
                                         numericInput("mort.1", label = "Natural Mortality", value = 0.137),
                                         sliderInput("fish.1", label = ("Fishing Intensity"),min=0,max=3,step=0.1,value = 0),
                                         helpText(h4("Length-Weight Relationship")),
                                         numericInput("lw.a.1",label="Length-weight a",value=0.0105),
                                         numericInput("lw.b.1",label="Length-weight b",value=3.11)
                                         ),
                                  column(6,
                                        helpText(h4("Reproduction")),
                                        numericInput("repr.age.1", label = "Reproductive age", value = 2),
                                        numericInput("fert1.1", label = "f1", value = 0.0129),
                                        numericInput("fert2.1", label = "f2", value = 3.03),
                                        helpText(h4("Selectivity")),
                                        numericInput("L50.1",label="Length at 50% selectivity",value=25),
                                        numericInput("L95.1",label="Length at 95% selectivity",value=40),
                                        helpText(h4("Larval stage")),
                                        numericInput("a.mu.1",label="Self-recruitment rate (0 to 1)",value=.0005),
                                        numericInput("gamma.1",label="First year larval survival",value=0.125),
                                        numericInput("larvala.mu.1",label="Recruitment rate to Population 2",value=.0005),
                                        helpText(h4("Adult movement")),
                                        numericInput("adulta.mu.1",label="Adult annual movement rate",value=0)
                                        )
                                  )
                                )),
                            column(6,
                             wellPanel(
                               fluidRow(
                                 column(6,
                                        numericInput("max.age.2",label="Maximum Age",value=14),
                                        helpText(h4("Von-Bertalanffy Growth Parameters")),
                                        numericInput("Linf.2", label = ("Asymptotic length"), value = 52.2),
                                        numericInput("K.2", label = ("Growth Rate K"), value = 0.354),
                                        numericInput("t0.2", label = ("t0"), value = -0.766),
                                        helpText(h4("Mortality and Survival")),
                                        numericInput("mort.2", label = "Natural Mortality", value = 0.137),
                                        sliderInput("fish.2", label = ("Fishing Intensity"),min=0,max=3,step=0.1,value = 0),
                                        helpText(h4("Length-Weight Relationship")),
                                        numericInput("lw.a.2",label="Length-weight a",value=0.0105),
                                        numericInput("lw.b.2",label="Length-weight b",value=3.11)
                                 ),
                                 column(6,
                                        helpText(h4("Reproduction")),
                                        numericInput("repr.age.2", label = "Reproductive age", value = 2),
                                        numericInput("fert1.2", label = "f1", value = 0.0129),
                                        numericInput("fert2.2", label = "f2", value = 3.03),
                                        helpText(h4("Selectivity")),
                                        numericInput("L50.2",label="Length at 50% selectivity",value=25),
                                        numericInput("L95.2",label="Length at 95% selectivity",value=40),
                                        helpText(h4("Larval stage")),
                                        numericInput("a.mu.2",label="Self-recruitment rate (0 to 1)",value=.0005),
                                        numericInput("gamma.2",label="First year larval survival",value=0.125),
                                        numericInput("larvala.mu.2",label="Recruitment rate to Population 2",value=.0005),
                                        helpText(h4("Adult movement")),
                                        numericInput("adulta.mu.2",label="Adult annual movement rate",value=0)
                                 )
                               )
                            )),
                            #Run or re-run the simulation
                            helpText(h3("Must push this button to update results")),
                            submitButton(text = "Run Simulation", icon = icon('random'))
                   ),
                   # Parameter visualization tab, with sidebar to choose population
                   tabPanel("Growth and Mortality",
                            navlistPanel(
                              "Choose Population",
                              "-----",
                              widths=c(2,10),
                              tabPanel("Population 1",
                                       fluidRow(
                                         column(4,plotOutput("length.1"),offset=1),
                                         column(4,plotOutput("select.1"),offset=2)
                                       ),
                                       fluidRow(
                                         column(4,plotOutput("surv.1")),
                                         column(4,plotOutput("fecun.1")),
                                         column(4,plotOutput("ageweight.1"))
                                       )
                              ),
                              tabPanel("Population 2",
                                       fluidRow(
                                         column(4,plotOutput("length.2"),offset=1),
                                         column(4,plotOutput("select.2"),offset=2)
                                       ),
                                       fluidRow(
                                         column(4,plotOutput("surv.2")),
                                         column(4,plotOutput("fecun.2")),
                                         column(4,plotOutput("ageweight.2"))
                                       )
                              )
                            )),
                   # Output of simulations, with sidebar choices for which graphs to view
                   tabPanel("Simulation Outputs",
                            navlistPanel(
                              "Simulation Results",
                              "-----",
                              widths=c(2,10),
                              tabPanel("Net Reproductive Rate",
                                       plotOutput('Radj',height = '800px')),
                              tabPanel("Eigen Values",
                                       plotOutput('eigens',height = '800px')),
                              tabPanel("Yield",
                                       plotOutput('yield',height = '800px')),
                              tabPanel("Abundance",
                                       plotOutput('abun',height = '800px')),
                              "-----",
                              tabPanel("Entire Population",
                                       plotOutput('totpop',height = '800px'))
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
