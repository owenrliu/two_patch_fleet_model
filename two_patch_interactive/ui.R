library(shiny)

shinyUI(navbarPage("Two-Patch Model",
                   #Parameter entry tab
                   tabPanel("Parameters",
                            helpText(h4("Pay attention to default parameters. Although parameters can and should be 
                                        re-specified manually, parameters far outside their usual values or of inappropriate
                                        form for their respective expressions may cause the model to work incorrectly.  See
                                        descriptive RMarkdown file. Additionally, every time parameters are changed, be
                                        sure to push the 'Run Simulation' button at the bottom of this tab.")),
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
                                         numericInput("K.1", label = ("Growth Rate K"), step=0.01,value = 0.354),
                                         numericInput("t0.1", label = ("t0"), step=0.01,value = -0.766),
                                         helpText(h4("Mortality and Survival")),
                                         numericInput("mort.1", label = "Natural Mortality", value = 0.137,step=0.01),
                                         sliderInput("fish.1", label = ("Fishing Intensity"),min=0,max=3,step=0.1,value = 0),
                                         helpText(h4("Length-Weight Relationship")),
                                         numericInput("lw.a.1",label="Length-weight a",value=0.0105,step=0.01),
                                         numericInput("lw.b.1",label="Length-weight b",value=3.11,step=0.01)
                                         ),
                                  column(6,
                                        helpText(h4("Reproduction")),
                                        numericInput("repr.age.1", label = "Reproductive age", value = 2),
                                        numericInput("fert1.1", label = "f1", value = 0.0129,step=0.01),
                                        numericInput("fert2.1", label = "f2", value = 3.03,step=0.01),
                                        helpText(h4("Selectivity")),
                                        numericInput("L50.1",label="Length at 50% selectivity",value=25),
                                        numericInput("L95.1",label="Length at 95% selectivity",value=40),
                                        helpText(h4("Larval stage")),
                                        numericInput("a.mu.1",label="Self-recruitment rate (0 to 1)",value=.0005,step=0.0005),
                                        numericInput("gamma.1",label="First year larval survival",value=0.125,step=0.01),
                                        numericInput("larvala.mu.1",label="Recruitment rate from Population 2",value=.0005,step=0.0005),
                                        helpText(h4("Adult movement")),
                                        numericInput("adulta.mu.1",label="Adult annual movement rate",value=0,step=0.0005)
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
                                        numericInput("K.2", label = ("Growth Rate K"), value = 0.354,step=0.01),
                                        numericInput("t0.2", label = ("t0"), value = -0.766,step=0.01),
                                        helpText(h4("Mortality and Survival")),
                                        numericInput("mort.2", label = "Natural Mortality", value = 0.137,step=0.01),
                                        sliderInput("fish.2", label = ("Fishing Intensity"),min=0,max=3,step=0.1,value = 0),
                                        helpText(h4("Length-Weight Relationship")),
                                        numericInput("lw.a.2",label="Length-weight a",value=0.0105,step=0.01),
                                        numericInput("lw.b.2",label="Length-weight b",value=3.11,step=0.01)
                                 ),
                                 column(6,
                                        helpText(h4("Reproduction")),
                                        numericInput("repr.age.2", label = "Reproductive age", value = 2),
                                        numericInput("fert1.2", label = "f1", value = 0.0129,step=0.01),
                                        numericInput("fert2.2", label = "f2", value = 3.03,step=0.01),
                                        helpText(h4("Selectivity")),
                                        numericInput("L50.2",label="Length at 50% selectivity",value=25),
                                        numericInput("L95.2",label="Length at 95% selectivity",value=40),
                                        helpText(h4("Larval stage")),
                                        numericInput("a.mu.2",label="Self-recruitment rate (0 to 1)",value=.0005,step=0.0005),
                                        numericInput("gamma.2",label="First year larval survival",value=0.125,step=0.01),
                                        numericInput("larvala.mu.2",label="Recruitment rate from Population 1",value=.0005,step=0.0005),
                                        helpText(h4("Adult movement")),
                                        numericInput("adulta.mu.2",label="Adult annual movement rate",value=0,step=0.0005)
                                 )
                               )
                            )),
                            hr(),
                            #Run or re-run the simulation
                            helpText(h3("Must push this button to update results")),
                            submitButton(text = "Run Simulation", icon = icon('random')),
                            br(),
                            br()
                   ),
                   # Parameter visualization tab, with sidebar to choose population
                   tabPanel("Growth and Mortality",
                            fluidRow(
                              column(4,plotOutput("length"),offset=1),
                              column(4,plotOutput("select"),offset=2)
                              ),
                            fluidRow(
                              column(4,plotOutput("surv")),
                              column(4,plotOutput("fecun")),
                              column(4,plotOutput("ageweight"))
                            )),
                   # Output of simulations, with sidebar choices for which graphs to view
                   tabPanel("Simulation Outputs",
                            navlistPanel(
                              "Simulation Results",
                              "-----",
                              widths=c(2,10),
                              tabPanel("Net Reproductive Rate",
                                       helpText("The net reproductive rate is defined as the expected lifetime 
                                                reproductive contribution from an individual that survives the first 
                                                year. If this value is less than 1, we can expect than the population will shrink 
                                                because an individual is not expected to replace itself (a sink population)."),
                                       plotOutput('Radj',height = '800px')),
                              tabPanel("Eigen Values",
                                       helpText("Similar to the net reproductive rate, we can calculate the eigen
                                                values of the population projection matrix. If the eigen value is less
                                                than 1, the population will shrink. If it is greater than 1, the population
                                                will grow."),
                                       plotOutput('eigens',height = '800px')),
                              tabPanel("Yield",
                                       helpText("Yield measures the biomass (weight) of fish captured by the fishery in
                                                each year of the simulation.  Keep in mind that if the fishing intensity
                                                parameter is 0, there will be no yield, since there is no fishing!"),
                                       plotOutput('yield',height = '800px')),
                              tabPanel("Abundance",
                                       helpText("Total abundance (numbers) in all age groups over time."),
                                       plotOutput('abun',height = '800px')),
                              "-----",
                              tabPanel("Entire Population",
                                       helpText("Eigen values, yield, and total abundance for the two populations combined,
                                                over time."),
                                       plotOutput('totpop',height = '800px'))
                            ))
))
