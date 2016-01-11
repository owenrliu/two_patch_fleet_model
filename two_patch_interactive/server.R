library(shiny)

source(file='two_patch_fxns_shiny.R')
paramNames <- c('max.age.1','Linf.1','K.1','t0.1','mort.1','repr.age.1','gamma.1',
                'fert1.1','fert2.1','fish.1','L50.1','L95.1','lw.a.1','lw.b.1','a.mu.1',
                'max.age.2','Linf.2','K.2','t0.2','mort.2','repr.age.2','gamma.2','fert1.2','fert2.2',
                'fish.2','L50.2','L95.2','lw.a.2','lw.b.2','a.mu.2','larvala.mu.1',
                'adulta.mu.1','larvala.mu.2','adulta.mu.2')

#
shinyServer(function(input, output) {
  
  getParams <- function() {
    params <- lapply(paramNames,function(p) input[[p]])
    names(params) <- paramNames
    params
  }
  params <- reactive(getParams())
  simdat <- reactive(do.call(sim.2pops,getParams()))
  
  # Parameter visualization plot outputs
  # Population 1
  output$length.1 <- renderPlot({
    p=params()
    plotparams(type='Length',vb.growth(age.vec=(0:p$max.age.1),vb=c(p$Linf.1,p$K.1,p$t0.1)))
  })
  output$select.1 <- renderPlot({
    p=params()
    plotparams(type='Selectivity',selectivity.at.length(age.vec=0:p$max.age.1,vb=c(p$Linf.1,p$K.1,p$t0.1),L50=p$L50.1,L95=p$L95.1))
  })
  output$surv.1 <- renderPlot({
    p=params()
    q.vec<-selectivity.at.length(age.vec=0:p$max.age.1,vb=c(p$Linf.1,p$K.1,p$t0.1),L50=p$L50.1,L95=p$L95.1)
    plotparams(type='Survival',surv.fxn(fish=p$fish.1,q.vec=q.vec,mort=p$mort.1))
  })
  output$fecun.1 <- renderPlot({
    p=params()
    plotparams(type='Fecundity',fert(age.vec=0:p$max.age.1, vb=c(p$Linf.1,p$K.1,p$t0.1), repr.age=p$repr.age.1,
                    fert1=p$fert1.1,fert2=p$fert2.1))
  })
  output$ageweight.1 <- renderPlot({
    p=params()
    plotparams(type='Weight',weight.at.age(vb=c(p$Linf.1,p$K.1,p$t0.1),age.vec=0:p$max.age.1,
                             lw.a=p$lw.a.1,lw.b=p$lw.b.1))
  })
  # Population 2
  output$length.2 <- renderPlot({
    p=params()
    plotparams(type='Length',vb.growth(age.vec=0:p$max.age.2,vb=c(p$Linf.2,p$K.2,p$t0.2)))
  })
  output$select.2 <- renderPlot({
    p=params()
    plotparams(type='Selectivity',selectivity.at.length(age.vec=0:p$max.age.2,vb=c(p$Linf.2,p$K.2,p$t0.2),L50=p$L50.2,L95=p$L95.2))
  })
  output$surv.2 <- renderPlot({
    p=params()
    q.vec<-selectivity.at.length(age.vec=0:p$max.age.2,vb=c(p$Linf.2,p$K.2,p$t0.2),L50=p$L50.2,L95=p$L95.2)
    plotparams(type='Survival',surv.fxn(fish=p$fish.2,q.vec=q.vec,mort=p$mort.2))
  })
  output$fecun.2 <- renderPlot({
    p=params()
    plotparams(type='Fecundity',fert(age.vec=0:p$max.age.2, vb=c(p$Linf.2,p$K.2,p$t0.2), repr.age=p$repr.age.2,
                    fert1=p$fert1.2,fert2=p$fert2.2))
  })
  output$ageweight.2 <- renderPlot({
    p=params()
    plotparams(type='Weight',weight.at.age(vb=c(p$Linf.2,p$K.2,p$t0.2),age.vec=0:p$max.age.2,
                             lw.a=p$lw.a.2,lw.b=p$lw.b.2))
  })
  
  # Simulation output plots
  output$Radj <- renderPlot({
    plotRadj(simdat())
  })
  output$eigens <- renderPlot({
    ploteigens(simdat())
  })
  output$yield <- renderPlot({
    plotyield(simdat())
  })
  output$abun <- renderPlot({
    plotabun(simdat())
  })
  output$totpop <- renderPlot({
    plottotpop(simdat())
  })
})
  # shinyServer(function(input, output, session) {
  #   
  #   getParams <- function(prefix) {
  #     input[[paste0(prefix, "_recalc")]]
  #     
  #     params <- lapply(paramNames, function(p) {
  #       input[[paste0(prefix, "_", p)]]
  #     })
  #     names(params) <- paramNames
  #     params
  #   }
  #   
  #   # Function that generates scenarios and computes NAV. The expression
  #   # is wrapped in a call to reactive to indicate that:
  #   #
  #   #  1) It is "reactive" and therefore should be automatically
  #   #     re-executed when inputs change
  #   #
  #   navA <- reactive(do.call(simulate_nav, getParams("a")))
  #   navB <- reactive(do.call(simulate_nav, getParams("b")))
  #   
  #   # Expression that plot NAV paths. The expression
  #   # is wrapped in a call to renderPlot to indicate that:
  #   #
  #   #  1) It is "reactive" and therefore should be automatically
  #   #     re-executed when inputs change
  #   #  2) Its output type is a plot
  #   #
  #   output$a_distPlot <- renderPlot({
  #     plot_nav(navA())
  #   })
  #   output$b_distPlot <- renderPlot({
  #     plot_nav(navB())
  #   })
  #   
  # })