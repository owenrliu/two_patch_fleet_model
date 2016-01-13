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
  output$length<- renderPlot({
    p=params()
    vec1<-vb.growth(age.vec=(0:p$max.age.1),vb=c(p$Linf.1,p$K.1,p$t0.1))
    vec2<-vb.growth(age.vec=0:p$max.age.2,vb=c(p$Linf.2,p$K.2,p$t0.2))
    plotparams(type='Length',vec1=vec1,vec2=vec2)
  })
  output$select<- renderPlot({
    p=params()
    vec1<-selectivity.at.length(age.vec=0:p$max.age.1,vb=c(p$Linf.1,p$K.1,p$t0.1),L50=p$L50.1,L95=p$L95.1)
    vec2<-selectivity.at.length(age.vec=0:p$max.age.2,vb=c(p$Linf.2,p$K.2,p$t0.2),L50=p$L50.2,L95=p$L95.2)
    plotparams(type='Selectivity',vec1=vec1,vec2=vec2)
  })
  output$surv <- renderPlot({
    p=params()
    q.vec1<-selectivity.at.length(age.vec=0:p$max.age.1,vb=c(p$Linf.1,p$K.1,p$t0.1),L50=p$L50.1,L95=p$L95.1)
    q.vec2<-selectivity.at.length(age.vec=0:p$max.age.2,vb=c(p$Linf.2,p$K.2,p$t0.2),L50=p$L50.2,L95=p$L95.2)
    vec1<-surv.fxn(fish=p$fish.1,q.vec=q.vec1,mort=p$mort.1)
    vec2<-surv.fxn(fish=p$fish.2,q.vec=q.vec2,mort=p$mort.2)
    plotparams(type='Survival',vec1=vec1,vec2=vec2)
  })
  output$fecun<- renderPlot({
    p=params()
    vec1<-fert(age.vec=0:p$max.age.1, vb=c(p$Linf.1,p$K.1,p$t0.1), repr.age=p$repr.age.1,
               fert1=p$fert1.1,fert2=p$fert2.1)
    vec2<-fert(age.vec=0:p$max.age.2, vb=c(p$Linf.2,p$K.2,p$t0.2), repr.age=p$repr.age.2,
               fert1=p$fert1.2,fert2=p$fert2.2)
    plotparams(type='Fecundity',vec1=vec1,vec2=vec2)
  })
  output$ageweight<- renderPlot({
    p=params()
    vec1<-weight.at.age(vb=c(p$Linf.1,p$K.1,p$t0.1),age.vec=0:p$max.age.1,
                        lw.a=p$lw.a.1,lw.b=p$lw.b.1)
    vec2<-weight.at.age(vb=c(p$Linf.2,p$K.2,p$t0.2),age.vec=0:p$max.age.2,
                        lw.a=p$lw.a.2,lw.b=p$lw.b.2)
    plotparams(type='Weight',vec1=vec1,vec2=vec2)
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