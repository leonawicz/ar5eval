compositeModUI <- function(id){
  ns <- NS(id)
  tabItem(tabName=id,
    fluidRow(column(12, h4("Variable selection"))),
    fluidRow(
      column(3,
        selectInput(ns("stat"), "Error statistic", 
          c("RMSE", "RMSE (bias removed)"="RMSE0", "MAE", "MAE (bias removed)"="MAE0"), width="100%")
      ),
      column(3,
        selectInput(ns("var"), "Climate variable", 
          c("Integrated"="integrated", "Temperature"="tas", "Precipitation"="pr", "Sea level pressure"="psl"), "integrated", width="100%")
      )
    ),
    fluidRow(
      box(title="Sequential selected GCMs", plotOutput(ns("hmap_sel"), height=600), status="primary", width=6),
      box(title="Random ensebles of opportunity", plotOutput(ns("hmap_ran"), height=600), status="primary", width=6)
    )
  )
}

compositeMod <- function(input, output, session, dom0, dom, .theme){
  ns <- session$ns
  stat <- reactive(input$stat)
  d <- reactive({ 
    if(dom()==dom0) readRDS(paste0("data/", stat(), "_", dom(), ".rds")) else NULL
  })
  
  hm_title <- "Estimated monthly error: composites of lowest-error GCMs"
  hm_title2 <- "Estimated monthly error: random ensembles"
  hm_subtitle <- reactive({
    switch(input$var,
           "integrated"=expression(italic("Integrated variables are standardized to unitless error metric")),
           "pr"=expression(italic("Estimated precipitation error is in mm")),
           "psl"=expression(italic("Estimated sea level pressure error is in kPa")),
           "tas"=expression(italic("Estimated temperature error is in"~degree~C))
    )
  })
  lab_rnd <- reactive({
    switch(input$var, "integrated"=1, "pr"=0, "psl"=0, "tas"=1)
  })
  
  output$hmap_sel <- renderPlot({
    req(d())
    gcmHeatmap(filter(d()$sb.hm1, Var==input$var & Group=="Selected"), "Month", "Composite",
               lab="Val", lab.rnd=lab_rnd(), 
               title=hm_title, subtitle=hm_subtitle(), xlb="Month", ylb="Number of GCMs in composite") +
      .theme + geom_hline(yintercept=21 - c(4.5, 5.5) + 1, linetype=2)
  })
  output$hmap_ran <- renderPlot({
    req(d())
    gcmHeatmap(filter(d()$sb.hm1, Var==input$var & Group=="Random"), "Month", "Composite",
               lab="Val", lab.rnd=lab_rnd(), 
               title=hm_title2, subtitle=hm_subtitle(), xlb="Month", ylb="Number of GCMs in composite") +
      .theme + geom_hline(yintercept=21 - c(4.5, 5.5) + 1, linetype=2)
  })
}
