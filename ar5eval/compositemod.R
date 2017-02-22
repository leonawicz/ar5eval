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
      tabBox(
        tabPanel("Permutation tests",
          fluidRow(
           box(title="Selected composite permutation test", 
               plotOutput(ns("perm_one"), height=560), sliderInput(ns("size"), "Composite size", 1, 21, 5, 1, width="50%"),
               status="primary", width=6),
           box(title="Estimated error and p-values vs. composite size", plotOutput(ns("perm_all"), height=600), status="primary", width=6)
          )
        ),
        tabPanel("Monthly error maps", 
          fluidRow(
           box(title="Sequential selected GCMs", plotOutput(ns("hmap_sel"), height=600), status="primary", width=6),
           box(title="Random ensebles of opportunity", plotOutput(ns("hmap_ran"), height=600), status="primary", width=6)
          )
        ),
        id="ensembles", selected="Monthly error maps", title="Selected composites and random ensembles", width=12, side="right"
      )
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
  
  output$perm_one <- renderPlot({
    req(d())
    gcmPlot(d()$re, input$var, "histogram", size=input$size) + .theme
  })
  output$perm_all <- renderPlot({
    req(d())
    gcmPlot(d()$re, input$var, "line") + .theme + scale_x_continuous(breaks=1:21) +
      geom_vline(xintercept=input$size, size=1, linetype=2)
  })
}
