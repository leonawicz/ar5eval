compositeModUI <- function(id){
  ns <- NS(id)
  tabItem(tabName=id,
    fluidRow(
      column(3,
        selectInput(ns("stat"), "Error statistic", 
          c("RMSE", "RMSE (bias removed)"="RMSE0", "MAE", "MAE (bias removed)"="MAE0"), width="100%")
      ),
      column(3,
        selectInput(ns("var"), "Climate variable", 
          c("Integrated", "Temperature"="tas", "Precipitation"="pr", "Sea level pressure"="psl"), width="100%")
      )
    ),
    fluidRow(
      box(title="Sequential selected GCMs", plotOutput(ns("hmap_sel")), status="primary", width=6),
      box(title="Random ensebles of opportunity", plotOutput(ns("hmap_ran")), status="primary", width=6)
    )
  )
}

compositeMod <- function(input, output, session, dom0, dom){
  ns <- session$ns
  stat <- reactive(input$stat)
  d <- reactive({ 
    if(dom()==dom0) readRDS(paste0("data/", stat(), "_", dom(), ".rds")) else NULL
  })
  
  output$hmap_sel <- renderPlot({
    req(d())
    gcmHeatmap(filter(d()$sb.hm1, Var==input$var & Group=="Selected"), "Month", "Composite")
  })
  output$hmap_ran <- renderPlot({
    req(d())
    gcmHeatmap(filter(d()$sb.hm1, Var==input$var & Group=="Random"), "Month", "Composite")
  })
}
