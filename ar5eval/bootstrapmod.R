spbootModUI <- function(id){
  ns <- NS(id)
  tabItem(tabName=id,
    fluidRow(
      column(3,
        selectInput(ns("stat"), "Error statistic", 
          c("RMSE", "RMSE (bias removed)"="RMSE0", "MAE", "MAE (bias removed)"="MAE0"), width="100%")
      )
    ),
    fluidRow(
      box(title="Mean annual integrated climate variable estimated GCM error distributions", 
          plotOutput(ns("distPlot"), height=600), status="primary", width=7),
      box(title="Monthly estimated mean GCM error heat map", 
          plotOutput(ns("hmap_gcms"), height=600), status="primary", width=5)
    ),
    fluidRow(
      box(
        fluidRow(
          column(4,
            selectInput(ns("vals"), "Sampling distribution", 
              c("Estimated error"="Val", "Performance rankings"="Rank"), width="100%")
          )
        ), status="primary", width=7
      ),
      box(
        fluidRow(
          column(6,
            selectInput(ns("var"), "Climate variable", 
              c("Integrated", "Temperature"="tas", "Precipitation"="pr", "Sea level pressure"="psl"), width="100%")
          ),
          column(6, 
            selectInput(ns("overlay"), "Cell overlay stats", 
              c("", "Raw error values"="Val", "Probability in top 5"="PropTop5", "Mean rank"="Mean_Rank"), width="100%")
          )
        ), status="primary", width=5
      )
    ), br(), br(), br()
  )
}

spbootMod <- function(input, output, session, dom0, dom){
  ns <- session$ns
  
  .plottheme <- theme(panel.grid.major=element_line(size = .5, color = "grey"),
                      plot.title=element_text(hjust=0),
                      axis.line=element_line(size=.7, color="black"),
                      axis.ticks.length=unit(0.35,"cm"),
                      legend.position="bottom",
                      text = element_text(size=14),
                      panel.spacing.x=unit(0.25,"cm"),
                      plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
                      strip.text=element_text(size=14))
  
  stat <- reactive(input$stat)
  d <- reactive({ 
    dom2 <- if(substr(dom(), 1, 2)=="sb") substring(dom(), 3) else dom()
    if(dom2==dom0) readRDS(paste0("data/", stat(), "_", dom2, ".rds")) else NULL
  })
  
  stat.lab <- reactive({ 
    x <- switch(stat(), 
           "RMSE"="RMSE", "RMSE0"="RMSE (biased removed)", 
           "MAE"="MAE", "MAE0"="MAE (bias removed)")
    if(input$vals=="Estimated error") x else "performance rankings"
  })
  
  output$distPlot <- renderPlot({
    req(d())
    xlb <- if(stat.lab()=="performance rankings") "GCM performance rank" else stat.lab()
    top5 <- unique((filter(d()$re, Composite <= 5 & Group=="Individual")$GCM))
    below5 <- unique((filter(d()$re, Composite > 5)$GCM))
    d5 <- filter(d()$samples, GCM %in% top5)
    dx <- filter(d()$samples, GCM %in% below5)
    g <- ggplot(d5, aes_string(input$vals, colour="GCM", fill="GCM", group="GCM")) + 
      geom_density(data=dx, size=1, colour="gray30", fill="gray30", alpha=0.3) + 
      geom_density(size=1, alpha=0.3) + 
      labs(title=paste0("Sampling distributions of ", stat.lab(), " by GCM"), 
           subtitle=expression(italic("based on spatial bootstrap resampling")),
           x=xlb, y="Density") +
      .plottheme
    g
  })
  
  lab <- reactive({
    if(is.null(input$overlay) || input$overlay=="") return()
    input$overlay
  })
  
  hm_title <- "Estimated monthly error by GCM"
  hm_subtitle <- reactive({
    switch(input$var,
      "Integrated"=expression(italic("Integrated variables are standardized to unitless error metric")),
      "pr"=expression(italic("Estimated precipitation error is in mm")),
      "psl"=expression(italic("Estimated sea level pressure error is in kPa")),
      "tas"=expression(italic("Estimated temperature error is in"~degree~C))
    )
  })
  lab_rnd <- reactive({
    if(input$overlay=="Val") 
      x <- switch(input$var, "Integrated"=1, "pr"=0, "psl"=0, "tas"=1)
    if(input$overlay=="PropTop5") x <- 2
    if(input$overlay=="Mean_Rank") x <- 0
    x
  })
  
  output$hmap_gcms <- renderPlot({
    req(d())
    if(input$overlay %in% c("", "Val")){
      gcmHeatmap(filter(d()$sb.hm1, Var==input$var & Group=="Individual"), "Month", "GCM", 
        lab=lab(), lab.rnd=lab_rnd(), 
        title=hm_title, subtitle=hm_subtitle(), xlb="Month", ylb="GCM") +
        .plottheme
    } else {
      gcmHeatmap(filter(d()$sb.hm2, Var==input$var), "Month", "GCM", 
        lab=lab(), lab.rnd=lab_rnd(),
        title=hm_title, subtitle=hm_subtitle(), xlb="Month", ylb="GCM") +
        .plottheme
    }
  })
}
