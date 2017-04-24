spbootModUI <- function(id){
  ns <- NS(id)
  tabItem(tabName=id,
    fluidRow(column(12, h4("Variable selection"))),
    fluidRow(
      column(3,
        selectInput(ns("stat"), "Error statistic", 
          c("RMSE", "RMSE (bias removed)"="RMSE0", "MAE", "MAE (bias removed)"="MAE0"), width="100%")
      )#, # use later when samples available for each variable, see ditribution plot below
      #column(3, selectInput(ns("var"), "Climate variable", variables, "integrated", width="100%"))
    ),
    div(id="plot-container",
      fluidRow(
        box(title="Mean annual estimated GCM error distributions", 
            plotOutput(ns("distPlot"), height=600), status="primary", width=7),
        box(title="Monthly estimated mean GCM error heat map", 
            plotOutput(ns("hmap_gcms"), height=600), status="primary", width=5)
      ),
      conditionalPanel(
        sprintf("output['%s'] == null", ns("distPlot")), 
        h4("Loading data", style="position: absolute; left: 0; top: 40%; right: 0; text-align: center;"),
        tags$img(src="spinner.gif", id="loading-spinner")
      )
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
              c("Integrated"="integrated", "Temperature"="tas", "Precipitation"="pr", "Sea level pressure"="psl"), "integrated", width="100%")
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

spbootMod <- function(input, output, session, dom0, dom, .theme){
  ns <- session$ns
  
  stat <- reactive(input$stat)
  rdsfile <- reactive({ 
    dom2 <- if(substr(dom(), 1, 2)=="sb") substring(dom(), 3) else dom()
    if(dom2==dom0) paste0(dataloc, "/", stat(), "_", dom2, ".rds") else NULL
  })
  source("data.R", local=TRUE)
  
  stat.lab <- reactive({ 
    x <- switch(stat(), 
           "RMSE"="RMSE", "RMSE0"="RMSE (biased removed)", 
           "MAE"="MAE", "MAE0"="MAE (bias removed)")
    if(input$vals=="Val") x else "performance rankings"
  })
  
  distPlot <- reactive({
    input$vals
    input$var
    stat.lab()
    isolate({
      req(rv$d)
      xlb <- if(stat.lab()=="performance rankings") "GCM performance rank" else stat.lab()
      top5 <- unique((filter(rv$d$re, Composite <= 5 & Group=="Individual" & Var=="integrated")$GCM)) # temporary
      below5 <- unique((filter(rv$d$re, Composite > 5 & Group=="Individual" & Var=="integrated")$GCM))
      # use later when samples available for each variable:
      #top5 <- unique((filter(rv$d$re, Composite <= 5 & Group=="Individual" & Var==input$var)$GCM))
      #below5 <- unique((filter(rv$d$re, Composite > 5 & Group=="Individual" & Var==input$var)$GCM))
      d5 <- filter(rv$d$samples, GCM %in% top5)
      dx <- filter(rv$d$samples, GCM %in% below5)
      g <- ggplot(d5, aes_string(input$vals, colour="GCM", fill="GCM", group="GCM")) + 
        geom_density(data=dx, size=1, colour="gray30", fill="gray30", alpha=0.3) + 
        geom_density(size=1, alpha=0.3) + 
        # temporary hardcoded "integrated variable" only
        labs(title=paste(xlb, "sampling distributions by GCM"),
             subtitle=expression(italic("Integrated variable, based on spatial bootstrap resampling")),
             x=xlb, y="Density") +
        .theme
      g
    })
  })
  output$distPlot <- renderPlot({ distPlot() })
  
  lab <- reactive({
    if(is.null(input$overlay) || input$overlay=="") return()
    input$overlay
  })
  
  hm_title <- "Estimated monthly error by GCM"
  hm_subtitle <- reactive({
    switch(input$var,
      "integrated"=expression(italic("Integrated variables are normalized")),
      "pr"=expression(italic("Estimated precipitation error is in mm")),
      "psl"=expression(italic("Estimated sea level pressure error is in kPa")),
      "tas"=expression(italic("Estimated temperature error is in"~degree~C))
    )
  })
  lab_rnd <- reactive({
    if(input$overlay=="Val") 
      x <- switch(input$var, "integrated"=1, "pr"=0, "psl"=0, "tas"=1)
    if(input$overlay=="PropTop5") x <- 2
    if(input$overlay=="Mean_Rank") x <- 1
    x
  })
  
  output$hmap_gcms <- renderPlot({
    req(rv$d)
    if(input$overlay %in% c("", "Val")){
      gcmHeatmap(filter(rv$d$sb.hm1, Var==input$var & Group=="Individual"), "Month", "GCM", 
        lab=lab(), lab.rnd=lab_rnd(), 
        title=hm_title, subtitle=hm_subtitle(), xlb="Month", ylb="GCM") +
        .theme
    } else {
      gcmHeatmap(filter(rv$d$sb.hm2, Var==input$var), "Month", "GCM", 
        lab=lab(), lab.rnd=lab_rnd(),
        title=hm_title, subtitle=hm_subtitle(), xlb="Month", ylb="GCM") +
        .theme
    }
  })
}
