compositeModUI <- function(id){
  ns <- NS(id)
  tabItem(tabName=id,
    fluidRow(
      column(9, h4("Variable selection")),
      column(3, actionButton(ns("gcm_order_info"), "GCM inclusion order", class="btn-block", icon("info-circle")))
    ),
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
          div(id="plot-container",
            fluidRow(
             box(title="Selected composite permutation test", 
                 plotOutput(ns("perm_one"), height=560), 
                 fluidRow(column(6, sliderInput(ns("size"), "Composite size", 1, 21, 5, 1, width="100%"))),
                 status="primary", width=6),
             box(title="Estimated error and p-values vs. composite size", 
                 plotOutput(ns("perm_all"), height=600), 
                 status="primary", width=6)
            ),
            conditionalPanel(
              sprintf("output['%s'] == null", ns("perm_one")), 
              h4("Loading data", style="position: absolute; left: 0; top: 40%; right: 0; text-align: center;"),
              tags$img(src="spinner.gif", id="loading-spinner")
            )
          )
        ),
        tabPanel("Monthly error maps", 
          div(id="plot-container",
            fluidRow(
             box(title="Sequential selected GCMs", 
                 plotOutput(ns("hmap_sel"), height=600),
                 checkboxInput(ns("gcm_labels"), "Use GCM labels by composite order", FALSE), status="primary", width=6),
             box(title="Random ensembles of opportunity", plotOutput(ns("hmap_ran"), height=600), status="primary", width=6)
            ),
            conditionalPanel(
              sprintf("output['%s'] == null", ns("hmap_sel")), 
              h4("Loading data", style="position: absolute; left: 0; top: 40%; right: 0; text-align: center;"),
              tags$img(src="spinner.gif", id="loading-spinner")
            )
          )
        ),
        id="ensembles", selected="Monthly error maps", title="Selected composites and random ensembles", width=12, side="right"
      )
    )
  )
}

compositeMod <- function(input, output, session, dom0, dom, .theme, ...){
  ns <- session$ns
  stat <- reactive(input$stat)

  observeEvent(input$gcm_order_info, {
    showModal(modalDialog(
      title="About GCM inclusion order", list(...)$info, size="l", easyClose=TRUE, footer=NULL
    ))
  })
  
  rdsfile <- reactive({ if(dom()==dom0) paste0(dataloc, "/", stat(), "_", dom(), ".rds") else NULL })
  source("data.R", local=TRUE)
  
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
    req(rv$d)
    gcmlab <- if(input$gcm_labels) slice(filter(rv$d$re, Var==input$var), 1:21)$GCM else NULL
    gcmHeatmap(filter(rv$d$sb.hm1, Var==input$var & Group=="Selected"), "Month", "Composite",
               lab="Val", lab.rnd=lab_rnd(), 
               title=hm_title, subtitle=hm_subtitle(), xlb="Month", ylb="Number of GCMs in composite",
               gcm_labels=gcmlab) +
      .theme + geom_hline(yintercept=21 - c(4.5, 5.5) + 1, linetype=2)
  })
  output$hmap_ran <- renderPlot({
    req(rv$d)
    gcmHeatmap(filter(rv$d$sb.hm1, Var==input$var & Group=="Random"), "Month", "Composite",
               lab="Val", lab.rnd=lab_rnd(), 
               title=hm_title2, subtitle=hm_subtitle(), xlb="Month", ylb="Number of GCMs in composite") +
      .theme + geom_hline(yintercept=21 - c(4.5, 5.5) + 1, linetype=2)
  })
  
  output$perm_one <- renderPlot({
    req(rv$d)
    gcmPlot(rv$d$re, input$var, "histogram", size=input$size) + .theme
  })
  output$perm_all <- renderPlot({
    req(rv$d)
    gcmPlot(rv$d$re, input$var, "line") + .theme + scale_x_continuous(breaks=1:21) +
      geom_vline(xintercept=input$size, size=1, linetype=2)
  })
}
