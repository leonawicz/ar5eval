library(dplyr)
library(purrr)
library(ggplot2)
library(snapplot)

app_intro <- list(
  title = app_intro_title, message = app_intro_message, logo = app_intro_logo, 
  toast_args = list(timeOut = 0, extendedTimeOut = 0)
)

shinyServer(function(input, output, session) {
  
  observe({
    x <- actionButton("overview_btn", "Overview", class = "btn-block", icon("info-circle"))
    showNotification("Need an overview?", action = x, duration = NULL, id = "overview_note", type = "message")
  })
  
  observe({
    input$overview_btn
    isolate({
      if(!is.null(input$overview_btn) && input$overview_btn > 0){
        appintro(title = app_intro$title, message = app_intro$message, logo = app_intro$logo, 
                 toast_args = app_intro$toast_args)
      }
    })
  })
  
  source("tour.R", local=TRUE) # introjs tour
  
  observeEvent(input$staticmap, {
    showModal(modalDialog(
      title = "AR5 GCM evaluation spatial domains",
      img(src = 'domain_map.png', align = "center", style = "width: 100%"),
      size = "l", easyClose = TRUE, footer = NULL
      ))
  })
  
  observeEvent(input$quick, {
    showModal(modalDialog(
      title = "Two stages of GCM evaluation", quick.text,
      size="l", easyClose = TRUE, footer = NULL
    ))
  })
  
  dsub <- reactive({ 
    x <- filter(d, Domain %in% input$spdom & Stat %in% input$stat & 
                  Var %in% input$vars & Period %in% input$time) %>%
          group_by(Domain, Stat, Var) %>% filter(rank(Mean_Rank) <= input$n_gcms) %>% ungroup
    xx <<- x
    if(input$order=="mean"){
      lev <- (group_by(xx, GCM) %>% summarise(Mean_Rank = mean(Mean_Rank)) %>% arrange(Mean_Rank))$GCM
      x <- mutate(x, GCM = factor(GCM, levels = lev))
    }
    x
  })
  
  clrby <- reactive({ if(input$clrby=="") NULL else input$clrby })
  
  period <- reactive({ 
    if(input$time=="Annual") "mean annual" else month.name[match(input$time, month.abb)] 
  })
  
  output$rankPlot <- renderPlot({
    .theme <- snapplot::theme_snap(base_size = 16)
    if(is.null(input$spdom) || is.null(input$stat) || is.null(input$vars)) return()
    pos <- if(!is.null(clrby())) position_dodge(width=0.75) else "identity"
    subtitle <- paste("based on", period(), "error metric")
    g <- ggplot(dsub(), 
      aes_string(x="GCM", y="Mean_Rank", ymin="Min_Rank", ymax="Max_Rank", colour=clrby())) +
      geom_point(position=pos) + geom_crossbar(width=0.75, position=pos)
    if(input$fctby!="") g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), scales="free")
    g + .theme + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            strip.background = element_rect(color = "white", fill = "white")) +
      labs(title="Spatial bootstrap GCM performance rankings", 
           subtitle=bquote(italic(.(subtitle))), 
           y="GCM rank range and mean")
  })
  
  plot_top5 <- reactive({
    .theme <- snapplot::theme_snap(base_size = 16)
    if(is.null(input$spdom) || is.null(input$stat) || is.null(input$vars)) return()
    pos <- if(!is.null(clrby())) position_dodge(width=0.75) else "identity"
    subtitle <- paste(period(), "spatial bootstrap of GCM ranking fifth or better")
    g <- ggplot(dsub(), 
                aes_string(x="GCM", y="PropTop5", fill=clrby())) +
      geom_bar(stat="identity", position=pos, colour="black", width=0.75)
    if(input$fctby!="") g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), scales="free")
    g + .theme + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=0.6),
                       strip.background = element_rect(color = "white", fill = "white")) +
      labs(title="Probability of GCM among top five performers", 
           subtitle=bquote(italic(.(subtitle))), 
           x="GCM", y="P(in top five)")
  })
  output$top5Plot <- renderPlot({ plot_top5() })
  
  # Observe plot for hiding launch overlay (must appear in code after plot_ts() is defined)
  observe(if(!is.null(plot_top5())) shinyjs::hide("fade-wrapper", anim=TRUE, animType="fade", time=1))
  
  dom <- reactive(input$tabs)
  map(domains, ~callModule(spbootMod, paste0("sb", .x), .x, dom, snapplot::theme_snap(base_size = 18)))
  map(domains, ~callModule(compositeMod, .x, .x, dom, snapplot::theme_snap(base_size = 18), info=gcm_inclusion))
})
