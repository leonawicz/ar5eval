library(dplyr)
library(purrr)
library(ggplot2)
library(snapplot)

shinyServer(function(input, output, session) {
  
  source("tour.R", local=TRUE) # introjs tour
  
  observeEvent(input$staticmap, {
    showModal(modalDialog(
      title="AR5 GCM evalutation spatial domains",
      img(src='domain_map.png', align="center", style="width: 100%"),
      size="l", easyClose=TRUE, footer=NULL
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
    .theme <- snapplot::theme_snap(base_size = 17)
    if(is.null(input$spdom) || is.null(input$stat) || is.null(input$vars)) return()
    pos <- if(!is.null(clrby())) position_dodge(width=0.75) else "identity"
    subtitle <- paste("based on", period(), "error metric")
    g <- ggplot(dsub(), 
      aes_string(x="GCM", y="Mean_Rank", ymin="Min_Rank", ymax="Max_Rank", colour=clrby())) +
      geom_point(position=pos) + geom_crossbar(width=0.75, position=pos)
    if(input$fctby!="") g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), scales="free")
    g + .theme + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      labs(title="Spatial bootstrap GCM performance rankings", 
           subtitle=bquote(italic(.(subtitle))), 
           y="Bootstrap GCM rank range and mean")
  })
  
  output$top5Plot <- renderPlot({
    .theme <- snapplot::theme_snap(base_size = 17)
    if(is.null(input$spdom) || is.null(input$stat) || is.null(input$vars)) return()
    pos <- if(!is.null(clrby())) position_dodge(width=0.75) else "identity"
    subtitle <- paste(period(), "spatial bootstrap of GCM ranking fifth or better")
    g <- ggplot(dsub(), 
                aes_string(x="GCM", y="PropTop5", fill=clrby())) +
      geom_bar(stat="identity", position=pos, colour="black", width=0.75)
    if(input$fctby!="") g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), scales="free")
    g + .theme + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
      labs(title="Probability of GCM among top five performers", 
           subtitle=bquote(italic(.(subtitle))), 
           x="GCM", y="P(among top five performing GCMs)")
  })
  
  dom <- reactive(input$tabs)
  map(domains, ~callModule(spbootMod, paste0("sb", .x), .x, dom, snapplot::theme_snap(base_size = 18)))
  map(domains, ~callModule(compositeMod, .x, .x, dom, snapplot::theme_snap(base_size = 18), info=gcm_inclusion))
})
