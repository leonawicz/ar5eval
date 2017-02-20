library(dplyr)
library(purrr)
library(ggplot2)
.plottheme <- theme(panel.grid.major=element_line(size = .5, color = "grey"),
                    plot.title=element_text(hjust=0),
                    axis.line=element_line(size=.7, color="black"),
                    axis.ticks.length=unit(0.35,"cm"),
                    legend.position="bottom",
                    text = element_text(size=14),
                    panel.spacing.x=unit(0.25,"cm"),
                    plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
                    strip.text=element_text(size=14))

shinyServer(function(input, output, session) {
  
  dsub <- reactive({ 
    filter(d, Domain %in% input$spdom & Stat %in% input$stat & Var %in% input$vars & Period %in% input$time) %>%
          group_by(Domain, Stat, Var) %>% filter(rank(Mean_Rank) <= input$n_gcms) %>% ungroup
  })
  clrby <- reactive({ if(input$clrby=="") NULL else input$clrby })
  period <- reactive({ 
    if(input$time=="Annual") "mean annual" else month.name[match(input$time, month.abb)] 
  })
  
  output$rankPlot <- renderPlot({
    if(is.null(input$spdom) || is.null(input$stat) || is.null(input$vars)) return()
    pos <- if(!is.null(clrby())) position_dodge(width=0.75) else "identity"
    subtitle <- paste("based on", period(), "error metric")
    g <- ggplot(dsub(), 
      aes_string(x="GCM", y="Mean_Rank", ymin="Min_Rank", ymax="Max_Rank", colour=clrby())) +
      geom_point(position=pos) + geom_crossbar(width=0.5, position=pos)
    if(input$fctby!="") g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), scales="free")
    g + .plottheme + 
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
      labs(title="Spatial bootstrap GCM performance rankings", 
           subtitle=bquote(italic(.(subtitle))), 
           y="Bootstrap GCM rank range and mean")
  })
  
  output$top5Plot <- renderPlot({
    if(is.null(input$spdom) || is.null(input$stat) || is.null(input$vars)) return()
    pos <- if(!is.null(clrby())) position_dodge(width=0.75) else "identity"
    subtitle <- paste(period(), "spatial bootstrap of GCM ranking fifth or better")
    g <- ggplot(dsub(), 
                aes_string(x="GCM", y="PropTop5", fill=clrby())) +
      geom_bar(stat="identity", position=pos, colour="black", width=0.5)
    if(input$fctby!="") g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), scales="free")
    g + .plottheme + theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
      labs(title="Probability of GCM among top five performers", 
           subtitle=bquote(italic(.(subtitle))), 
           x="GCM", y="P(among top five performing GCMs)")
  })
  
  dom <- reactive(input$tabs)
  
  callModule(spbootMod, "sbAK", "AK", dom)
  callModule(spbootMod, "sbAK_land", "AK_land", dom)
  callModule(spbootMod, "sbAK_water", "AK_water", dom)
  callModule(spbootMod, "sbCAN", "CAN", dom)
  callModule(spbootMod, "sbAKCAN", "AKCAN", dom)
  callModule(spbootMod, "sb6090N", "6090N", dom)
  callModule(spbootMod, "sb2090N", "2090N", dom)
  callModule(spbootMod, "sblow48", "low48", dom)
  callModule(spbootMod, "sbpacif", "pacif", dom)
  
  callModule(compositeMod, "AK", "AK", dom)
  callModule(compositeMod, "AK_land", "AK_land", dom)
  callModule(compositeMod, "AK_water", "AK_water", dom)
  callModule(compositeMod, "CAN", "CAN", dom)
  callModule(compositeMod, "AKCAN", "AKCAN", dom)
  callModule(compositeMod, "6090N", "6090N", dom)
  callModule(compositeMod, "2090N", "2090N", dom)
  callModule(compositeMod, "low48", "low48", dom)
  callModule(compositeMod, "pacif", "pacif", dom)
})
