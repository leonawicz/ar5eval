gcmPlot <- function(x, var, type, size=1){
  stopifnot(type %in% c("histogram", "line"))
  stopifnot(size %in% unique(x$Composite))
  x <- filter(x, Var==var)
  err <- expression(RMSE)
  prime.lab <- paste0("Estimated ", tolower(var), " mean annual ", err)
  sel <- filter(x, Group=="Selected" & Composite==size)$Val
  x2 <- filter(x, Group!="Individual" & Composite==size)
  pval <- sum(sel > x2$Val)/nrow(x2)
  
  if(type=="histogram"){
    lbl <- paste("Selected composite:", round(sel, 2), "\np-value:", round(pval, 3), "\n")
    g <- ggplot(filter(x2, Composite==size), aes(Val)) + geom_density(size=1) + 
      geom_vline(xintercept=sel, size=1, linetype=2) + geom_point(x=sel, y=0, size=5, shape=18) +
      annotate("text", Inf, Inf, label=lbl, hjust = 1, vjust = 1) +
      labs(title=paste0("Selected ", size, "-GCM composite"), 
           subtitle=expression(italic("among ensembles of opportunity")),
           x=prime.lab, y="Density")
    return(g)
  }
  
  x <- group_by(x, Composite, Group) %>% 
    summarise(LB=min(Val), p05=quantile(Val, 0.05), Mean=mean(Val), UB=max(Val))
  g <- ggplot(x, aes(Composite, Mean, colour=Group, group=Group)) + 
    geom_ribbon(data=filter(x, Group=="Random"), aes(ymin=LB, ymax=UB), alpha=0.3, colour="white") +
    geom_line(data=filter(x, Group=="Random"), size=1) +
    #geom_line(data=filter(x, Group=="Random"), aes(y=p05), size=1) +
    geom_line(data=filter(x, Group=="Selected"), size=1) +
    geom_line(data=filter(x, Group=="Individual"), size=1) +
    labs(title="Estimated error", subtitle=expression(italic("by composite GCM size")),
         x="Number of GCMs in composite", y=prime.lab) +
    scale_colour_manual(values=c("orange", "black", "royalblue"))
  g
}

gcmHeatmap <- function(data, x, y, fill="Val", lab=NULL, lab.rnd=2,
  title="", subtitle="", xlb="", ylb="", gcm_labels=NULL, legend.title="Estimated error"){
  clrs <- switch(as.character(data$Var[1]), 
                 integrated=c("white", "chocolate4"), 
                 pr=c("white", "lightgreen", "darkgreen"), 
                 psl=c("white", "lightblue", "dodgerblue", "blue"), 
                 tas=c("white", "salmon", "red"))
  if(!is.null(gcm_labels)) ylb <- "GCM composite order (top to bottom)"
  if("Composite" %in% c(x, y)){
    data <- mutate(data, Composite=factor(Composite, levels=21:1))
    if(is.null(gcm_labels)) gcm_labels <- 1:21
  }
  g <- ggplot(data, aes_string(x, y)) + geom_tile(aes_string(fill=fill))
  if(!is.null(lab)){
    lab <- paste0("round(", lab, ", ", lab.rnd, ")")
    g <- g + geom_text(aes_string(label=lab))
  }
  yscale <- if(is.null(gcm_labels)) scale_y_discrete(position="left") else 
    scale_y_discrete(position="left", labels=rev(gcm_labels))
  g + scale_fill_gradientn(colours=clrs) +
    labs(title=title, subtitle=subtitle, x=xlb, y=ylb) +
    yscale + scale_x_discrete(expand=c(0,0)) +
    guides(fill=guide_legend(title=legend.title))
}
