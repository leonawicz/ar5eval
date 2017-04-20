# calculate RMSE and MAE with and without bias correction
.get_rmse_mae <- function(x, y){
  dif <- x-y
  bias <- mean(dif, na.rm=T)
  rmse <- sqrt(colMeans(dif^2, na.rm=T))
  rmse0 <- sqrt(colMeans((dif-bias)^2, na.rm=T))
  mae <- colMeans(abs(dif), na.rm=T)
  mae0 <- colMeans(abs(dif-bias), na.rm=T)
  stats <- c("RMSE", "RMSE0", "MAE", "MAE0")
  data.frame(Month=factor(month.abb, levels=c("Annual", month.abb)),
             Stat=factor(rep(stats, each=12), levels=stats),
             Val=c(rmse, rmse0, mae, mae0)) %>% tbl_df
}

combo <- function(size, n){
  x <- map(size, ~combn(max(size), .x))
  l <- sapply(x, ncol)
  map2(x, l, ~if(.y > n) cbind(.x[, 1], .x[, sample(2:ncol(.x), n-1)]) else .x)
}

# calculate error distributions
#
# if composite=TRUE, compositing is done ordered by 'gcms',
# typically run as second pass (first pass obtains ordered list of best performing individual GCMs)
# 
# a second composite option, set composite.size to an integer or integer sequence to obtain error
# distributions for random ensembles of GCMs of each given integer composite size,
# useful for assessing the p-value of the chosen composite model in a permutation test
# involving a sample of composite models of the size composite size
#
# 'data' and 'return.data' exist simply to speed up iterative calls to gcmEval.
# run once with return.data=TRUE and subsequently run gcmEval passing the returend data to 'data'
# each time with return.data=FALSE
gcmEval <- function(i, gcmDir, baseDir, surface.mask, bbox, gcms=list.files(gcmDir), n=1, 
                    vars=c("pr", "psl", "tas"), type="all", land=1, water=0,
                    composite=FALSE, composite.size=NULL, exact=TRUE, data=NULL, return.data=FALSE, 
                    all.vars=c("integrated", "pr", "psl", "tas")){
  doms <- names(bbox); id <- doms[i]; bbox <- bbox[[i]]; type <- type[i]
  cells <- cellsFromExtent(surface.mask, bbox)
  if(type=="land") cells <- cells[which(surface.mask[cells]==land)]
  if(type=="water") cells <- cells[which(surface.mask[cells]==water)]
  vars <- c("pr", "psl", "tas")
  base <- map(vars, ~extract(stack(list.files(baseDir, pattern=.x, full=T)), cells))
  files.gcm <- map(vars, ~list.files(file.path(gcmDir, gcms), pattern=.x, full=T)
                   %>% split(basename(dirname(.))))
  gcmExtract <- function(files, cells) map(files, ~extract(stack(.x), cells))
  if(is.null(data)){
    x <- map(seq_along(vars), ~gcmExtract(files.gcm[[.x]], cells))
  } else {
    if(is.null(names(data))) data <- data[[i]]
    x <- data$data
    x <- map(seq_along(vars), ~x[[.x]][match(gcms, data$gcms)])
  }
  if(return.data) return(list(data=x, gcms=gcms))
  
  boot.replace <- if(n==1) FALSE else TRUE
  nr <- nrow(base[[1]])
  .composite <- function(gcms, gcm, x, rb){
    idx <- match(gcms[1:which(gcms==gcm)], gcms)
    Reduce("+", x[idx])[rb,]/length(idx)
  }
  .compositeRandom <- function(gcms, j, x, rb){
    idx <- sample(seq_along(gcms), j, replace=FALSE)
    Reduce("+", x[idx])[rb,]/j
  }
  gcmStats <- function(gcms, x, base, rb, composite=FALSE, composite.size=NULL){
    if(composite){
      if(is.null(composite.size)){
        map(gcms, ~.get_rmse_mae(base[rb,], .composite(gcms, .x, x, rb)) %>% 
              mutate(GCM=.x, Composite=which(gcms==.x))) %>% bind_rows
      } else {
        map(composite.size, ~.get_rmse_mae(base[rb,], .compositeRandom(gcms, .x, x, rb)) %>%
              mutate(GCM="Random", Composite=.x)) %>% bind_rows
      }
    } else {
      map(gcms, ~.get_rmse_mae(base[rb,], x[[.x]][rb,]) %>% mutate(GCM=.x)) %>% bind_rows
    }
  }
  
  if(!exact){
    d <- vector("list", n)
    for(k in 1:n){
      rb <- sample(1:nr, nr, replace=boot.replace)
      if(composite) d[[k]] <- map(seq_along(vars), ~
                                    gcmStats(gcms, x[[.x]], base[[.x]], rb, TRUE, composite.size) %>% 
                                    mutate(Var=vars[.x]))
      if(!composite) d[[k]] <- map(seq_along(vars), ~gcmStats(gcms, x[[.x]], base[[.x]], rb) %>%
                                     mutate(Var=vars[.x]))
      d[[k]] <- d[[k]] %>% bind_rows %>% mutate(Sample=k)
      if(k %% 100 == 0) print(k)
    }
  }
  
  exactStats <- function(gcms, x, base, rb, combos){
    d1 <- map(gcms, ~.get_rmse_mae(base[rb,], x[[.x]][rb,]) %>% 
          mutate(GCM=.x, Composite=which(gcms==.x), Group="Individual")) %>% bind_rows
    
    .composites <- function(gcms, x, base, rb, combos){
      d <- vector("list", ncol(combos))
      for(i in 1:ncol(combos)){
        if(i==1) print(paste("Estimating error for composite size =", nrow(combos)))
        gcms.sub <- gcms[combos[, i]]
        idx <- match(gcms.sub, gcms)
        tmp <- Reduce("+", x[idx])[rb,]/length(idx)
        d[[i]] <- .get_rmse_mae(base[rb,], tmp) %>% 
          mutate(GCM=paste(gcms.sub, collapse=","), 
                 Composite=length(gcms.sub), 
                 Group="Random")
        if(i==1) d[[i]] <- bind_rows(d[[i]], mutate(d[[i]], Group="Selected"))
      }
      bind_rows(d)
    }
    d2 <- map(combos, ~.composites(gcms, x, base, rb, .x)) %>% bind_rows
    bind_rows(d1, d2)
  }
  
  if(composite & exact){
    nn <- 1 # in place of original 'n' which is now for composite combinations
    d <- vector("list", nn)
    for(k in 1:nn){
      rb <- 1:nr #sample(1:nr, nr, replace=boot.replace)
      combos <- combo(composite.size, n)
      d[[k]] <- map(seq_along(vars), ~
                 exactStats(gcms, x[[.x]], base[[.x]], rb, combos) %>% mutate(Var=vars[.x])) %>%
        bind_rows %>% mutate(Sample=k)
      if(k %% 100 == 0) print(k)
    }
  }
  
  d <- bind_rows(d) %>% 
    mutate(Domain=factor(id, levels=doms), Var=factor(Var, levels=all.vars))
  if(exact & composite){
    d <- select(d, Domain, Var, Stat, Month, Sample, GCM, Composite, Group, Val) %>%
      mutate(Group=factor(Group, levels=c("Random", "Selected", "Individual"))) %>%
      arrange(Domain, Var, Stat, Month, Sample, Composite, Group)
  } else if(composite){
    d <- select(d, Domain, Var, Stat, Month, Sample, GCM, Composite, Val) %>%
      arrange(Domain, Var, Stat, Month, Sample, Composite)
  } else {
    d <- select(d, Domain, Var, Stat, Month, Sample, GCM, Val) %>%
      arrange(Domain, Var, Stat, Month, Sample, Val)
  }
  d
}

integrateVars <- function(x, append=TRUE){
  lev <- unique(c("integrated", levels(x$Var)))
  dots <- names(x)[!names(x) %in% c("Var", "GCM", "Month", "Sample", "Val")]
  y <- x %>% group_by_(.dots=c(dots, "Var", "Sample")) %>% 
    mutate(Val=(Val-mean(Val))/sd(Val)) %>%
    group_by_(.dots=c(dots, "Month", "GCM", "Sample")) %>% 
    summarise(Var="integrated", Val=mean(Val)) %>% ungroup %>%
    mutate(Var=factor(Var, levels=lev))
  if(append) bind_rows(x, y) else y
}

.standardize <- function(x, grp, keep.monthly=FALSE, rescale=TRUE, zeromin=FALSE){
  x <- filter(x, Group %in% grp)
  if(rescale) x <- group_by(x, Var) %>% mutate(Val=(Val-mean(Val))/sd(Val))
  var <- unique(x$Var)
  if(keep.monthly){
    x <- group_by(x, Month, GCM, Composite, Group)
  } else x <- group_by(x, GCM, Composite, Group)
  x <- summarise(x, Val=mean(Val))
  if(keep.monthly){
    x <- group_by(x, Month, GCM)
  } else x <- group_by(x, GCM)
  x <- arrange(x, Composite) %>% ungroup
  if(zeromin) x$Val <- x$Val - min(x$Val)
  x
}

error_scale <- function(x, keep.monthly=FALSE, rescale=TRUE, offset=FALSE, zeromin=FALSE){
  y <- .standardize(x, c("Random", "Selected"), keep.monthly, rescale, zeromin)
  x <- .standardize(x, c("Individual"), keep.monthly, rescale, zeromin) 
  if(offset) x$Val <- x$Val - x$Val[1] + filter(y, Composite==1 & Group=="Selected")$Val
  bind_rows(x, y)
}

gcmPrep <- function(x, type, offset=TRUE, zeromin=FALSE){
  var <- unique(x$Var)
  nvar <- length(var)
  types <- c("spatial bootstrap", "random ensembles")
  if(!type %in% types)
    paste0("'type' must be one of '", types[1], "' or '", types[2], "'.")
  if(length(type) > 1) stop("must provide only one 'type'.")
  re <- type==types[2]
  keep.monthly <- if(re) FALSE else TRUE
  rescale <- if(nvar > 1) TRUE else FALSE
  if(nvar > 1 | offset | re) x <- error_scale(x, keep.monthly, rescale, offset, zeromin)
  if(re) return(x)
  x1 <- filter(x, Group=="Individual")
  x2 <- filter(x, Group=="Selected")
  lev <- rev(unique(x1$GCM))
  x1 <- mutate(x1, GCM=factor(GCM, levels=lev))
  x2 <- mutate(x2, GCM=factor(GCM, levels=lev))
  x <- filter(x, Group=="Random") %>% group_by(Group, Month, Composite) %>% 
    summarise(Val=mean(Val))
  bind_rows(x1, x2, x)
}
