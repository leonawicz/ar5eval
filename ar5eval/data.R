rv <- reactiveValues(d=NULL)
rds.env <- .GlobalEnv
local_cache <- reactive({ paste0("rdsdata_", stat(), "_", dom0) })

load_file <- function(file, source="local", id=NULL, envir=.GlobalEnv){
  if(!is.null(id) && exists(id, envir=envir)){
    rv[["d"]] <- get(id, envir=envir)
  } else {
    progress <- shiny::Progress$new()
    on.exit(progress$close())
  if(source=="local"){
    progress$set(message="Loading local data", value=1)
    d <- readRDS(file)
    progress$set(message="Loading local data", value=2)
  }
  if(source=="aws"){
    progress$set(message="Fetching AWS data", value=1)
    d <- s3readRDS(object=file)
    progress$set(message="Fetching AWS data", value=2)
  }
  rv[["d"]] <- d
  assign(id, d, envir=rds.env)
  }
}

updateData <- reactive({
  if(is.null(rdsfile())){
    rv[["d"]] <- NULL
  } else {
    load_file(rdsfile(), source=datasrc, id=local_cache(), envir=rds.env)
  }
})
observe({ updateData() })
