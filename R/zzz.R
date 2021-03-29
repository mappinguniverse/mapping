
mappingStartupMessage <- function()
{
#   msg <- c(paste0(
# "mapping version"," ",(packageVersion("mapping")),"\n", "Automatic static and dynamic maps for worldwide, european, USA,  and italian map"), "\n", "Type 'citation(\"mapping\")' for citing this R package in publications.")

  msg <- c(paste0(paste0(rep("#", 100), collapse = ""), "\n",
    "Welcome in mapping ", (packageVersion("mapping")), " "), "\n", "Type 'citation(\"mapping\")' for citing this R package in publications.")


  return(msg)
}

.onAttach <- function(lib, pkg)
{
  unlockBinding(".mapping", asNamespace("mapping"))
  # startup message
  msg <- mappingStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'mapping' version", packageVersion("mapping"))
  packageStartupMessage(msg)
  invisible()
}


