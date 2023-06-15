my_packages = c("shiny", "tidyr", "readr", "dplyr", "ggplot2", "plotly", "leaflet", "shinydashboard", "rvest")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p)
  }
}

invisible(sapply(my_packages, install_if_missing))