library(rsconnect)
rsconnect::setAccountInfo("your shiny web tokens")
options(encoding = "UTF-8")
rsconnect::deployApp("your folder path for web construction")

