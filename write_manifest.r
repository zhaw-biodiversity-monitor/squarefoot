

appfiles <- c(
    c("ui.R","server.R","utils.R", "libraries.R"),
    list.files("appdata", full.names = TRUE)
)

rsconnect::writeManifest(appDir = getwd(), appfiles)
