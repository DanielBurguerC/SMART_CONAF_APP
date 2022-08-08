my_icon <- function (name, class = NULL, lib = "font-awesome") {
  require(htmltools)
  prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
  prefix <- prefixes[[lib]]
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ", 
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  iconClass <- ""
  if (!is.null(name)) {
    prefix_class <- prefix
    #if (prefix_class == "fa" && name %in% font_awesome_brands) {
    #  prefix_class <- "fab"
    #}
    iconClass <- paste0(prefix_class, " ", prefix, "-", name)
  }
  if (!is.null(class)) 
    iconClass <- paste(iconClass, class)
  iconTag <- tags$i(class = iconClass)
  if (lib == "font-awesome") {
    htmlDependencies(iconTag) <- htmltools::htmlDependency("font-awesome", 
                                                "5.7.2", "./www/iconos/", 
                                                stylesheet = c("css/all.min.css"))
  }
  htmltools::browsable(iconTag)
}

