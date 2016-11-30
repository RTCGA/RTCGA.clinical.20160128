.onLoad <- function(libname, pkgname) {
    titles <- read.csv(system.file("extdata", "metadata.csv", 
                      package = "RTCGA.clinical.20160128"), 
                      stringsAsFactors=FALSE)$Title
    rda <- gsub(".rda", "", titles, fixed=TRUE)
    if (!length(rda))
        stop("no .rda objects found in metadata")

    ## Functions to load resources by name:
    ns <- asNamespace(pkgname)
    sapply(rda, 
        function(xx) {
            func = function(metadata = FALSE) {
                if (!isNamespaceLoaded("ExperimentHub"))
                    attachNamespace("ExperimentHub")
                eh <- AnnotationHub::query(ExperimentHub(),  "RTCGA.clinical.20160128" )
                ehid <- names(AnnotationHub::query(eh, xx))
                if (!length(ehid))
                    stop(paste0("resource ", xx, 
                         "not found in ExperimentHub"))
                if (metadata)
                    eh[ehid]
                else eh[[ehid]]
            }
            assign(xx, func, envir=ns)
            namespaceExport(ns, xx)
        })
}