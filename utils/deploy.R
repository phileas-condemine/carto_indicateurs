rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Cartographie_des_indicateurs",
                     launch.browser = F,
                     account = "drees",forceUpdate = T
)

rsconnect::deployApp(appFileManifest = "manifest.txt",
                     appName = "Cartographie_des_indicateurs_dev",
                     launch.browser = T,
                     account = "drees",forceUpdate = T
)


sapply(readLines("manifest.txt"),
       function(x)gdata::humanReadable(file.size(x),standard="SI",units="kB"))
gdata::humanReadable(sum(sapply(readLines("manifest.txt"),
                                function(x)file.size(x))),
                     standard="SI",units="kB")
