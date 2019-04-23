

needed_packages=rsconnect::appDependencies()$package
installed=rownames(installed.packages())
needed_packages=needed_packages[!needed_packages%in%installed]
install.packages(needed_packages)
