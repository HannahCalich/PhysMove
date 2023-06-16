
pkgwd <- getwd()
setwd(paste0(pkgwd,'/vignettes'))
files <- list.files(pattern = "\\.Rmd")
filename <- strsplit(files, ".Rmd")
i <- 1
for (i in 1:length(files)) {
  rmarkdown::render(files[i], output_file = paste(filename[i],'_', Sys.Date(),'.docx', sep=''))
}

setwd(pkgwd)
