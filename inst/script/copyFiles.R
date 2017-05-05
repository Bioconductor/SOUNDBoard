## Transfer CSS and Images to vignettes directory

## Run once
file.copy("inst/resources/images", "vignettes/.assets/", recursive = TRUE)

## Run when modifying original
file.copy("inst/resources/html/soundboard2.css", "vignettes/", overwrite = TRUE)
