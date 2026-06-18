# 1. Open the package root
# setwd("path/to/pva4jecs")

# 2. Generate DESCRIPTION automatically
Sys.setenv(PVA4JECS_MAINTAINER_EMAIL = "fabre@nies.go.jp")
source("tools/update-description.R")

# 3. If you moved CSV files to data-raw/, regenerate package data
# Only run this if you have a data-raw script.
# source("data-raw/generate-data.R")

# 4. Load package during development
devtools::load_all()

# 5. Generate documentation from roxygen comments
devtools::document()

# 6. Render README.Rmd into README.md
devtools::build_readme()

# 7. Run tests
devtools::test()

# 8. Run package check
devtools::check()
