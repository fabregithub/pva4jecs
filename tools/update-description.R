# Generate DESCRIPTION from package metadata.
# Run from the package root with:
#   source("tools/update-description.R")

stopifnot(file.exists("NEWS.md"))

news <- readLines("NEWS.md", warn = FALSE)
news_version <- grep("^#\\s+pva4jecs\\s+", news, value = TRUE)
version <- if (length(news_version) > 0) {
  sub("^#\\s+pva4jecs\\s+", "", news_version[[1]])
} else {
  "0.1.0"
}

maintainer_email <- Sys.getenv("PVA4JECS_MAINTAINER_EMAIL", unset = "your.email@example.com")
if (identical(maintainer_email, "your.email@example.com")) {
  warning(
    "Set PVA4JECS_MAINTAINER_EMAIL before release, e.g. ",
    "Sys.setenv(PVA4JECS_MAINTAINER_EMAIL = 'name@example.org').",
    call. = FALSE
  )
}

authors_at_r <- paste0(
  "c(person(given = \"Shoji F.\", family = \"Nakayama\", ",
  "role = c(\"aut\", \"cre\"), email = \"", maintainer_email, "\"))"
)

roxygen_note <- if (requireNamespace("roxygen2", quietly = TRUE)) {
  as.character(utils::packageVersion("roxygen2"))
} else {
  NULL
}

fields <- list(
  Package = "pva4jecs",
  Type = "Package",
  Title = "Polytopic Vector Analysis",
  Version = version,
  `Authors@R` = authors_at_r,
  Description = paste(
    "Provides tools for polytopic vector analysis (PVA), including data",
    "transformation, end-member selection, oblique rotation, back-transformation,",
    "model diagnostics, plotting, and example datasets. The package was created",
    "as an R translation of Python code for PVA developed by Dr. Yang Ju."
  ),
  License = "MIT + file LICENSE",
  Encoding = "UTF-8",
  LazyData = "true",
  Roxygen = "list(markdown = TRUE)",
  RoxygenNote = roxygen_note,
  Depends = "R (>= 3.5)",
  Imports = paste(
    "dplyr",
    "ggplot2",
    "ggpubr",
    "rlang",
    "tidyr",
    sep = ",\n    "
  ),
  Suggests = paste(
    "knitr",
    "rmarkdown",
    "testthat (>= 3.0.0)",
    sep = ",\n    "
  ),
  `Config/testthat/edition` = "3"
)

fields <- fields[!vapply(fields, is.null, logical(1))]
write_description <- function(fields, file) {
  lines <- unlist(Map(function(name, value) {
    value <- as.character(value)
    parts <- strsplit(value, "\n", fixed = TRUE)[[1]]
    out <- paste0(name, ": ", parts[[1]])

    if (length(parts) > 1) {
      continuation <- vapply(parts[-1], function(line) {
        if (grepl("^\\s", line)) line else paste0("    ", line)
      }, character(1))
      out <- c(out, continuation)
    }

    out
  }, names(fields), fields), use.names = FALSE)

  writeLines(lines, con = file, useBytes = TRUE)
}

write_description(fields, file = "DESCRIPTION")
message("Wrote DESCRIPTION for pva4jecs ", version)
