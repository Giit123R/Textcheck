# init.R
#
# Example R code to install packages if not already installed
#

my_packages = c("dplyr",
                "bslib",
                "httr",
                "jsonlite",
                "tokenizers",
                "plotly"
                )

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, clean=TRUE)
  }
}

invisible(sapply(my_packages, install_if_missing))