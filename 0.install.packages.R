############################################################
# Summer School: Network Analysis in R
# Part 0: Install packages
############################################################

# Install required packages
install.packages(c(
  "readr",
  "dplyr",
  "tidyr",
  "tibble",
  "remotes"
))

# Install the version of bipartite used in this practical
remotes::install_version(
  "bipartite",
  version = "2.24",
  repos = "https://cloud.r-project.org",
  dependencies = TRUE,
  upgrade = "never"
)

# Check the installed version
packageVersion("bipartite")