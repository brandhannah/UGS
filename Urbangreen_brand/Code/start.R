r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)


library(apollo)
library(tidyverse)
library(haven)
library(labelled)
library(dplyr)
library(tinytex)
library(tidylog)
library(devtools)
library(texreg)
library(janitor)

devtools::source_gist("1fda3215ee548d64d42b1db78f880ec5")