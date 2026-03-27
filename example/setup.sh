#!/usr/bin/env bash

# Add CRAN key and repository
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/'

# System dependencies
apt-get update
apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev libfontconfig1-dev \
                   libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
                   libtiff5-dev libjpeg-dev

# R base
apt-get install -y r-base

# Package manager
Rscript -e "install.packages('pak')"

# Student-facing packages (install these first so Robjgrader can suggest them)
Rscript -e "pak::pkg_install(c('tidyverse', 'scales', 'ggthemes'))"
Rscript -e "pak::pkg_install(c('gt', 'modelsummary', 'fixest', 'lme4'))"

# Robjgrader and its hard dependencies
Rscript -e "pak::pkg_install('NiklasHaehn/robjgrader')"

# LLM text grading (needed for validate_text())
Rscript -e "pak::pkg_install('httr2')"
