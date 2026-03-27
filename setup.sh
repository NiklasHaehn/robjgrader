#!/usr/bin/env bash

# give public key: https://cran.r-project.org/bin/linux/ubuntu/README.html#secure-apt
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9

# add source
add-apt-repository 'deb https://cloud.r-project.org/bin/linux/ubuntu jammy-cran40/'

# update everything
apt-get update

# now do usual R installation
apt-get install -y libxml2-dev libcurl4-openssl-dev libssl-dev
apt-get install -y r-base

Rscript -e "install.packages('pak')"
Rscript -e "pak::pkg_install('json')"
Rscript -e "pak::pkg_install('tidyverse')"
Rscript -e "pak::pkg_install('scales')"
Rscript -e "pak::pkg_install('ggthemes')"
Rscript -e "pak::pkg_install('gt')"
Rscript -e "pak::pkg_install('modelsummary')"


