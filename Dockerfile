FROM rocker/shiny:4.2.3

USER root

# Install minimal system libraries for compilation
# Added libgdal-dev, libudunits2-dev, pkg-config, and cmake for R package dependencies
RUN apt-get update -qq && \
    apt-get install -y --no-install-recommends \
    libcairo2-dev \
    libpng-dev \
    libjpeg-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libgdal-dev \
    libudunits2-dev \
    pkg-config \
    cmake \
    build-essential \
    gfortran && \
    rm -rf /var/lib/apt/lists/*

# Attempt to remove any existing vctrs to prevent version conflicts
# Use try() to ensure the build doesn't fail if vctrs isn't initially present
RUN R -e 'try(remove.packages("vctrs"), silent = TRUE)'

# Install/Update vctrs to ensure other packages get a compatible version
# A newer version (>=0.6.3) is required by subsequent packages.
RUN R -e 'install.packages("vctrs", repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'


# Attempt to remove any existing rlang to prevent version conflicts
# Use try() to ensure the build doesn't fail if rlang isn't initially present
RUN R -e 'try(remove.packages("rlang"), silent = TRUE)'

# Install/Update rlang to ensure other packages get a compatible version
# A newer version (>=1.1.1) is required by subsequent packages.
RUN R -e 'install.packages("rlang", repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'


# Attempt to remove any existing htmltools to prevent version conflicts
# Use try() to ensure the build doesn't fail if htmltools isn't initially present
RUN R -e 'try(remove.packages("htmltools"), silent = TRUE)'

# Install/Update htmltools to ensure leaflet gets a compatible version
# Leaflet (or its dependencies) requires htmltools >= 0.5.7
RUN R -e 'install.packages(c("htmltools","jsonlite","httr"), repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'


# Install/Update Rcpp first to ensure 'terra' compiles against a recent version
# This is crucial because terra 1.8-50 seems to require Rcpp features
# that might be newer than what's in the base rocker/shiny:4.2.3 image.
RUN R -e 'install.packages("Rcpp", repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'


# Install R package: leaflet
# This command installs leaflet and its essential dependencies
# (those listed under Depends, Imports, LinkingTo on CRAN), including 'terra'.
# 'terra' will now compile against the Rcpp version installed above.
# It will NOT install packages listed under 'Suggests'.
RUN R -e 'install.packages("leaflet", repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'


RUN R -e 'install.packages("purrr", repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'
# If you have other R packages that are NOT dependencies of leaflet
# and you want to install them without their 'Suggests', you can add them similarly:
# RUN R -e 'install.packages(c("your_other_package1", "your_other_package2"), repos = "https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'

RUN R -e 'install.packages(c("dplyr", "lubridate", "tidyr"), repos="https://cloud.r-project.org", dependencies=c("Depends", "Imports", "LinkingTo"), lib=.Library)'


EXPOSE 3838
USER shiny
CMD ["shiny-server"]
