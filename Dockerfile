FROM rocker/r-ubuntu:20.04

LABEL maintainer="aikia <aikia.update@gmail.com>"

RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libmariadb-dev \
    libquantlib0-dev \
    && rm -rf /var/lib/apt/lists/*

# libmysqlclient seems to need an update before installing.
RUN apt-get update && apt-get install -y libmysqlclient-dev

COPY Rprofile.site /etc/R
COPY ld.so.conf /etc
COPY my.cnf /etc
ENV _R_SHLIB_STRIP_=true

RUN sudo ldconfig

RUN install.r shiny tidyverse forecast jsonlite htmltools DBI \ 
    RMariaDB devtools remotes plotly fs RQuantLib \
    scales gt shinydashboard bizdays here patchwork ggtext heatmaply \
    metathis shinydashboardPlus
    
RUN Rscript -e "remotes::install_github(c('daattali/shinycssloaders', 'gadenbuie/shinyThings','ebailey78/shinyBS','igraph/rigraph@master'))"
RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app
COPY app .

RUN chown app:app -R /home/app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]
