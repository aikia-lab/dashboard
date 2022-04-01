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
    && rm -rf /var/lib/apt/lists/*

COPY Rprofile.site /etc/R
ENV _R_SHLIB_STRIP_=true

RUN install.r shiny forecast jsonlite ggplot2 htmltools DBI RMariaDB devtools remotes plotly
#RUN Rscript -e "install.packages('plotly','forecast', 'jsonlite', 'ggplot2', 'htmltools', 'tidyquant', 'DBI', 'RMySQL', 'devtools', 'remotes', 'shinycssloaders', 'plotly')"
RUN Rscript -e "remotes::install_github('daattali/shinycssloaders')"

RUN echo "local(options(shiny.port = 3838, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site

RUN addgroup --system app && adduser --system --ingroup app app
WORKDIR /home/app
COPY app .

RUN chown app:app -R /home/app
USER app

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/home/app')"]