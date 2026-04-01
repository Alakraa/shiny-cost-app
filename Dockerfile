FROM rocker/shiny:4.3.3

RUN R -e "install.packages(c('shiny','readr','dplyr'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
