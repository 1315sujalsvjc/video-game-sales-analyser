FROM rocker/shiny:latest

RUN R -e "install.packages(c('shiny','readr','dplyr','tidyr','ggplot2','DT'), repos='https://cloud.r-project.org')"

COPY . /srv/shiny-server/

EXPOSE 3838

CMD ["sh", "-c", "R -e \"shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', '3838')))\""]