FROM rocker/shiny:3.5.1
MAINTAINER Marine Institute
# install ssl
# and gdal
RUN sudo apt-get update && apt-get install -y libssl-dev libudunits2-0 libudunits2-dev libproj-dev libgdal-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# install additional packages
RUN Rscript -e "install.packages(c('htmlwidgets','dplyr','plotly','leaflet','mapview'), repos='https://cran.rstudio.com/')"
## fixing running as non root
RUN sudo chown -R shiny:shiny /var/lib/shiny-server/
RUN Rscript -e "install.packages(c('shinythemes','shinydashboard','flexdashboard','ggridges','shinycssloaders','tidyverse'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN sudo apt-get update && apt-get install -y libprotobuf-dev protobuf-compiler libv8-3.14-dev libjq-dev && apt-get clean && rm -rf /var/lib/apt/lists/ && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN Rscript -e "install.packages(c('tidyr','geojsonio'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
RUN Rscript -e "install.packages(c('ggplot2','colorRamps','DT','reshape2'), repos='https://cran.rstudio.com/')" && rm -rf /tmp/downloaded_packages/ /tmp/*.rds
# copy shiny-server config file
#COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY www /srv/shiny-server/www
COPY .RData /srv/shiny-server/
COPY .Rhistory /srv/shiny-server/
COPY app.R /srv/shiny-server/
COPY ForecastingData.csv /srv/shiny-server/
COPY ForecastOptionsV2.csv /srv/shiny-server/
COPY google-analytics.js /srv/shiny-server/
COPY ["ICES-New-Old - extra species.csv", "/srv/shiny-server/"]
COPY Introduction.csv /srv/shiny-server/
COPY IntroductionTable.csv /srv/shiny-server/
COPY KeyPoints.csv /srv/shiny-server/
COPY ManagementAdvice.csv /srv/shiny-server/
COPY MixedFish.csv /srv/shiny-server/
COPY README.md /srv/shiny-server/

EXPOSE 3838
CMD ["/usr/bin/shiny-server.sh"]
