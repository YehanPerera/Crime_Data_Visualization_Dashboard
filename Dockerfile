FROM rocker/shiny

ARG API_KEY

ENV API_KEY=${API_KEY}

RUN mkdir /home/shiny-app

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cloud.r-project.org'))"

WORKDIR /home/shiny-app/

RUN mkdir -p renv
COPY app.R app.R
COPY renv.lock renv.lock
COPY .Rprofile  .Rprofile
COPY renv/activate.R renv/activate.R
COPY renv/settings.json renv/settings.json
COPY .RData .RData

RUN R -e "renv::restore()"

VOLUME ["/home/shiny-app"]

EXPOSE 8180

CMD R -e "shiny::runApp('/home/shiny-app/app.R', host='0.0.0.0', port=8180)"