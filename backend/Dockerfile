FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y --no-install-recommends \
    libxml2-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libpoppler-cpp-dev \
    poppler-utils \
    pandoc \
    zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY backend/install.R /app/install.R
RUN Rscript install.R

COPY backend/pipeline.R backend/plumber.R /app/

EXPOSE 8080

ENV R_ENVIRON=""
ENV KARION_ANALISIS_DIR="/data/analisis"

VOLUME /data/analisis

CMD ["R", "-e", "library(plumber); pr('/app/plumber.R') %>% pr_run(host='0.0.0.0', port=8080)"]
