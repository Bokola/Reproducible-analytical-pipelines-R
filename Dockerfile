FROM rocker/r-ver:4.3.2

RUN apt-get update && apt-get install -y \
    libglpk-dev \
    libxml2-dev \
    libcairo2-dev \
    libgit2-dev \
    default-libmysqlclient-dev \
    libpq-dev \
    libsasl2-dev \
    libsqlite3-dev \
    libssh2-1-dev \
    libxtst6 \
    libcurl4-openssl-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libxt-dev \
    unixodbc-dev \
    wget \
    pandoc





#RUN mkdir /home/rapR

# create a volume to store outputs of runtime

#RUN mkdir /home/rapR/pipeline_output

#RUN mkdir /home/rapR/shared_folder

#COPY renv.lock /home/rapR/renv.lock

#COPY analyse_data.Rmd /home/rapR/analyse_data.Rmd

#COPY _targets.R /home/rapR/_targets.R

#RUN R -e "setwd('/home/rapR')"

RUN R -e "install.packages(c('remotes'))"
RUN R -e "remotes::install_github('rstudio/renv@v1.0.11')"

#RUN R -e "install.packages(c('janitor', 'targets', 'tarchetypes', 'ggplot2', 'here', 'rlang', 'tidyr', 'purrr', 'fusen', 'testthat', 'usethis'), dep = T)"

#RUN R -e "remotes::install_github('Bokola/Reproducible-analytical-pipelines-R@fussen', ref = '08537708c68d75a8a00491e8fd2a5b33d4a6b8c4', force = TRUE)"

#RUN R -e "renv::init()"
#RUN R -e "renv::restore()"


#RUN cd /home/rapR
#RUN R -e "targets::tar_make()"

#CMD mv /home/rapR/pipeline_output/* /home/rapR/shared_folder/

