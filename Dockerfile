FROM rocker/rstudio:4.1.1

RUN git clone https://github.com/schalkdaniel/cacb-paper-bmr /home/rstudio/cacb-paper-bmr

RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e "remotes::install_version('BH', version = '1.75')"
RUN Rscript -e "remotes::install_version('here', version = '0.1')"
RUN Rscript -e "remotes::install_version('dplyr', version = '1.0.7')"
RUN Rscript -e "remotes::install_version('tidyr', version = '1.1.4')"
RUN Rscript -e "remotes::install_version('ggplot2', version = '3.3.5')"
RUN Rscript -e "remotes::install_version('gridExtra', version = '2.3')"
RUN Rscript -e "remotes::install_version('ggsci', version = '2.9')"
RUN Rscript -e "remotes::install_version('mlr3', version = '0.12.0')"
RUN Rscript -e "remotes::install_version('mlr3tuning', version = '0.8.0')"
RUN Rscript -e "remotes::install_version('mlr3learners', version = '0.5.0')"
RUN Rscript -e "remotes::install_version('mlr3pipelines', version = '0.3.5-1')"
RUN Rscript -e "remotes::install_version('reticulate', version = '1.18')"
RUN Rscript -e "remotes::install_version('paradox', version = '0.7.1')"
RUN Rscript -e "remotes::install_version('xgboost', version = '1.3.2.1')"
RUN Rscript -e "remotes::install_version('mboost', version = '2.9-4')"
RUN Rscript -e "remotes::install_version('mlr3oml', version = '0.5.0')"
RUN Rscript -e "remotes::install_version('mlr3hyperband', version = '0.1.2')"
RUN Rscript -e "remotes::install_version('batchtools', version = '0.9.15')"

RUN Rscript -e "remotes::install_github('mlr-org/mlr3extralearners', ref = 'df6b209d20a0e9416458fa3e7784830e655cd3ca')"
RUN Rscript -e "remotes::install_github('schalkdaniel/compboost', ref = 'c68e8fb32aea862750991260d243cdca1d3ebd0e', upgrade = 'never')"

RUN apt-get install python3
RUN apt-get update
RUN apt-get install -y python3-pip
RUN apt-get install -y python3-venv
RUN mkdir /home/rstudio/venv
RUN python3 -m venv /home/rstudio/venv/ebm
RUN . /home/rstudio/venv/ebm
RUN pip3 install pandas sklearn interpret

RUN echo "setwd('/home/rstudio/cacb-paper-bmr')" >> /home/rstudio/.Rprofile
