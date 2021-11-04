FROM rocker/rstudio:latest

RUN git clone https://github.com/schalkdaniel/cacb-paper-bmr /home/rstudio/cacb-paper-bmr
RUN export R_HOME='/home/rstudio/cacb-paper-bmr/'

RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e "install.packages(c('dplyr', 'tidyr', 'ggplot2'))"
RUN Rscript -e "install.packages(c('processx', 'callr', 'mlr3', 'mlr3tuning', 'mlr3learners', 'mlr3pipelines', 'reticulate', 'paradox', 'xgboost', 'ranger', 'mboost', 'mlr3oml', 'reticulate', 'mlr3hyperband'))"
RUN Rscript -e "remotes::install_github('mlr-org/mlr3extralearners')"
RUN Rscript -e "remotes::install_github('schalkdaniel/compboost", ref = "c68e8fb32aea862750991260d243cdca1d3ebd0e')"

RUN apt-get install python3 python3-pip python3-venv
RUN mkdir /home/rstudio/venv
RUN python3 -m venv /home/rstudio/venv/ebm
RUN source /home/rstudio/venv/ebm
RUN pip3 install pandas sklearn interpret
