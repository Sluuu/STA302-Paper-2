FROM justinian336/lighthergm:v1.1.0

RUN mkdir -p /home/rstudio/.local/share/renv/cache \
    && chown -R rstudio:rstudio /home/rstudio \
    && apt install libharfbuzz-dev libfribidi-dev -y

RUN R -e 'options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/cran/2022-12-01"));\
    install.packages(c("renv"))'

WORKDIR /home/rstudio/aea2023
