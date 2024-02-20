FROM r-base:4.2.2
RUN apt-get update
RUN apt-get install libcurl4-openssl-dev -y
RUN apt-get install libssl-dev -y

RUN apt-get install libharfbuzz-dev libfribidi-dev libfontconfig1-dev libssl-dev r-base-dev libgit2-dev libtiff-dev libssl-dev libxml2-dev libssl-dev -y


WORKDIR /DataVisualization
COPY Libraries.R .
RUN Rscript Libraries.R
COPY . .
EXPOSE 8180
CMD ["Rscript", "Preprocessing_Shiny.R"]


