FROM fpco/stack-build:lts-11.2 as build
RUN mkdir /opt/log-classifier
COPY . /opt/log-classifier
RUN cd /opt/log-classifier
CMD ["stack build"]
