FROM alpine:latest

RUN apk add ca-certificates

ADD https://github.com/prometheus/blackbox_exporter/releases/download/v0.14.0/blackbox_exporter-0.14.0.linux-amd64.tar.gz blackbox_exporter.tar.gz
RUN tar xvf blackbox_exporter.tar.gz && mv -v blackbox_exporter-0.14.0.linux-amd64/blackbox_exporter /usr/bin/blackbox_exporter && rm -Rfv blackbox_exporter-0.14.0.linux-amd64 blackbox_exporter.tar.gz
ADD config.yml /etc/blackbox_exporter/config.yml

ENTRYPOINT [ "blackbox_exporter" ]
CMD [ "--config.file=/etc/blackbox_exporter/config.yml" ]
