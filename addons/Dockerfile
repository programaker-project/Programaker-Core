FROM node:alpine as programaker-addon-ci-base
RUN apk add make zip
RUN mkdir /app
WORKDIR /app

# Build dependencies
ADD package.json /app
ADD package-lock.json /app
RUN npm install .

# Build final app
FROM programaker-addon-ci-base as builder
ENTRYPOINT make
