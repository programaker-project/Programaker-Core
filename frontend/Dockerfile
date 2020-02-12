FROM node:lts-alpine as ci-base
RUN apk add --no-cache make python2 g++
RUN mkdir /app
WORKDIR /app

# Build dependencies
ADD package.json /app
ADD package-lock.json /app
RUN npm install --unsafe-perm

# Build final app
FROM ci-base as builder
ADD . /app
RUN make
RUN npm run build-prod

# Copy final app to runner
FROM nginx:alpine as runner

COPY --from=builder /app/dist/ /usr/share/nginx/html/

# Add nginx configuration
ADD config/simple-nginx.conf /etc/nginx/conf.d/default.conf

# Webserver port
EXPOSE 80
