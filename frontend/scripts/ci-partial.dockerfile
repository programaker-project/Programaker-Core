FROM programakerproject/ci-base-frontend:b0fcf3b17c87060f72aae4407c9789925bcfd668 as builder

# Prepare dependencies
ADD . /app
RUN npm install . && make

# Build application
ARG BUILD_COMMAND=build-prod
RUN npm run ${BUILD_COMMAND}

# Copy final app to runner
FROM nginx:alpine as runner

copy --from=builder /app/dist/ /usr/share/nginx/html/

# Add nginx configuration
ADD config/simple-nginx.conf /etc/nginx/conf.d/default.conf

# Webserver port
EXPOSE 80
