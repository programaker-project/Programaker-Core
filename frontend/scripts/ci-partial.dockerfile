FROM plazaproject/ci-base-frontend:3af9f6978093bd6b63b9c9882a0044c5d4af2c19 as builder

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
