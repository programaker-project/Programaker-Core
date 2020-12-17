FROM programakerproject/ci-base-frontend:50e8c3f1a1fb0e7d24b7a42cf77421764e0f27e3 as builder

# Prepare dependencies
ADD . /app
RUN npm install . && make

# Build application
ARG BUILD_COMMAND=build-prod
RUN npm run ${BUILD_COMMAND}

# Copy final app to runner
FROM nginx:alpine as runner

copy --from=builder /app/dist/programaker/browser /usr/share/nginx/html/

# Add nginx configuration
ADD config/simple-nginx.conf /etc/nginx/conf.d/default.conf

# Webserver port
EXPOSE 80
