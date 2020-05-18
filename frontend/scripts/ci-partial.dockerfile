FROM plazaproject/ci-base-frontend:410be9260b3be635dfcceb5c85aed7173aca649f as builder

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
