FROM plazaproject/ci-base-frontend:f11105a784435cb05241da6ee0b3c410bc39870c as builder
ADD . /app
RUN npm install . && make
RUN npm run build-prod

# Copy final app to runner
FROM nginx:alpine as runner

copy --from=builder /app/dist/ /usr/share/nginx/html/

# Add nginx configuration
ADD config/simple-nginx.conf /etc/nginx/conf.d/default.conf

# Webserver port
EXPOSE 80