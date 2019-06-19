FROM nginx:alpine as runner

# Add nginx configuration
ADD config/router.conf /etc/nginx/conf.d/default.conf.orig

# Webserver port
EXPOSE 80

CMD ["ash", "-c", "envsubst '${FRONTEND_NODE} ${FRONTEND_PORT} ${BACKEND_NODE} ${BACKEND_PORT}' < /etc/nginx/conf.d/default.conf.orig > /etc/nginx/conf.d/default.conf && nginx -g 'daemon off;'"]
