FROM plazaproject/ci-base-frontend:f3fbff16ad75212c3f51d8c854c098461453d046 as builder
ADD . /app
RUN npm install . && make
RUN npm run build-prod

# Copy final app to runner
FROM nginx:alpine as runner
copy --from=builder /app/dist/ /usr/share/nginx/html/
