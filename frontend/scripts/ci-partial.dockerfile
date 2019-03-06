FROM plazaproject/ci-base-frontend:ccce8f23a5de5dbe040636e04818e6c12f7301b7 as builder
ADD . /app
RUN make
RUN npm run build-prod

# Copy final app to runner
FROM nginx:alpine as runner
copy --from=builder /app/dist/ /usr/share/nginx/html/
