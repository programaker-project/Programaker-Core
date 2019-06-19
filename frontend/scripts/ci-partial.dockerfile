FROM plazaproject/ci-base-frontend:2a4e818161c1ea7cf6d2d5c7874523b5545684f6 as builder
ADD . /app
RUN npm install . && make
RUN npm run build-prod

# Copy final app to runner
FROM nginx:alpine as runner
copy --from=builder /app/dist/ /usr/share/nginx/html/
