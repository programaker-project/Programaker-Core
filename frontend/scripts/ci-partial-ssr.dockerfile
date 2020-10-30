FROM programakerproject/ci-base-frontend:50e8c3f1a1fb0e7d24b7a42cf77421764e0f27e3 as builder

# Prepare dependencies
ADD . /app
RUN npm install . && make

# Build application
ARG BUILD_COMMAND=build:ssr
RUN npm run ${BUILD_COMMAND}

# Copy final app to runner
FROM node:lts-alpine as runner

COPY --from=builder /app/dist /app/dist

WORKDIR app

# Webserver port
ENV PORT 80
EXPOSE 80

CMD ["node", "/app/dist/programaker/server/main.js"]
