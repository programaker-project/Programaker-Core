FROM node:alpine as ci-base
RUN apk add make
RUN mkdir /app
WORKDIR /app

# Build dependencies
ADD package.json /app
ADD package-lock.json /app
RUN npm install .

# Build final app
FROM ci-base as builder
ADD . /app
RUN make
RUN npm run build-prod

# Copy final app to runner
FROM nginx:alpine as runner
copy --from=builder /app/dist/ /usr/share/nginx/html/

