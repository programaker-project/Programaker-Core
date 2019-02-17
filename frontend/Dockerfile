FROM node:alpine as builder

RUN mkdir /app
WORKDIR /app

# Build dependencies
ADD package.json /app
ADD package-lock.json /app
RUN npm install .

# Build final app
ADD . /app
RUN npm run build-prod

FROM nginx:alpine as runner

copy --from=builder /app/dist/ /usr/share/nginx/html/

