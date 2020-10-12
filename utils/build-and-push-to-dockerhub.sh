#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")"

# Frontend
cd ../frontend
docker build . -t programakerproject/programaker-core-frontend:`git rev-parse HEAD`
docker push programakerproject/programaker-core-frontend:`git rev-parse HEAD`

# Backend
cd ../backend
docker build . -t programakerproject/programaker-core-backend:`git rev-parse HEAD`
docker push programakerproject/programaker-core-backend:`git rev-parse HEAD`
