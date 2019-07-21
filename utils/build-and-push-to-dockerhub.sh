#!/usr/bin/env bash

set -eu

cd "$(dirname "$0")"

# Frontend
cd ../frontend
docker build . -t plazaproject/plaza-core-frontend:`git rev-parse HEAD`
docker push plazaproject/plaza-core-frontend:`git rev-parse HEAD`

# Backend
cd ../backend
docker build . -t plazaproject/plaza-core-backend:`git rev-parse HEAD`
docker push plazaproject/plaza-core-backend:`git rev-parse HEAD`

