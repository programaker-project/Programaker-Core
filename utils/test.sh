#!/bin/bash

set -e

cd "$(dirname "$0")/.."

set -x

echo "[INFO] Building BACKEND"
pushd backend
time rebar3 compile
echo "[INFO] Testing BACKEND"
time rebar3 eunit -c
time rebar3 cover -v
time rebar3 dialyzer
popd

echo "[INFO] Building FRONTEND"
pushd frontend
npm install .
make
npm run build
npm run lint
popd

echo "[INFO] Building addon"
pushd addons
npm install .
npm run build
npm run lint
make dist/programaker.xpi
popd
