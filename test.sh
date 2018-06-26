#!/bin/bash -ex

echo "[INFO] Building BACKEND"
pushd backend
time rebar3 compile
echo "[INFO] Testing BACKEND"
time rebar3 eunit -c
time rebar3 cover -v
popd

echo "[INFO] Building FRONTEND"
pushd frontend
make
npm install .
npm build
npm lint
