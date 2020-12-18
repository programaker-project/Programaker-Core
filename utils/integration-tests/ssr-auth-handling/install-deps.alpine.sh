#!/bin/sh

set  -eux

apk add bash go curl wget make unzip git jq links

go get github.com/gorilla/websocket
go get github.com/levigross/grequests
