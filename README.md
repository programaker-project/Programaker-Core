# What is Plaza?

Plaza is the project behind [PrograMaker](https://programaker.com). It has the goal of enabling anyone to build anything, without the need for code, servers or technical expertise.

Plaza programs are not run on your computer. Thus, it is especially suited for simple tasks that don't require a lot of computing power but that must run contiguously, for example:
* Chat bots
* Connections between services
* Scheduled tasks

Plaza is programmed using MIT's Scratch language. Through it, and Plaza's distributed computer, the steps to create a new program are:
* Open a new program in your web browser
* Configure the program steps
* Press run

[(We are looking for other environments to learn from, tell us about your favourite visual automation tools!)](mailto:contact@programaker.com)

## Bridges

Plaza bridges are the components that connect the Plaza platform with external services and devices. This is a list of some bridges in no particular order.

| Name                                                                                | Maturity            | Language    | Description                                                    | License                                                                                                |
|-------------------------------------------------------------------------------------|---------------------|-------------|----------------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| Hue lights [[repo](https://gitlab.com/adri1177/hue-lights-bridge)]                  | Usable/Experimental | Python      | Bridge for Phillips Hue lights                                 | [Apache License 2.0](https://gitlab.com/adri1177/hue-lights-bridge/blob/master/LICENSE)                |
| AEMET [[repo](https://gitlab.com/plaza-project/bridges/aemet-bridge)]               | Usable/Experimental | Python      | Bridge for Spanish Weather Agency predictions.                 | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/aemet-bridge/blob/master/LICENSE)        |
| Meteogalicia [[repo](https://gitlab.com/plaza-project/bridges/meteogalicia-bridge)] | Usable/Experimental | Python      | Bridge for Galician weather predictions.                       | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/meteogalicia-bridge/blob/master/LICENSE) |
| Twitter bridge [[repo](https://gitlab.com/plaza-project/bridges/twitter-bridge)]    | In development      | Python      | Bridge to read data from Twitter.                              | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/twitter-bridge/blob/master/LICENSE)      |
| Toggl bridge [[repo](https://gitlab.com/plaza-project/bridges/toggl-bridge)]        | Usable/Experimental | Python      | Bridge to keep track of time on Toggl platform.                | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/toggl-bridge/blob/master/LICENSE)        |
| Telegram bridge [[repo](https://gitlab.com/plaza-project/bridges/telegram-bridge)]  | Usable/Experimental | Python      | Bridge to control bots on the Telegram IM network.             | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/telegram-bridge/blob/develop/LICENSE)    |
| Unix bridge [[repo](https://gitlab.com/plaza-project/bridges/unix-bridge)]          | Experimental        | Python/Bash | Library to write bridges using Unix tools (like bash scripts). | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/unix-bridge/blob/master/LICENSE)         |
| Matrix bridge [[repo](https://gitlab.com/plaza-project/bridges/matrix-bridge)]      | Usable/Experimental | Python      | Bridge for the Matrix IM network.                              | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/matrix-bridge/blob/master/LICENSE)       |
| XMPP bridge [[repo](https://gitlab.com/plaza-project/bridges/xmpp-bridge)]          | Experimental        | Python      | Bridge for the XMPP/Jabber IM network.                         | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/xmpp-bridge/blob/master/LICENSE)         |
| Gitlab bridge [[repo](https://gitlab.com/plaza-project/bridges/gitlab-bridge)]      | Experimental        | Python      | Bridge for the Gitlab plaform.                                 | [Apache License 2.0](https://gitlab.com/plaza-project/bridges/gitlab-bridge/blob/master/LICENSE)       |
| InfluxDB bridge [[repo](https://gitlab.com/kenkeiras/influxdb-bridge)]              | Usable/Experimental | Python      | Bridge for the InfluxDB time series database.                  | [Apache License 2.0](https://gitlab.com/kenkeiras/influxdb-bridge/blob/master/LICENSE)                 |

## Setup

### For development

#### Directly on the development machine

The services can be direcly run on a host machine.
This is the recommended way of developing the core as it can rely on angular capabilities for reloading.

##### Frontend

An updated [node.js](https://nodejs.org/) version is required for this. After getting it do the following:

* Go to the frontend directory: `cd frontend`
* Install dependencies: `npm install .`
* Download necessary css and font type resources using the Makefile: `make`
* Start the development server: `npm run start`

After this, the frontend can be accessed on http://localhost:4200

##### Backend

An updated version of [erlang](http://www.erlang.org/) and [rebar3](http://www.rebar3.org/) are needed for this.
After getting them do the following:

* Go to the backend directory: `cd backend`
* Get dependencies: `sh ./get-deps.sh`
* Run a rebar shell (which includes a server): `rebar3 shell`

After this, the backend is available on http://localhost:8888 (although the operation is done normaly through the frontend).

#### Docker compose

A [docker-compose](https://docs.docker.com/compose/overview/) script exists to setup a base deployment of Plaza.
This can be used to do some tests or as a help to develop bridges.
But keep in mind that a deployment launched with this script **has no redundancy** and **the data is not saved** between executions.

* To launch the services, run `docker-compose up --build`.
* To launch the services on the background, run `docker-compose up -d --build`. Same command to update.
* To stop the services, run `docker-compose down`.

After this, the service can be accessed on http://localhost:8080 .

## Architecture overview

![Image of the architecture overview](docs/architecture-overview.png)
