# What is Plaza?

Plaza is a project to build an easily programmable, distributed computer that can run your programs on a fault-resistant manner.

Plaza programs are not run on your computer. Thus, it is especially suited for simple tasks that don't require a lot of computing power but that must run contiguously, for example:
* Chat bots
* Connections between services
* Scheduled tasks

Plaza is programmed using MIT's Scratch language. Through it, and Plaza's distributed computer, the steps to create a new program are:
* Open a new program in your web browser
* Configure the program steps
* Press run

[(We are looking for other environments to learn from, tell us about your favourite visual automation tools!)](mailto:plaza@spiral.systems)

## Setup

### For development

#### Docker compose

A [docker-compose](https://docs.docker.com/compose/overview/) script exists to setup a base deployment of Plaza.
This can be used to do some tests or as a help to develop bridges.
But keep in mind that a deployment launched with this script **has no redundancy** and **the data is not saved** between executions.

* To launch the services, run `docker-compose up`.
* To launch the services on the background, run `docker-compose up -d`.
* To stop the services, run `docker-compose down`.

After this, the service can be accessed on http://localhost:8080 .

#### Directly on the development machine

The services can be direcly run on a host machine.
This is the recommended way of developing the core as it can rely on angular capabilities for reloading.

##### Frontend

An updated [node.js](https://nodejs.org/) version is required for this. After getting it do the following:

* Go to the frontend directory: `cd frontend`
* Install dependencies: `npm install .`
* Start the development server: `npm run start`

After this, the frontend can be accessed on http://localhost:4200

##### Backend

An updated version of [erlang](http://www.erlang.org/) and [rebar3](http://www.rebar3.org/) are needed for this.
After getting them do the following:

* Go to the backend directory: `cd backend`
* Run a rebar shell (which includes a server): `rebar3 shell`

After this, the backend is available on http://localhost:8888 (although the operation is done normaly through the frontend).

## Architecture overview

![Image of the architecture overview](docs/architecture-overview.png)
