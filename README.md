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

A [docker-compose](https://docs.docker.com/compose/overview/) script exists to setup a base deployment of Plaza.
This can be used to do some tests or as a help to develop parts of the core or bridges.
But keep in mind that a deployment launched with this script **has no redundancy** and **the data is not saved** between executions.

* To launch the services, run `docker-compose up`.
* To launch the services on the background, run `docker-compose up -d`.
* To stop the services, run `docker-compose down`.

## Architecture overview

![Image of the architecture overview](docs/architecture-overview.png)
