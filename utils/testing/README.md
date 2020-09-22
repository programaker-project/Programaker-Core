# Automatic API testing

Please note that this is a Work In Progress.
It's currently being used to perform some assertions over the permissions that the API takes in to account, and so it's useful to have it along the code. **But no refactoring has been done to this scripts after the first experiments with them, so don't read them expecting cleanliness.**

## Usage

There are two main scripts in this folder, `gen_test_api_plan.py` and `run_plan.py`.

 - `gen_test_api_plan.py` takes an API permission matrix (the one on [docs/test-table.csv](../../docs/test-table.csv)) and generates a [Graphviz] graph describing a plan that can be "executed" to check that a given API server complies with the permissions described on the file. By default the plan is written to the `plan.gv` file, and can be visualized by opening the `plan.svg` file with a browser or a SVG viewer.
 - `run_plan.py` takes the `plan.gv` file created by the previous script and runs it over a given API server. It will generate a `results.svg` file as a visualization of the tests performed (red boxes indicate failed tests, blue ones indicate ignored and green ones indicate passed ones). It takes into account the `API_TEST_ROOT` and `API_TEST_DOCKER` environment variables.
   - `API_TEST_ROOT`: Points to the API root of the tested API server (default: `http://localhost:8881/api/v0`).
   - `API_TEST_DOCKER`: Refers to the name of the Docker container where the tests are to be performed (`back-test-docker`).

Note that the execution of the `run_plan.py` script will make a significant number of API calls in a short amount of time (>750 in ~10 seconds at the time of writting this), will require Docker-exec access to the server, and won't cleanup the resources created in the process, so it's preferable to not use a shared server as target.**

Consider using the `run-api-test.sh` shell script to launch a local Docker container and run there the tests.

Access to the Docker container is needed to promote users to admins, which cannnot be done through the API.

[Graphviz]: https://graphviz.org/
