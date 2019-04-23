# Known errors

## On boot

### Corrupted mnesia DB

An error like the following is shown on boot:

```erlang
 {'EXIT',
     {{badmatch,
          {error,
              {no_exists,
                  automate_service_registry_services_table},
              {"Tried to perform op on non-existing (non alive) item",
               automate_service_registry_services_table}}},
 ...
```

This means that the Mnesia database is corrupted and the application cannot boot (this happens when using Ctrl-C to stop the application).

In some of these cases, stopping the process and starting it again will be enought to fix it.
On other cases, removing Mnesia's LATEST.LOG file will be needed for booting up again. If Plaza was launcher with the docker-compose script, this file will be located on `/var/lib/docker/volumes/plaza_plaza-dev-backend-mnesia/_data`.
**Note:** Take into account that if that file takes more than 97 bytes, some of the latest transactions might be lost.
