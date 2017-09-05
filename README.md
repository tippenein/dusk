DUSK
====

## deploying

Deploys are handled by `docker` and images are created with `stack`

```sh
make
```

this will:
- run tests
- build frontend bundle and move to `static/js/bundle.js`
- build the api
- containerize it and push it to docker hub
- copy the hidden environment files to the container

from here, logging in to the `doc` user on the box and running the restart
script should reload the site.

Obviously that sucks and should be replaced with real devops eventually
