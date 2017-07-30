RSVP
====

## deploying

Deploys are handled by `docker` and images are created with `stack`

```sh
make publish
```

this will:
- create a new docker container
- push it to docker hub
- copy the hidden environment files to the container

from here, logging in to the `doc` user on the box and running the restart
script should reload the site.

