FROM thoughtbot/heroku-haskell-stack

# ENV DEBIAN_FRONTEND noninteractive
# ENV LANG en_US.UTF-8
# ENV PATH $PATH:/root/.local/bin

# RUN mkdir -p /app/user
# WORKDIR /app/user

COPY stack.yaml .
RUN stack setup

COPY *.cabal ./
RUN stack build --dependencies-only

COPY . /app/user
RUN stack build
RUN stack install
RUN cp /root/.local/bin/* .
RUN rm -rf /app/user/.stack-work
