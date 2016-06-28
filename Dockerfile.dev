FROM elixir:1.3

RUN apt-get update -qq \
    && apt-get install -y inotify-tools entr\
    && apt-get purge --auto-remove -y \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV APP_ROOT /code

WORKDIR ${APP_ROOT}

VOLUME ["${APP_ROOT}"]

RUN mix local.hex --force

CMD ["mix"]
