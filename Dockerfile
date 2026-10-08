# Stage 1: build the Elm frontend.
# node:22-bookworm-slim ships without a system CA store, which the Elm compiler
# needs when fetching packages from package.elm-lang.org.
FROM docker.io/library/node:22-bookworm-slim AS frontend
RUN apt-get update \
    && apt-get install -y --no-install-recommends ca-certificates \
    && rm -rf /var/lib/apt/lists/*
RUN npm install --global elm@0.19.1-6

WORKDIR /build
COPY elm/elm.json /build/elm/elm.json
COPY elm/src /build/elm/src
WORKDIR /build/elm
RUN elm make src/Main.elm --output /build/charsheet.js

# Stage 2: runtime image with SWI-Prolog.
FROM docker.io/library/swipl:stable AS app
WORKDIR /app
COPY . /app/
COPY --from=frontend /build/charsheet.js /app/static/js/charsheet.js
RUN mkdir -p /app/storage
ENV CHARSHEET_PORT=8000
EXPOSE 8000
CMD ["swipl", "--quiet", "docker-entrypoint.pl"]
