# Stage 1: build frontend and backend release with the official Elixir image
FROM elixir:1.17.3 AS build

ARG TARGETPLATFORM
RUN echo "Building pareto-client for $TARGETPLATFORM"

# Install Hex and Rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Install Node.js and npm
RUN curl -sL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs

# Elm 0.19.2 provides official Linux x64 and ARM64 binaries.
RUN npm install -g elm@0.19.2-0 elm-land

# Set environment
ENV MIX_ENV=prod

# Set the working directory for the frontend build
WORKDIR /app/frontend

# Copy the frontend code to the container and run the build script
COPY frontend /app/frontend
# Commit SHA is not in the image (.git is not copied). Pass it from CI:
# GitHub Actions: --build-arg GIT_VERSION=${{ github.sha }}
# Render: ARG RENDER_GIT_COMMIT is filled from the service env automatically.
ARG GIT_VERSION
ARG GITHUB_SHA
ARG RENDER_GIT_COMMIT
ARG IMAGE_CACHING_SERVER
RUN chmod +x build.sh && \
    if [ -n "$IMAGE_CACHING_SERVER" ]; then IMAGE_CACHING_SERVER="$IMAGE_CACHING_SERVER" ./build.sh; else ./build.sh; fi

# Set the working directory for the backend build
WORKDIR /app/nostr_backend

COPY nostr_backend/mix.exs nostr_backend/mix.lock ./
RUN mix deps.get --only prod && mix deps.compile

# Copy the backend code to the container
COPY nostr_backend/assets ./assets
COPY nostr_backend/config ./config
COPY nostr_backend/lib ./lib
COPY nostr_backend/priv ./priv
COPY nostr_backend/test ./test

# Copy built frontend assets to the Phoenix static directory
RUN cp -r /app/frontend/dist/* ./priv/static/

# Install Elixir dependencies and compile the backend application
RUN mix phx.digest && \
    SECRET_KEY_BASE=buildtime_placeholder mix run --no-start priv/scripts/brotli_assets.exs && \
    mix release

# Stage 2: runtime image. Only the release and priv/ are carried over; Node.js, Elm,
# the frontend sources, node_modules and the Mix build tree stay in the build stage.
# Must use the same Debian release as the elixir image above (ERTS and NIFs link against its glibc).
FROM debian:bookworm-slim AS runtime

RUN apt-get update && \
    apt-get install -y --no-install-recommends libstdc++6 openssl libncurses6 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

ENV LANG=C.UTF-8

# Keep the same paths as in the build stage: several modules resolve
# :code.priv_dir/1 at compile time (-> /app/nostr_backend/_build/prod/lib/nostr_backend/priv),
# and the feed/RSS generators write to "priv/static" relative to the working directory.
WORKDIR /app/nostr_backend

COPY --from=build /app/nostr_backend/_build/prod/rel/nostr_backend ./_build/prod/rel/nostr_backend
COPY --from=build /app/nostr_backend/priv ./priv
RUN mkdir -p _build/prod/lib/nostr_backend && \
    ln -s ../../../../priv _build/prod/lib/nostr_backend/priv

# Expose the Phoenix port
EXPOSE 4000
ENV PHX_SERVER=true \
    PORT=4000

CMD ["_build/prod/rel/nostr_backend/bin/nostr_backend", "start"]

