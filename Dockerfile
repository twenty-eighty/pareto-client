# Use official Elixir image
FROM elixir:1.17.3

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

# Expose the Phoenix port
EXPOSE 4000
ENV PHX_SERVER=true \
    PORT=4000

CMD ["_build/prod/rel/nostr_backend/bin/nostr_backend", "start"]

