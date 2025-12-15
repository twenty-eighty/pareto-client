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

# Install Elm and elm-land for asset compilation
# Elm official package currently does not support ARM64, but lydell/elm does (used used by elm-land library)
# For ARM architecture we setup elm and alm -land through node_module libraries, installed as dev dependenices in frontend
RUN if [ "$TARGETPLATFORM" != "linux/arm64" ] ; \
    then npm install -g elm elm-land ; \
    else echo 'export PATH=$PATH:/app/frontend/node_modules/elm/bin ;  alias elm-land="node /app/frontend/node_modules/elm-land/src/index.js"' >> ~/.bashrc ; \
    fi

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
ENV REPLACE_OS_VARS=true \
    PORT=4000

CMD ["mix", "phx.server"]

