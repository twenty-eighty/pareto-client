# Use official Elixir image
FROM elixir:1.17.3

# Install Hex and Rebar
RUN mix local.hex --force && \
    mix local.rebar --force

# Install Node.js, npm, and Elm for asset compilation
RUN curl -sL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    npm install -g elm elm-land

# Set environment
ENV MIX_ENV=prod

# Set the working directory for the frontend build
WORKDIR /app/frontend

# Copy the frontend code to the container and run the build script
COPY frontend /app/frontend
RUN chmod +x build.sh && ./build.sh

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
    mix release

# Expose the Phoenix port
EXPOSE 4000
ENV REPLACE_OS_VARS=true \
    PORT=4000

CMD ["mix", "phx.server"]

