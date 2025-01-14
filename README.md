# Backend and Frontend of the Pareto Nostr client

This is a monorepo with both Elixir/Phoenix backend and Elm frontend.

The backend delivers for certain routes HTML metadata for nostr articles, communities, and profiles:
- /a/naddr1qvzqq...            - metadata for long-form article
- /c/ncomm1qvzqq...            - metadata for Nostr community
- /e/nevent1q...               - metadata for Nostr article
- /u/bella@zaps.lol            - metadata for Nostr profile
- /u/bella@zaps.lol/identifier - metadata for Nostr article
- /p/nprofile1qyt8w... 	       - metadata for Nostr profile

Additionally a static landing page is delivered for the home route.

There's an API available that redirects to the image of OpenGraph metadata:
/api/opengraph/image??url=...
This is being used for thumbnails of Odysee videos

This API call proxies OEMBED calls in order to avoid CORS issues (Twitter/X):
/api/oembed

Another API endpoint helps embed Rumble videos in articles:
/api/rumble/embed

## Backend configuration

These environment variables need to be set
- **PHX_HOST**: The domain where the server is accessed from external (e. g. pareto.space)
- **PORT**: The port where the server accepts connections (e. g. 4000)
- **SECRET_KEY_BASE**: The secret key generated by *mix phx.gen.secret*.
- **POSTHOG_API_KEY**: The API key of a PostHog server
- **POSTHOG_HOST**: Host name of a PostHog server (e. g. https://eu.i.posthog.com)

## Adding Nostr users

Additional users @pareto.space can be added at nostr\_backend/lib/nostr\_backend\_web/controllers/nostr\_controller.ex

## Updating the landing page

This shell script downloads and modifies the landing page as needed:

```
nostr\_backend/update\_landing\_page.sh
```

## Frontend debugging

To enable debugging, write the following command in the browser console:
`localStorage.setItem('debug', 'ndk*,pareto-client*');`

To disable debugging, type
`localStorage.removeItem('debug');`

Make sure you display all log levels in the console in order to see debug messages.

# Development setup

For development purposes you don't need Docker. The Docker setup is mainly intended for deployment.

1. Setup the latest versions of Elm, Elixir and Erlang (OTP). One of the best options is [asdf](https://github.com/asdf-vm/asdf)
2. When using [VSCodium](https://vscodium.com/), install Elm, Elm Land, and Elixir support
3. Open the frontend and backend separately (`frontend` and `nostr_backend` directories)

## Frontend
Simply run `./run.sh` in the frontend directory.
On http://localhost:1234/read you'll find the Pareto client.

You'll notice a small rect bottom/right of the screen that allows to open the Elm debugger.

## Backend
The backend can be started locally with `./run.sh` in the `nostr_backend` directory. This includes building of the frontend.
To run the backend without building the frontend execute `./dev.sh`.

