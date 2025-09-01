# Pareto Client

> Built with [Elm Land](https://elm.land)

## Local development

```bash
# Requires Node.js v18+ (https://nodejs.org)
./run.sh
```

## Deploying to production

Deployment is eventually done with Docker but you can produce the Frontend build like this: 
```bash
./build.sh
```

## Nostr Deployment

For a deployment according to the NIP proposal [nsite spec](https://github.com/hzrd149/nips/blob/nsite/nsite.md) we use the [nsyte tooling](https://nsyte.run/).

Install nsyte with in your system with
```
curl -fsSL https://nsyte.run/get/install.sh | bash
```

Build the Pareto app in the frontend directory for the standalone environment
```
ELM_ENV=standalone ./build.sh 
```

Finally upload the app to the Nostr network with
```
nsyte upload dist/
```

The upload can take some minutes.

The tool will use the configuration file .nsite/config.json and ask for the Nostr private key. The key isn't include in the configuration file for security reasons.

After the deployment the new version of the app is available on any nsite gateway, currently only https://nsite.lol is functional.

You can try the current deployment at [Pareto standalone](https://npub17kz9w0x2atk0xu0chf4jjs4zl5sl2htpymsz42efurdp0tuxm3jqmrjrgu.nsite.lol/).

For more infos about this topic see [awesome-nsite](https://github.com/nostrver-se/awesome-nsite?tab=readme-ov-file).