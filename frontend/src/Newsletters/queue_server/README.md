## @queue-server/client@1.0.0

This generator creates TypeScript/JavaScript client that utilizes [axios](https://github.com/axios/axios). The generated Node module can be used in the following environments:

Environment
* Node.js
* Webpack
* Browserify

Language level
* ES5 - you must have a Promises/A+ library installed
* ES6

Module system
* CommonJS
* ES6 module system

It can be used in both TypeScript and JavaScript. In TypeScript, the definition will be automatically resolved via `package.json`. ([Reference](https://www.typescriptlang.org/docs/handbook/declaration-files/consumption.html))

### Building

To build and compile the typescript sources to javascript use:
```
npm install
npm run build
```

### Publishing

First build the package then run `npm publish`

### Consuming

navigate to the folder of your consuming project and run one of the following commands.

_published:_

```
npm install @queue-server/client@1.0.0 --save
```

_unPublished (not recommended):_

```
npm install PATH_TO_GENERATED_PACKAGE --save
```

### Documentation for API Endpoints

All URIs are relative to */v1*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*DefaultApi* | [**bulkEnqueueJobs**](docs/DefaultApi.md#bulkenqueuejobs) | **POST** /campaigns/{id}/jobs/bulk | Bulk enqueue jobs (NDJSON)
*DefaultApi* | [**commitCampaign**](docs/DefaultApi.md#commitcampaign) | **PATCH** /campaigns/{id}/commit | Commit campaign uploads and enable processing
*DefaultApi* | [**createCampaign**](docs/DefaultApi.md#createcampaign) | **POST** /campaigns | Create campaign
*DefaultApi* | [**getCampaignStatus**](docs/DefaultApi.md#getcampaignstatus) | **GET** /campaigns/{id}/status | Get campaign status


### Documentation For Models

 - [BulkResult](docs/BulkResult.md)
 - [CampaignCreate](docs/CampaignCreate.md)
 - [CampaignCreateResponse](docs/CampaignCreateResponse.md)
 - [CampaignStatus](docs/CampaignStatus.md)
 - [CampaignStatusCounts](docs/CampaignStatusCounts.md)
 - [CommitRequest](docs/CommitRequest.md)
 - [CommitResponse](docs/CommitResponse.md)
 - [ErrorResponse](docs/ErrorResponse.md)
 - [ErrorResponseError](docs/ErrorResponseError.md)


<a id="documentation-for-authorization"></a>
## Documentation For Authorization


Authentication schemes defined for the API:
<a id="BearerAuth"></a>
### BearerAuth

- **Type**: Bearer authentication

<a id="ConsumerAuth"></a>
### ConsumerAuth

- **Type**: Bearer authentication

<a id="NostrAuth"></a>
### NostrAuth


