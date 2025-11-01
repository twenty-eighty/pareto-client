"use strict";
// Queue Server TypeScript Client Example
// =====================================
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.createCampaign = createCampaign;
exports.leaseJobs = leaseJobs;
exports.completeJob = completeJob;
const _1 = require("./");
// Configure the client
const config = new _1.Configuration({
    basePath: 'http://localhost:4000/v1',
    // For JWT auth, add headers here
    // headers: { 'Authorization': 'Bearer YOUR_JWT_TOKEN' }
});
const api = new _1.QueueServerApi(config);
// Example: Create a campaign (requires JWT auth)
function createCampaign() {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            const campaign = {
                id: 'my-campaign-' + Date.now(),
                ownerId: 'your-pubkey',
                authorPubkey: 'your-pubkey',
                title: 'My Newsletter Campaign'
            };
            const response = yield api.createCampaign(campaign);
            console.log('Campaign created:', response.data);
        }
        catch (error) {
            console.error('Error creating campaign:', error);
        }
    });
}
// Example: Lease jobs (requires consumer auth)
function leaseJobs() {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            const leaseRequest = {
                queue: 'newsletter',
                max: 10,
                visibilityTimeoutS: 600
            };
            const response = yield api.leaseJobs(leaseRequest);
            console.log('Leased jobs:', response.data);
        }
        catch (error) {
            console.error('Error leasing jobs:', error);
        }
    });
}
// Example: Complete a job
function completeJob(jobId, leaseToken) {
    return __awaiter(this, void 0, void 0, function* () {
        try {
            const response = yield api.completeJob(jobId, {
                leaseToken: leaseToken
            });
            console.log('Job completed:', response.data);
        }
        catch (error) {
            console.error('Error completing job:', error);
        }
    });
}
