declare function createCampaign(): Promise<void>;
declare function leaseJobs(): Promise<void>;
declare function completeJob(jobId: string, leaseToken: string): Promise<void>;
export { createCampaign, leaseJobs, completeJob };
