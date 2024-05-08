CREATE TABLE agents (
  agent_id VARCHAR(255) PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  version TEXT NOT NULL,
  profile_id VARCHAR(255),
  flow_id VARCHAR(255),
  function_ids VARCHAR(255) [],
  attributes JSONB NOT NULL
);
