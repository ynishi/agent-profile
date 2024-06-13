CREATE TABLE flows (
  flow_id VARCHAR(255) PRIMARY KEY,
  name TEXT NOT NULL,
  step_ids TEXT[] NOT NULL,
  description TEXT,
  attributes JSONB,
  error_policy TEXT
);
