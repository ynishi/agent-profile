CREATE TABLE steps (
  step_id VARCHAR(255) PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  content TEXT NOT NULL,
  condition TEXT,
  function_ids TEXT[] NOT NULL,
  attributes JSONB,
  error_step_id VARCHAR(255)
);
