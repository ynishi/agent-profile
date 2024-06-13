CREATE TABLE functions (
  function_id VARCHAR(255) PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  content TEXT NOT NULL,
  input_data TEXT,
  output_data TEXT,
  attributes JSONB
);
