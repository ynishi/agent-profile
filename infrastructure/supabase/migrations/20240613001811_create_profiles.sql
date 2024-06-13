CREATE TABLE profiles (
  profile_id VARCHAR(255) PRIMARY KEY,
  name TEXT NOT NULL,
  description TEXT,
  personalities JSONB,
  expertise JSONB,
  behavior JSONB,
  mission JSONB,
  policies JSONB
);
