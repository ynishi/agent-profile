import { createClient } from "@refinedev/supabase";

const SUPABASE_URL = import.meta.env.VITE_SUPABASE_URL;
const SUPABASE_KEY = import.meta.env.VITE_SUPABASE_KEY;
const SUPABASE_SCHEMA = import.meta.env.VITE_SUPABASE_SCHEMA;

export const supabaseClient = createClient(SUPABASE_URL, SUPABASE_KEY, {
  db: {
    schema: SUPABASE_SCHEMA,
  },
  auth: {
    persistSession: true,
  },
});
