/**
 * Read-only PostgreSQL connection pool.
 *
 * IMPORTANT: This pool is configured for READ-ONLY access.
 * Never perform INSERT / UPDATE / DELETE / DDL through this pool.
 */

import { Pool } from "pg";

declare global {
  // allow global var to persist across hot-reloads in development
  // eslint-disable-next-line no-var
  var _pgPool: Pool | undefined;
}

function createPool(): Pool {
  const pool = new Pool({
    host: process.env.host,
    port: parseInt(process.env.port ?? "5432", 10),
    database: process.env.dbname,
    user: process.env.user,
    password: process.env.password,
    max: 10,
    idleTimeoutMillis: 30_000,
    connectionTimeoutMillis: 10_000,
    ssl: process.env.DB_SSL === "true" ? { rejectUnauthorized: false } : undefined,
  });

  pool.on("error", (err) => {
    console.error("Unexpected error on idle DB client", err);
  });

  return pool;
}

// In development, reuse the pool across hot reloads to avoid exhausting connections.
const pool: Pool =
  process.env.NODE_ENV === "development"
    ? (global._pgPool ?? (global._pgPool = createPool()))
    : createPool();

export default pool;

/**
 * Execute a read-only query. Typed helper that prevents accidental mutations by
 * convention – callers should only pass SELECT statements.
 */
export async function query<T = Record<string, unknown>>(
  sql: string,
  params?: unknown[]
): Promise<T[]> {
  const client = await pool.connect();
  try {
    const result = await client.query<T>(sql, params);
    return result.rows;
  } finally {
    client.release();
  }
}
