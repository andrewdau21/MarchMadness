/**
 * Read-only PostgreSQL connection pool.
 *
 * IMPORTANT: This pool is configured for READ-ONLY access.
 * Never perform INSERT / UPDATE / DELETE / DDL through this pool.
 *
 * Pool is created lazily on first query() call so Next.js build-time static
 * analysis never attempts a real DB connection.
 */

import { Pool } from "pg";

let pool: Pool | null = null;

function getPool(): Pool {
  if (!pool) {
    pool = new Pool({
      host: process.env.host,
      port: parseInt(process.env.port ?? "5432", 10),
      database: process.env.dbname,
      user: process.env.user,
      password: process.env.password,
      max: 10,
      idleTimeoutMillis: 30_000,
      connectionTimeoutMillis: 10_000,
      ssl: { rejectUnauthorized: false },
    });
    pool.on("error", (err) => {
      console.error("Unexpected error on idle DB client", err);
      pool = null; // reset so next call creates a fresh pool
    });
  }
  return pool;
}

/**
 * Execute a read-only query. Typed helper that prevents accidental mutations by
 * convention – callers should only pass SELECT statements.
 */
export async function query<T = Record<string, unknown>>(
  sql: string,
  params?: unknown[]
): Promise<T[]> {
  const client = await getPool().connect();
  try {
    const result = await client.query<T>(sql, params);
    return result.rows;
  } finally {
    client.release();
  }
}
