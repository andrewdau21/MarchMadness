export function Header({
  title,
  subtitle,
  badge,
}: {
  title: string;
  subtitle?: string;
  badge?: string;
}) {
  return (
    <div className="mb-6">
      <div className="flex items-center gap-2 mb-1">
        <h1 className="text-2xl font-bold" style={{ color: "var(--text)" }}>
          {title}
        </h1>
        {badge && (
          <span
            className="text-xs font-medium px-2 py-0.5 rounded-full"
            style={{ background: "rgba(0,163,108,0.15)", color: "var(--accent)" }}
          >
            {badge}
          </span>
        )}
      </div>
      {subtitle && (
        <p className="text-sm" style={{ color: "var(--text-muted)" }}>
          {subtitle}
        </p>
      )}
    </div>
  );
}
