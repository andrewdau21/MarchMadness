"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";
import Image from "next/image";

const links = [
  { href: "/entry", label: "Enter" },
  { href: "/leaderboard", label: "Leaderboard" },
  { href: "/standings", label: "Full Standings" },
];

export function Navigation() {
  const pathname = usePathname();

  return (
    <nav
      className="sticky top-0 z-50 border-b"
      style={{
        background: "rgba(15,17,23,0.95)",
        borderColor: "var(--border)",
        backdropFilter: "blur(10px)",
      }}
    >
      <div className="container mx-auto px-4 max-w-7xl">
        <div className="flex items-center justify-between h-14">
          {/* Logo + Brand */}
          <Link href="/" className="flex items-center gap-2 shrink-0">
            <div className="relative w-8 h-8 rounded-full overflow-hidden">
              <Image
                src="/salbot.jpeg"
                alt="March Capness"
                fill
                sizes="32px"
                className="object-cover"
              />
            </div>
            <span className="font-bold text-sm md:text-base" style={{ color: "var(--accent)" }}>
              March Capness <span className="text-gray-400 font-normal">2026</span>
            </span>
          </Link>

          {/* Nav links */}
          <div className="flex items-center gap-1">
            {links.map(({ href, label }) => {
              const isActive = pathname === href || pathname.startsWith(href + "/");
              return (
                <Link
                  key={href}
                  href={href}
                  className="px-3 py-1.5 rounded-md text-sm font-medium transition-colors"
                  style={{
                    color: isActive ? "var(--accent)" : "var(--text-muted)",
                    background: isActive ? "rgba(0,163,108,0.1)" : "transparent",
                  }}
                >
                  {label}
                </Link>
              );
            })}
          </div>
        </div>
      </div>
    </nav>
  );
}
