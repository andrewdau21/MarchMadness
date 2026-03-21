"use client";

import { useState } from "react";
import Link from "next/link";
import { usePathname } from "next/navigation";
import Image from "next/image";

const ENTRY_CLOSED = new Date() > new Date("2026-03-18T12:00:00-07:00");

const links = [
  ...(ENTRY_CLOSED ? [] : [{ href: "/entry", label: "Enter" }]),
  { href: "/leaderboard", label: "Leaderboard" },
  { href: "/standings", label: "Full Standings" },
];

export function Navigation() {
  const pathname = usePathname();
  const [menuOpen, setMenuOpen] = useState(false);

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
          <Link href="/" className="flex items-center gap-2 shrink-0" onClick={() => setMenuOpen(false)}>
            <div className="relative w-8 h-8 rounded-full overflow-hidden">
              <Image src="/salbot.jpeg" alt="March Capness" fill sizes="32px" className="object-cover" />
            </div>
            <span className="font-bold text-sm md:text-base" style={{ color: "var(--accent)" }}>
              March Capness <span className="text-gray-400 font-normal">2026</span>
            </span>
          </Link>

          {/* Desktop nav */}
          <div className="hidden md:flex items-center gap-1">
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

          {/* Mobile hamburger */}
          <button
            className="md:hidden flex flex-col justify-center items-center w-8 h-8 gap-1.5"
            onClick={() => setMenuOpen((o) => !o)}
            aria-label="Toggle menu"
          >
            <span
              className="block w-5 h-0.5 transition-all duration-200"
              style={{
                background: "var(--text-muted)",
                transform: menuOpen ? "translateY(8px) rotate(45deg)" : "none",
              }}
            />
            <span
              className="block w-5 h-0.5 transition-all duration-200"
              style={{
                background: "var(--text-muted)",
                opacity: menuOpen ? 0 : 1,
              }}
            />
            <span
              className="block w-5 h-0.5 transition-all duration-200"
              style={{
                background: "var(--text-muted)",
                transform: menuOpen ? "translateY(-8px) rotate(-45deg)" : "none",
              }}
            />
          </button>
        </div>
      </div>

      {/* Mobile dropdown */}
      {menuOpen && (
        <div
          className="md:hidden border-t px-4 py-3 flex flex-col gap-1"
          style={{ borderColor: "var(--border)", background: "rgba(15,17,23,0.98)" }}
        >
          {links.map(({ href, label }) => {
            const isActive = pathname === href || pathname.startsWith(href + "/");
            return (
              <Link
                key={href}
                href={href}
                onClick={() => setMenuOpen(false)}
                className="px-3 py-2.5 rounded-md text-sm font-medium transition-colors"
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
      )}
    </nav>
  );
}
