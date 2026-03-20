import type { Metadata } from "next";
import "./globals.css";
import { Providers } from "./providers";
import { Navigation } from "@/components/layout/Navigation";

export const metadata: Metadata = {
  title: "March Capness 2026",
  description: "Salary cap March Madness bracket game",
  icons: {
    icon: "/salbot.jpeg",
  },
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en" className="h-full">
      <body className="h-full min-h-screen" style={{ background: "var(--bg)" }}>
        <Providers>
          <div className="flex flex-col min-h-screen">
            <Navigation />
            <main className="flex-1 container mx-auto px-4 py-6 max-w-7xl">
              {children}
            </main>
            <footer className="border-t py-4 text-center text-xs" style={{ borderColor: "var(--border)", color: "var(--text-muted)" }}>
              March Capness 2026 &bull; $100 Salary Cap &bull; 63-Game Tournament
            </footer>
          </div>
        </Providers>
      </body>
    </html>
  );
}
