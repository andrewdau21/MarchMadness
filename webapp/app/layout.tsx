import type { Metadata } from "next";
import "./globals.css";
import { Navigation } from "@/components/layout/Navigation";
import { Providers } from "./providers";

export const metadata: Metadata = {
  title: "March Capness 2026",
  description: "Annual March Madness salary cap bracket contest",
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="en">
      <body>
        <Providers>
          <Navigation />
          <main className="container mx-auto px-4 py-6 max-w-7xl">
            {children}
          </main>
        </Providers>
      </body>
    </html>
  );
}
