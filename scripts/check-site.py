#!/usr/bin/env python3
"""Check the rendered site in _site/ before it is deployed.

Three things go wrong silently with a site like this one, and none of them is
caught by `quarto render` succeeding:

1. A link to a PDF, a deck or an image whose target was never copied into
   _site (a typo in a URL-encoded file name, a resource missing from the
   `resources:` list in _quarto.yml).
2. A deck missing from the render list, or one that rendered as a plain HTML
   page instead of a revealjs presentation.
3. A Git LFS pointer published in place of a PDF, which nobody notices until a
   student opens it.

Run from the repository root, after `quarto render`:

    python scripts/check-site.py --decks 12
"""
from __future__ import annotations

import argparse
import os
import re
import sys
import urllib.parse

SITE = "_site"
LINK = re.compile(r"""<(?:a|img|link|script|source|iframe)\b[^>]*?\b(?:href|src)\s*=\s*["']([^"']+)["']""", re.I)
EXTERNAL = re.compile(r"^(https?:|mailto:|tel:|data:|javascript:|//)", re.I)


def internal_targets(html: str) -> list[str]:
    out = []
    for target in LINK.findall(html):
        target = target.strip()
        if not target or target.startswith("#") or EXTERNAL.match(target):
            continue
        out.append(target)
    return out


def resolve(page_dir: str, target: str) -> str:
    path = urllib.parse.unquote(target.split("#", 1)[0].split("?", 1)[0])
    base = SITE if path.startswith("/") else page_dir
    return os.path.normpath(os.path.join(base, path.lstrip("/")))


def exists(path: str) -> bool:
    if os.path.isfile(path):
        return True
    # A link to a directory is fine if it has an index page.
    return os.path.isdir(path) and os.path.isfile(os.path.join(path, "index.html"))


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--decks", type=int, default=None,
                    help="expected number of revealjs decks under _site/materials")
    args = ap.parse_args()

    if not os.path.isdir(SITE):
        print(f"error: no {SITE}/ directory; run `quarto render` first", file=sys.stderr)
        return 1

    fail = 0
    pages = 0
    decks = []
    broken: list[tuple[str, str]] = []
    seen: set[tuple[str, str]] = set()

    for root, _dirs, files in os.walk(SITE):
        for fname in files:
            if not fname.endswith(".html"):
                continue
            path = os.path.join(root, fname)
            html = open(path, encoding="utf-8", errors="replace").read()
            pages += 1
            rel = os.path.relpath(path, SITE).replace(os.sep, "/")

            if rel.startswith("materials/") and "reveal.js" in html:
                decks.append(rel)

            for target in internal_targets(html):
                resolved = resolve(root, target)
                key = (rel, target)
                if key in seen:
                    continue
                seen.add(key)
                if not exists(resolved):
                    broken.append(key)

    print(f"pages checked: {pages}")
    print(f"revealjs decks: {len(decks)}")
    for d in sorted(decks):
        print(f"  {d}")

    if broken:
        fail = 1
        print(f"\n{len(broken)} broken internal link(s):")
        for page, target in broken:
            print(f"  {page}  ->  {target}")
    else:
        print("\nno broken internal links")

    if args.decks is not None and len(decks) != args.decks:
        fail = 1
        print(f"\nerror: expected {args.decks} revealjs decks, found {len(decks)}")

    pdfs = 0
    pointers = []
    for folder in ("ilias-2023-24", "ilias-2024-25"):
        d = os.path.join(SITE, folder)
        if not os.path.isdir(d):
            fail = 1
            print(f"\nerror: {d} was not published")
            continue
        for fname in sorted(os.listdir(d)):
            if not fname.lower().endswith(".pdf"):
                continue
            pdfs += 1
            with open(os.path.join(d, fname), "rb") as fh:
                head = fh.read(64)
            if not head.startswith(b"%PDF"):
                pointers.append(f"{folder}/{fname}")
    print(f"\nPDFs published: {pdfs}")
    if pointers:
        fail = 1
        print(f"error: {len(pointers)} PDF(s) are not PDF files (LFS pointers?):")
        for p in pointers:
            print(f"  {p}")

    for must in ("index.html", "slides.html", "data.html", "resources.html", "about.html",
                 "cohorts/ws-2023-24.html", "cohorts/ws-2024-25.html",
                 "weeks/week-00.html", "weeks/week-07.html",
                 "prerequisites.html"):
        if not os.path.isfile(os.path.join(SITE, must)):
            fail = 1
            print(f"error: missing {SITE}/{must}")

    print("\nFAILED" if fail else "\nOK")
    return fail


if __name__ == "__main__":
    raise SystemExit(main())
