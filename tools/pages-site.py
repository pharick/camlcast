#!/usr/bin/env python3
"""Flatten dune's odoc output into the tree GitHub Pages should serve.

The docs then live at pharick.github.io/camlcast/ rather than one level down at
pharick.github.io/camlcast/camlcast/.

The doubling is structural, not a mistake: a project site is rooted at the
repository name and odoc namespaces its output by package name, and here both
are "camlcast". Neither dune nor odoc can be told to drop the package directory
-- @doc-new does the opposite and buries it under docs/local/ beside the stdlib
-- so the tree gets rearranged after the fact.

What that takes is small and fully enumerable. odoc writes relative links, so
everything inside camlcast/ still resolves once the whole directory moves up: a
sibling reference like ../Vec/index.html is as true at the root as it was one
level down. Exactly two kinds of link point *out* of the package directory, and
both land on _html itself:

    (../)^depth odoc.support/...    the stylesheet and the highlighter
    (../)^depth index.html          the "Up"/"Index" nav, to the package list

Both sit at full depth by construction -- that is what makes them findable, and
what keeps a shallower intra-package link at the same depth (../index.html from
Camlcast/, meaning the package index) from matching. Rewriting the full-depth
ones to a level shallower is the whole job. The package list itself is dropped:
with one package in it, it said nothing the new root page does not.

Verified rather than assumed. check_links resolves every local href and src in
the finished tree and fails on the first that does not exist or that climbs out
of the site root, so an odoc release that renames a directory or reshapes the
nav breaks the build here instead of quietly shipping a site with no stylesheet.

Usage: tools/pages-site.py [html directory] [output directory]
"""

import re
import shutil
import sys
from pathlib import Path

PACKAGE = "camlcast"
SUPPORT = "odoc.support"

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_HTML = ROOT / "_build" / "default" / "_doc" / "_html"
DEFAULT_OUT = ROOT / "_site"

# href="..." or src="...", the only two ways an odoc page names another file.
LINK = re.compile(r'(?:href|src)="([^"]+)"')
NAV = re.compile(r'<nav class="odoc-nav">.*?</nav>', re.DOTALL)


def build(html: Path, out: Path) -> None:
    """Move the package directory up to the root, with its support files."""
    if not (html / PACKAGE).is_dir():
        sys.exit(f"pages-site: no odoc output at {html}; run 'dune build @doc' first")

    if out.exists():
        shutil.rmtree(out)
    # dune leaves its output read-only and the rewrite below writes in place.
    shutil.copytree(html / PACKAGE, out)
    shutil.copytree(html / SUPPORT, out / SUPPORT)
    for path in out.rglob("*"):
        path.chmod(path.stat().st_mode | 0o200)

    for page in sorted(out.rglob("*.html")):
        # The prefix that escaped the package directory was one ../ longer than
        # the page's own depth, so it loses one.
        depth = len(page.relative_to(out).parts) - 1
        escaped, inside = "../" * (depth + 1), "../" * depth
        text = page.read_text(encoding="utf-8")
        text = text.replace(f'"{escaped}{SUPPORT}/', f'"{inside}{SUPPORT}/')
        text = text.replace(f'"{escaped}index.html"', f'"{inside}index.html"')
        page.write_text(text, encoding="utf-8")

    # The root page kept the nav it had as a subdirectory, which now offers "Up"
    # and "Index" links to itself. There is nothing above the root to go up to.
    index = out / "index.html"
    index.write_text(NAV.sub("", index.read_text(encoding="utf-8")), encoding="utf-8")


def check_links(out: Path) -> int:
    """Resolve every local link. Returns how many were broken."""
    root = out.resolve()
    broken = 0

    for page in sorted(out.rglob("*.html")):
        for target in sorted(set(LINK.findall(page.read_text(encoding="utf-8")))):
            # A bare #anchor is same-page; a scheme or // is somewhere else.
            target = target.split("#", 1)[0]
            if not target or "://" in target or target.startswith(("//", "mailto:")):
                continue

            path = (page.parent / target).resolve()
            where = page.relative_to(out)
            if not path.exists():
                print(f"pages-site: {where} -> {target} is missing", file=sys.stderr)
                broken += 1
            elif root not in path.parents and path != root:
                print(f"pages-site: {where} -> {target} escapes the root", file=sys.stderr)
                broken += 1

    return broken


def main() -> None:
    argv = sys.argv[1:]
    html = Path(argv[0]) if len(argv) > 0 else DEFAULT_HTML
    out = Path(argv[1]) if len(argv) > 1 else DEFAULT_OUT

    build(html, out)

    broken = check_links(out)
    if broken:
        sys.exit(f"pages-site: {broken} broken link(s)")

    pages = sum(1 for _ in out.rglob("*.html"))
    print(f"pages-site: {out}")
    print(f"pages-site: {pages} pages, links checked")


if __name__ == "__main__":
    main()
