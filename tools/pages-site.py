#!/usr/bin/env python3
"""Flatten dune's odoc output into the tree GitHub Pages should serve.

The docs then live at pharick.github.io/camlcast/ rather than one level down at
pharick.github.io/camlcast/camlcast/.

The doubling is structural, not a mistake: a project site is rooted at the
repository name and odoc namespaces its output by package name, and here the
repository and the engine's package are both "camlcast". Neither dune nor odoc
can be told to drop the package directory -- @doc-new does the opposite and
buries it under docs/local/ beside the stdlib -- so the tree gets rearranged
after the fact.

There are two packages. camlcast is the one the site is named for and the one
that moves up; camlcast-demo keeps its directory and its depth, one level under
the root, exactly where odoc put it. Each needs a rewrite, and they are not the
same rewrite, because only one of them moved.

odoc writes relative links, so everything inside camlcast/ still resolves once
the whole directory moves up: a sibling reference like ../Vec/index.html is as
true at the root as it was one level down. Exactly three kinds of link point
*out* of that directory, and all three land on _html itself:

    (../)^(depth+1) odoc.support/...    the stylesheet and the highlighter
    (../)^(depth+1) index.html          the "Up"/"Index" nav, to the package list
    (../)^(depth+1) camlcast-demo/...   doc/index.mld's pointer at the demos

All three sit at full depth by construction -- that is what makes them findable,
and what keeps a shallower intra-package link at the same depth (../index.html
from Camlcast/, meaning the package index) from matching. Rewriting them a level
shallower is the whole job on that side.

The demo pages did not move, so their own links -- stylesheet, nav, siblings --
are as true in _site as they were in _html and are left alone. What did move is
what they point *at*: a demo module documents the engine types it uses, and odoc
resolves those across the package boundary as

    (../)^depth camlcast/Camlcast/Room/index.html

which was a sibling package directory under _html and is the site root here. So
that one prefix loses its package directory rather than a level. The trailing
slash is what keeps camlcast/ from also matching camlcast-demo/.

The package list at the old root is dropped. It listed the two packages, and the
new root page is one of them and points at the other.

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
DEMO = "camlcast-demo"
SUPPORT = "odoc.support"
IMAGES = "images"

# Dropped into the output directory, and looked for again before the next run
# deletes it. The site is built by wiping and recopying -- copytree wants a
# destination that does not exist -- so the output path is the one argument that
# is destroyed rather than read, and a mistyped one is not recoverable.
SENTINEL = ".pages-site"

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_HTML = ROOT / "_build" / "default" / "_doc" / "_html"
DEFAULT_OUT = ROOT / "_site"

# odoc's stylesheet gives images no width of their own -- its reset names img
# for margin and border only -- and the page is a grid whose middle track takes
# its minimum from its content. A 1024-pixel screenshot therefore widens that
# track past the body's own max-width and puts a horizontal scrollbar under the
# whole page, tables of contents and all. One rule fixes it, and it is appended
# rather than patched in so that an odoc upgrade rewriting the file above it
# changes nothing here.
IMAGE_CSS = """
/* Added by tools/pages-site.py: see the comment beside IMAGE_CSS there. */
.odoc-content img { max-width: 100%; height: auto; }
"""

# href="..." or src="...", the only two ways an odoc page names another file.
LINK = re.compile(r'(?:href|src)="([^"]+)"')
NAV = re.compile(r'<nav class="odoc-nav">.*?</nav>', re.DOTALL)


def target(out: Path) -> Path:
    """The directory build() will wipe. Refuses one it did not make.

    The bundle scripts beside this one never delete the output directory they
    are given -- they delete a subpath they built the name of themselves -- and
    that is the safe shape. copytree rules it out here, so the check has to be
    the other way round: the site marks itself with SENTINEL when it is made,
    and nothing without that mark is deleted.
    """
    out = out.resolve()

    if out.parent == out:
        sys.exit(f"pages-site: {out} is a filesystem root; refusing to delete it")
    if out == ROOT or out in ROOT.parents:
        sys.exit(f"pages-site: {out} holds this checkout; refusing to delete it")

    if out.exists():
        if not out.is_dir():
            sys.exit(f"pages-site: {out} is not a directory")
        if not (out / SENTINEL).is_file():
            sys.exit(
                f"pages-site: {out} was not built by pages-site -- no {SENTINEL} "
                "in it; delete it yourself if that is really what you meant"
            )

    return out


def build(html: Path, out: Path) -> None:
    """Move the package directory up to the root, with its support files."""
    for package in (PACKAGE, DEMO):
        if not (html / package).is_dir():
            sys.exit(
                f"pages-site: no odoc output for {package} at {html}; "
                "run 'dune build @doc' first"
            )

    images = ROOT / "doc" / IMAGES
    if not images.is_dir():
        # Committed, not built: nothing in this repository draws them, and a
        # checkout that is missing them is a checkout with something wrong.
        sys.exit(f"pages-site: no screenshots at {images}")

    if out.exists():
        shutil.rmtree(out)
    # dune leaves its output read-only and the rewrite below writes in place.
    shutil.copytree(html / PACKAGE, out)
    # The other package keeps its directory and its depth, so it is carried
    # across as it stands -- see the note about that in the module docstring.
    shutil.copytree(html / DEMO, out / DEMO)
    shutil.copytree(html / SUPPORT, out / SUPPORT)
    # The pictures the two guides are illustrated with. dune's @doc does not
    # carry them and says so -- "Dune does not yet support building
    # documentation for assets" -- so the .mld pages name them by plain relative
    # URL and this is what puts them where that URL points. Both guides land at
    # the site root, so images/ is their sibling and "images/x.png" resolves.
    # check_links below then proves it, the same as for every other link.
    shutil.copytree(images, out / IMAGES)
    # What target() above looks for. Written here rather than at the end so that
    # a run interrupted midway still leaves a directory the next one may wipe.
    (out / SENTINEL).write_text("", encoding="utf-8")
    for path in out.rglob("*"):
        path.chmod(path.stat().st_mode | 0o200)

    with (out / SUPPORT / "odoc.css").open("a", encoding="utf-8") as css:
        css.write(IMAGE_CSS)

    for page in sorted(out.rglob("*.html")):
        depth = len(page.relative_to(out).parts) - 1
        escaped, inside = "../" * (depth + 1), "../" * depth
        text = page.read_text(encoding="utf-8")
        if page.is_relative_to(out / DEMO):
            # This page stayed put; what it points at moved up. A reference
            # into the engine's package directory is now a reference to the
            # root, so it drops the directory and keeps its depth.
            text = text.replace(f'"{inside}{PACKAGE}/', f'"{inside}')
        else:
            # The prefix that escaped the package directory was one ../ longer
            # than the page's own depth, so it loses one.
            text = text.replace(f'"{escaped}{SUPPORT}/', f'"{inside}{SUPPORT}/')
            text = text.replace(f'"{escaped}index.html"', f'"{inside}index.html"')
            text = text.replace(f'"{escaped}{DEMO}/', f'"{inside}{DEMO}/')
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


USAGE = "usage: tools/pages-site.py [html directory] [output directory]"


def main() -> None:
    argv = sys.argv[1:]
    if argv[:1] in (["-h"], ["--help"]):
        print(USAGE)
        return
    # The second argument is deleted rather than read, so an extra one is a
    # misunderstanding worth stopping for and not a word to ignore.
    if len(argv) > 2:
        sys.exit(f"pages-site: too many arguments\n{USAGE}")

    html = Path(argv[0]) if len(argv) > 0 else DEFAULT_HTML
    out = target(Path(argv[1]) if len(argv) > 1 else DEFAULT_OUT)

    build(html, out)

    broken = check_links(out)
    if broken:
        sys.exit(f"pages-site: {broken} broken link(s)")

    pages = sum(1 for _ in out.rglob("*.html"))
    print(f"pages-site: {out}")
    print(f"pages-site: {pages} pages, links checked")


if __name__ == "__main__":
    main()
