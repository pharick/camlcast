#!/usr/bin/env python3
"""Fail the build on a documentation reference that does not resolve.

`dune build @doc` cannot do this itself. It exits 0 whatever odoc warns about,
and dune's knob for the opposite -- (env (_ (odoc (warnings fatal)))) -- is not
usable here, because most of what odoc warns about in this tree is not this
tree's fault.

The classic @doc pipeline builds odoc's include path out of local libraries
only: dune drops every external one, the stdlib included. So an @raise tag
naming a stdlib exception cannot resolve, however it is written. There are 35 of
them here, all @raise Invalid_argument or @raise Fun.Finally_raised, and all of
them correct documentation -- they resolve cleanly under @doc-new, which does
put the stdlib on the path. Turning warnings fatal would fail the build on 35
warnings nobody can fix, and deleting the tags to silence it would trade real
documentation for a green tick.

What is worth catching is the other kind: a reference the author got wrong. A
typo in {!Room.doorwya}, or a cross-library {!World.make} that needed to be
written {!Camlcast_core.World.make}. Those warn in exactly the same breath as
the stdlib ones and are just as invisible, because CI only reads an exit code
and tools/pages-site.py only follows links that were emitted -- an unresolved
reference is rendered as inert text and has no link to follow.

So the split is made here, by name, against a list short enough to read:

    TOLERATED    the external roots dune cannot resolve, and nothing else

Anything else is an offence. That is fail-closed: a new kind of warning is a
failure until somebody looks at it, and letting a new external root through is a
one-line change made on purpose rather than a silence nobody chose. If the list
goes stale -- the last @raise Fun.Finally_raised deleted, say -- that is said
out loud rather than failed on, since a doc improvement should not break a
build.

The diagnostics are read back out of the built artifacts with `odoc errors`
rather than scraped from the build's stderr, because @doc is cached: a tree that
is already built re-emits nothing, and a check that passes because nothing ran
is not a check.

Usage: tools/odoc-refs.py [odocls directory]
"""

import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DEFAULT_ODOCLS = ROOT / "_build" / "default" / "_doc" / "_odocls"

# Roots that live outside this project's own libraries, which dune's classic
# @doc pipeline does not put on odoc's include path. Each is here because an
# @raise tag names it: Invalid_argument throughout core/ and lib/, and
# Fun.Finally_raised on the two functions that unwind through Fun.protect.
# Add to this only for another external root, and only after checking that the
# reference really does resolve under `dune build @doc-new`.
TOLERATED = {"Invalid_argument", "Fun"}

# odoc prints a diagnostic as a location line followed by its message.
LOCATION = re.compile(r'^File "')
UNRESOLVED = re.compile(r"Couldn't find \"([^\"]+)\"")

USAGE = "usage: tools/odoc-refs.py [odocls directory]"


def diagnostics(odocl: Path) -> list[tuple[str, str]]:
    """Every (location, message) odoc stored while compiling and linking a unit.

    On stderr, where odoc puts diagnostics whether it is reporting them as it
    finds them or reading them back afterwards. Its stdout is empty, so a script
    that read that would find nothing to complain about and say so cheerfully.
    """
    stored = subprocess.run(
        ["odoc", "errors", str(odocl)],
        capture_output=True,
        text=True,
        check=True,
    ).stderr

    found: list[tuple[str, str]] = []
    where: str | None = None
    message: list[str] = []

    for line in stored.splitlines():
        if LOCATION.match(line):
            if where is not None:
                found.append((where, " ".join(message)))
            where, message = line.rstrip(":"), []
        elif where is not None and line.strip():
            message.append(line.strip())

    if where is not None:
        found.append((where, " ".join(message)))
    return found


def main() -> None:
    argv = sys.argv[1:]
    if argv[:1] in (["-h"], ["--help"]):
        print(USAGE)
        return
    if len(argv) > 1:
        sys.exit(f"odoc-refs: too many arguments\n{USAGE}")

    odocls = Path(argv[0]) if argv else DEFAULT_ODOCLS
    units = sorted(odocls.rglob("*.odocl")) if odocls.is_dir() else []
    # Nothing to read is a failed check and not a passed one: the likeliest
    # reason to find no artifacts is that the doc build did not happen.
    if not units:
        sys.exit(f"odoc-refs: no .odocl files under {odocls} -- run `dune build @doc` first")

    seen: Counter[str] = Counter()
    offences: list[tuple[str, str]] = []

    for unit in units:
        for where, message in diagnostics(unit):
            unresolved = UNRESOLVED.search(message)
            if unresolved and unresolved.group(1) in TOLERATED:
                seen[unresolved.group(1)] += 1
            else:
                offences.append((where, message))

    for where, message in offences:
        print(f"odoc-refs: {where}: {message}", file=sys.stderr)

    tolerated = sum(seen.values())
    print(f"odoc-refs: {len(units)} units read, {tolerated} tolerated, {len(offences)} to answer for")

    stale = sorted(TOLERATED - seen.keys())
    if stale:
        print(f"odoc-refs: nothing references {', '.join(stale)} any more -- drop from TOLERATED")

    if offences:
        sys.exit(f"odoc-refs: {len(offences)} unresolved reference(s)")


if __name__ == "__main__":
    main()
