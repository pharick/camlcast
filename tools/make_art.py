#!/usr/bin/env python3
"""Write the binary pictures this repository keeps under version control.

Two sets of files, for two different reasons.

`assets/` is the art the `loading` demo reads, and is here so that a demo of
reading art from files has art in files to read. It is drawn rather than
generated in the engine on purpose: a picture that came out of a paint program
is exactly the case `Image.load` and `Texture.load` exist for, and one produced
by the engine's own generators would prove less than it looked like it did.

`test/fixtures/` is the input to `test_surface`, `test_image` and `test_texture`.
Those files matter for a subtler reason: **this encoder shares no code with the
decoder under test.** It is the Python standard library's zlib and a PNG header
written out by hand, so a channel swap or an off-by-one in `lib/surface.ml`
cannot be cancelled out by the same mistake here. Their pixel values are chosen
to be distinct in every channel for that reason, and the OCaml tests assert them
as literals.

Run from the repository root:

    ./tools/make_art.py

It rewrites every file below, byte for byte, from nothing but this source.
The one JPEG is converted afterwards, because there is no JPEG encoder in the
standard library:

    sips -s format jpeg test/fixtures/swatch.png --out test/fixtures/swatch.jpg

Files written:

    assets/tiles.png        128x128 opaque    a wall pattern, for Texture.load
    assets/grille.png        64x64  masked    a see-through pattern
    assets/poster.png        96x64  RGBA      a decal, wider than it is tall
    assets/figure.png        96x96  RGBA      a sprite, square as a sprite must be
    test/fixtures/swatch.png   4x3  RGBA      known values, no two alike
    test/fixtures/tile.png     8x8  opaque    primaries and a grey ramp
    test/fixtures/holes.png    8x8  masked    one transparent quadrant
    test/fixtures/notanimage.txt              not a picture at all
"""

import math
import os
import struct
import zlib

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def write_png(path, width, height, pixel):
    """Write a PNG. `pixel(x, y)` returns (r, g, b, a), each 0..255.

    Colour type 6 is 8-bit RGBA, and filter type 0 (none) on every row keeps
    this short: the point is to be obviously correct, not to be small.
    """
    rows = bytearray()
    for y in range(height):
        rows.append(0)
        for x in range(width):
            r, g, b, a = pixel(x, y)
            rows += bytes((r & 255, g & 255, b & 255, a & 255))

    def chunk(tag, data):
        body = tag + data
        return (
            struct.pack(">I", len(data))
            + body
            + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF)
        )

    header = struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0)
    blob = (
        b"\x89PNG\r\n\x1a\n"
        + chunk(b"IHDR", header)
        + chunk(b"IDAT", zlib.compress(bytes(rows), 9))
        + chunk(b"IEND", b"")
    )
    full = os.path.join(ROOT, path)
    os.makedirs(os.path.dirname(full), exist_ok=True)
    with open(full, "wb") as f:
        f.write(blob)
    print(f"{path:<30} {len(blob)} bytes")


# --------------------------------------------------------------------------
# assets/ — what the `loading` demo reads.
#
# A Texture keeps only luminance, so the two patterns are drawn grey; their
# colour arrives at draw time from the Material wearing them.
# --------------------------------------------------------------------------


def wobble(x, y, scale, seed):
    """A cheap smooth field, so the surfaces are not flat. Wraps at 128, which
    is the size of the pattern, so the tile has no seam where it repeats."""
    return math.sin((x + seed) * scale) * math.cos((y - seed) * scale * 1.3)


def tiles(x, y):
    # Four rows of staggered tiles with a recessed grout line between them.
    row = y // 32
    sx = (x + (16 if row % 2 else 0)) % 64
    sy = y % 32
    grout = sx < 3 or sy < 3
    edge = sx < 6 or sy < 6 or sx > 57 or sy > 25
    base = 96 if grout else (176 if edge else 200)
    grain = 10 * wobble(x, y, 0.19, 3) + 5 * wobble(x, y, 0.61, 11)
    v = max(0, min(255, int(base + grain)))
    return (v, v, v, 255)


def grille(x, y):
    # A lattice of bars with square holes: the alpha is the point, since a
    # material wearing this pattern is see-through and takes the renderer's
    # translucent path.
    bar = x % 16 < 5 or y % 16 < 5
    if not bar:
        return (0, 0, 0, 0)
    lit = 150 + 60 * wobble(x, y, 0.5, 7)
    v = max(0, min(255, int(lit)))
    return (v, v, v, 255)


def poster(x, y):
    # 96x64: a framed print. Transparent outside the frame, so the wall shows
    # through the corners and the decal is not a rectangle of paint.
    if x < 4 or y < 4 or x > 91 or y > 59:
        return (0, 0, 0, 0)
    if x < 8 or y < 8 or x > 87 or y > 55:
        return (58, 44, 32, 255)  # the frame
    dx, dy = x - 48, y - 32
    if dx * dx + dy * dy * 2.2 < 320:
        return (206, 92, 64, 255)  # a sun
    if y > 44:
        band = (y - 44) // 4
        g = 120 - band * 14
        return (46, max(40, g), 82, 255)  # water
    return (228, 214, 186, 255)  # sky


def figure(x, y):
    # 96x96: a standing figure, cut out against nothing so only the shape is
    # drawn. Square, and not because an Image has to be — the poster beside it
    # is not — but because Viewport.sprite_box gives every sprite a billboard as
    # wide as it is tall, so a sprite drawn in a tall picture would be stretched
    # across a square one. The transparent margin is where the width goes.
    cx = 48
    if y < 22:  # head
        dx, dy = x - cx, y - 13
        if dx * dx + dy * dy < 81:
            return (222, 186, 152, 255)
        return (0, 0, 0, 0)
    if y < 30:  # neck and shoulders
        return (74, 82, 96, 255) if abs(x - cx) < 11 else (0, 0, 0, 0)
    if y < 62:  # coat
        w = 13 - (y - 30) // 12
        return (74, 82, 96, 255) if abs(x - cx) < w else (0, 0, 0, 0)
    if y < 90:  # legs, with a gap between them
        d = abs(x - cx)
        if 2 < d < 10:
            return (44, 48, 58, 255)
        return (0, 0, 0, 0)
    d = abs(x - cx)  # boots
    return (28, 26, 30, 255) if 1 < d < 11 else (0, 0, 0, 0)


# --------------------------------------------------------------------------
# test/fixtures/ — known values, asserted as literals by the OCaml tests.
# --------------------------------------------------------------------------


def swatch(x, y):
    """4x3. Every channel is a different function of the position, so a decoder
    that swapped two of them, or read a row at the wrong offset, cannot agree
    with this by accident. No alpha is 0 or 255: a value in between is the one
    that proves the channel survived rather than being defaulted."""
    return (10 + 20 * x, 100 + 40 * y, 200 - 10 * x - 5 * y, 245 - 30 * (x + y))


def tile(x, y):
    """8x8, fully opaque. The first four texels are the three primaries and
    white, which pin the Rec. 601 luma weights Texture.load reduces colour by;
    the rest is a grey ramp, whose luminance is itself, so the ramp pins the
    row-major order without the weights getting in the way."""
    if y == 0 and x < 4:
        return [
            (0, 255, 0, 255),  # luminance 149
            (255, 0, 0, 255),  # luminance 76
            (0, 0, 255, 255),  # luminance 29
            (255, 255, 255, 255),  # luminance 255
        ][x]
    v = 4 * (x + 8 * y)
    return (v, v, v, 255)


def holes(x, y):
    """8x8 with one transparent quadrant, so Texture.load has something to
    report `opaque = false` about."""
    if x >= 4 and y >= 4:
        return (200, 200, 200, 0)
    return (120, 120, 120, 255)


if __name__ == "__main__":
    write_png("assets/tiles.png", 128, 128, tiles)
    write_png("assets/grille.png", 64, 64, grille)
    write_png("assets/poster.png", 96, 64, poster)
    write_png("assets/figure.png", 96, 96, figure)

    write_png("test/fixtures/swatch.png", 4, 3, swatch)
    write_png("test/fixtures/tile.png", 8, 8, tile)
    write_png("test/fixtures/holes.png", 8, 8, holes)

    path = "test/fixtures/notanimage.txt"
    body = "This is not a picture. Loading it must fail, and say so.\n"
    with open(os.path.join(ROOT, path), "w") as f:
        f.write(body)
    print(f"{path:<30} {len(body)} bytes")
