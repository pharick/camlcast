"""Turn a capture into a PNG, optionally cropping and scaling it up.

capture/capture.exe writes PPM because nothing in this tree encodes a PNG and
acquiring an encoder to look at a picture would be a poor trade. This is the
other half: three ASCII numbers a pixel in, one PNG out.

The PNG writer is the one in tools/make_art.py, for the reason that one gives
for existing at all -- the standard library's zlib and a header, short enough
to be obviously correct.

    python3 capture/ppm.py plan.ppm plan.png
    python3 capture/ppm.py plan.ppm plan.png --crop 0,0,190,190 --zoom 3

--crop is x,y,w,h in the capture's own pixels. --zoom repeats each pixel a
whole number of times, which is what a 480-pixel buffer needs to be read on a
screen: nearest neighbour and no smoothing, so what is looked at is what was
drawn.
"""

import struct
import sys
import zlib


def read_ppm(path):
    with open(path, "rb") as handle:
        parts = handle.read().split()
    if parts[0] != b"P3":
        raise SystemExit(f"{path}: not an ASCII PPM")
    width, height = int(parts[1]), int(parts[2])
    values = [int(v) for v in parts[4:]]
    return width, height, values


def write_png(path, width, height, pixel):
    rows = bytearray()
    for y in range(height):
        rows.append(0)
        for x in range(width):
            r, g, b = pixel(x, y)
            rows += bytes((r & 255, g & 255, b & 255, 255))

    def chunk(tag, data):
        body = tag + data
        return (
            struct.pack(">I", len(data))
            + body
            + struct.pack(">I", zlib.crc32(body) & 0xFFFFFFFF)
        )

    with open(path, "wb") as handle:
        handle.write(
            b"\x89PNG\r\n\x1a\n"
            + chunk(b"IHDR", struct.pack(">IIBBBBB", width, height, 8, 6, 0, 0, 0))
            + chunk(b"IDAT", zlib.compress(bytes(rows), 9))
            + chunk(b"IEND", b"")
        )


def main(argv):
    if len(argv) < 3:
        raise SystemExit(__doc__)
    source, target = argv[1], argv[2]
    crop = None
    zoom = 1
    rest = argv[3:]
    while rest:
        flag = rest.pop(0)
        if flag == "--crop":
            crop = tuple(int(n) for n in rest.pop(0).split(","))
        elif flag == "--zoom":
            zoom = int(rest.pop(0))
        else:
            raise SystemExit(f"unknown option {flag}")

    width, height, values = read_ppm(source)
    cx, cy, cw, ch = crop if crop else (0, 0, width, height)
    cw = min(cw, width - cx)
    ch = min(ch, height - cy)

    def at(x, y):
        index = (((y + cy) * width) + (x + cx)) * 3
        return values[index], values[index + 1], values[index + 2]

    write_png(target, cw * zoom, ch * zoom, lambda x, y: at(x // zoom, y // zoom))
    print(f"{target}  {cw * zoom}x{ch * zoom}")


if __name__ == "__main__":
    main(sys.argv)
