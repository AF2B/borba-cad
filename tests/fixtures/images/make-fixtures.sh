#!/bin/bash
# Regenerates the pictures the reader tests use, and the pixels each must decode to. Needs ImageMagick, python3 and
# the libjpeg tools (cjpeg, djpeg). The reference pixels (*.rgba, 8-bit red, green, blue, opacity, row by row) come
# from ImageMagick for PNG and from djpeg (no smoothing, integer transform) for JPEG, never from this program.
set -e
export SOURCE_DATE_EPOCH=1700000000
cd "$(dirname "$0")"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

magick -seed 3 -size 13x11 plasma:fractal -depth 8 "$WORK/base.png"
magick "$WORK/base.png" \( -size 13x11 gradient:black-white \) -compose CopyOpacity -composite \
  -define png:color-type=6 "$WORK/alpha.png"

png() { # name source options...
  local name=$1 source=$2
  shift 2
  magick "$WORK/$source" "$@" -strip "png-$name.png"
}
png rgb8 base.png -define png:color-type=2 -define png:bit-depth=8
png rgb16 base.png -define png:color-type=2 -define png:bit-depth=16
png rgba8 alpha.png -define png:color-type=6 -define png:bit-depth=8
png rgba16 alpha.png -define png:color-type=6 -define png:bit-depth=16
png gray1 base.png -colorspace Gray -threshold 50% -define png:color-type=0 -define png:bit-depth=1
png gray2 base.png -colorspace Gray -posterize 4 -define png:color-type=0 -define png:bit-depth=2
png gray4 base.png -colorspace Gray -posterize 16 -define png:color-type=0 -define png:bit-depth=4
png gray8 base.png -colorspace Gray -define png:color-type=0 -define png:bit-depth=8
png gray16 base.png -colorspace Gray -define png:color-type=0 -define png:bit-depth=16
png graya8 alpha.png -colorspace Gray -define png:color-type=4 -define png:bit-depth=8
png graya16 alpha.png -colorspace Gray -define png:color-type=4 -define png:bit-depth=16
png pal1 base.png -colors 2 -define png:color-type=3 -define png:bit-depth=1
png pal2 base.png -colors 4 -define png:color-type=3 -define png:bit-depth=2
png pal4 base.png -colors 16 -define png:color-type=3 -define png:bit-depth=4
png pal8 base.png -colors 200 -define png:color-type=3 -define png:bit-depth=8
png rgb8-adam7 base.png -interlace PNG -define png:color-type=2 -define png:bit-depth=8
png rgba8-adam7 alpha.png -interlace PNG -define png:color-type=6 -define png:bit-depth=8
png gray1-adam7 base.png -colorspace Gray -threshold 50% -interlace PNG -define png:color-type=0 -define png:bit-depth=1
png pal4-adam7 base.png -colors 16 -interlace PNG -define png:color-type=3 -define png:bit-depth=4
png rgb16-adam7 base.png -interlace PNG -define png:color-type=2 -define png:bit-depth=16

# The transparency chunk, which ImageMagick does not write for these types, added by hand.
python3 - <<'PY'
import struct, zlib

def chunks(data):
    position = 8
    while position < len(data):
        size = struct.unpack(">I", data[position:position + 4])[0]
        yield data[position + 4:position + 8], data[position + 8:position + 8 + size]
        position += 12 + size

def chunk(kind, body):
    return struct.pack(">I", len(body)) + kind + body + struct.pack(">I", zlib.crc32(kind + body) & 0xffffffff)

def with_transparency(source, target, transparency):
    data = open(source, "rb").read()
    out = data[:8]
    for kind, body in chunks(data):
        out += chunk(kind, body)
        if kind == b"PLTE" or (kind == b"IHDR" and body[9] in (0, 2)):
            out += chunk(b"tRNS", transparency)
    open(target, "wb").write(out)

with_transparency("png-pal8.png", "png-pal8-trns.png", bytes([0, 128, 255, 64]))
with_transparency("png-pal4.png", "png-pal4-trns.png", bytes([0, 0, 50]))
with_transparency("png-gray8.png", "png-gray8-trns.png", struct.pack(">H", 100))
with_transparency("png-gray1.png", "png-gray1-trns.png", struct.pack(">H", 1))
with_transparency("png-rgb8.png", "png-rgb8-trns.png", struct.pack(">HHH", 10, 20, 30))
with_transparency("png-gray16.png", "png-gray16-trns.png", struct.pack(">H", 0x8000))
PY

for file in png-*.png; do
  magick "$file" -depth 8 -alpha on "rgba:${file%.png}.rgba"
done

magick -seed 5 -size 13x11 plasma:fractal -depth 8 "$WORK/src.ppm"
jpeg() { # name cjpeg-options...
  local name=$1
  shift
  cjpeg "$@" "$WORK/src.ppm" > "jpeg-$name.jpg"
}
jpeg base444 -quality 90 -sample 1x1
jpeg s420 -quality 85 -sample 2x2
jpeg s422 -quality 85 -sample 2x1
jpeg s440 -quality 85 -sample 1x2
jpeg s411 -quality 85 -sample 4x1
jpeg sodd -quality 85 -sample 2x2,1x1,1x2
jpeg prog444 -quality 90 -progressive
jpeg prog420 -quality 80 -progressive -sample 2x2
jpeg rst420 -quality 80 -restart 2 -sample 2x2
jpeg rstb422 -quality 80 -restart 2B -sample 2x1
jpeg progrst -quality 80 -progressive -restart 2
jpeg gray -quality 90 -grayscale
jpeg grayprog -quality 90 -grayscale -progressive
jpeg q100 -quality 100 -sample 1x1
jpeg q5 -quality 5 -sample 2x2
jpeg rgb -quality 90 -rgb -sample 1x1
magick "$WORK/src.ppm" -colorspace CMYK -quality 90 -strip jpeg-cmyk.jpg

for file in jpeg-*.jpg; do
  case $file in
    jpeg-cmyk.jpg) magick "$file" -depth 8 "rgba:${file%.jpg}.rgba" ;;
    *) djpeg -nosmooth -dct int -ppm "$file" > "$WORK/ref.ppm"
       magick "$WORK/ref.ppm" -depth 8 -alpha on "rgba:${file%.jpg}.rgba" ;;
  esac
done

# PDF files. The vector page is drawn by cairo from an SVG; the pages with a scan are made by ImageMagick; the
# page whose objects sit inside an object stream, with a cross-reference stream, is written by hand below.
# Reference pixels: poppler's pdftoppm for the vector pages, the source picture for the scans.
cat > "$WORK/vector.svg" <<'SVG'
<svg xmlns="http://www.w3.org/2000/svg" width="120px" height="80px" viewBox="0 0 120 80">
  <rect width="120" height="80" fill="white"/>
  <rect x="10" y="10" width="50" height="30" fill="none" stroke="black" stroke-width="2"/>
  <line x1="10" y1="25" x2="60" y2="25" stroke="red" stroke-width="1" stroke-dasharray="5 3"/>
  <circle cx="90" cy="25" r="15" fill="#8888ff" stroke="blue" stroke-width="2"/>
  <path d="M 10 70 C 30 45, 50 80, 70 60 S 100 50, 110 70" fill="none" stroke="darkgreen" stroke-width="3"
        stroke-linecap="round"/>
  <path d="M 75 45 L 105 45 L 105 55 L 75 55 Z M 80 48 L 100 48 L 100 52 L 80 52 Z" fill="#cc4444"
        fill-rule="evenodd"/>
</svg>
SVG
rsvg-convert -f pdf --dpi-x 72 --dpi-y 72 "$WORK/vector.svg" -o pdf-vector.pdf
pdftoppm -r 72 pdf-vector.pdf "$WORK/vector-ref"
magick "$WORK"/vector-ref*.ppm -depth 8 -alpha on rgba:pdf-vector.rgba

cat > "$WORK/text.svg" <<'SVG'
<svg xmlns="http://www.w3.org/2000/svg" width="160px" height="60px" viewBox="0 0 160 60">
  <rect width="160" height="60" fill="white"/>
  <text x="8" y="26" font-family="Noto Sans" font-size="20" fill="black">Room 12,5 m</text>
  <text x="8" y="50" font-family="Noto Sans Mono" font-size="12" fill="#cc0000">3.50 x 4.20</text>
</svg>
SVG
rsvg-convert -f pdf --dpi-x 72 --dpi-y 72 "$WORK/text.svg" -o pdf-text.pdf
pdftoppm -r 72 pdf-text.pdf "$WORK/text-ref"
magick "$WORK"/text-ref*.ppm -depth 8 -alpha on rgba:pdf-text.rgba

magick -seed 4 -size 40x30 plasma:fractal -depth 8 "$WORK/scan.png"
magick "$WORK/scan.png" -density 72 -compress zip pdf-scan-flate.pdf
magick "$WORK/scan.png" -depth 8 -alpha on rgba:pdf-scan-flate.rgba
magick "$WORK/scan.png" -colorspace Gray -threshold 50% -define png:color-type=0 -define png:bit-depth=1 "$WORK/bw.png"
magick "$WORK/bw.png" -density 72 -compress zip pdf-scan-1bit.pdf
magick "$WORK/bw.png" -depth 8 -alpha on rgba:pdf-scan-1bit.rgba
magick "$WORK/scan.png" -colors 16 -define png:color-type=3 "$WORK/pal.png"
magick "$WORK/pal.png" -density 72 -compress zip pdf-scan-indexed.pdf
magick "$WORK/pal.png" -depth 8 -alpha on rgba:pdf-scan-indexed.rgba
magick "$WORK/scan.png" -quality 88 "$WORK/scan.jpg"
magick "$WORK/scan.jpg" -density 72 -compress jpeg pdf-scan-jpeg.pdf
pdfimages -j pdf-scan-jpeg.pdf "$WORK/extracted"
djpeg -nosmooth -dct int -ppm "$WORK"/extracted-000.jpg > "$WORK/extracted.ppm"
magick "$WORK/extracted.ppm" -depth 8 -alpha on rgba:pdf-scan-jpeg.rgba
magick "$WORK/scan.png" -density 144 -compress zip pdf-scan-dense.pdf

python3 - <<'PY'
import zlib

def stream(dictionary, data, compress=True):
    body = zlib.compress(data) if compress else data
    flate = b"/Filter /FlateDecode " if compress else b""
    return (b"<< " + dictionary + b" " + flate + b"/Length " + str(len(body)).encode() + b" >>\nstream\n"
            + body + b"\nendstream")

content = b"0.2 0.4 0.9 rg 10 10 60 40 re f 1 0 0 RG 3 w 5 5 m 95 55 l S"
objects = {
    1: b"<< /Type /Catalog /Pages 2 0 R >>",
    2: b"<< /Type /Pages /Kids [3 0 R] /Count 1 >>",
    3: b"<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 60] /Contents 4 0 R /Resources << >> >>",
}
inner = b""
offsets = []
for number in (1, 2, 3):
    offsets.append((number, len(inner)))
    inner += objects[number] + b"\n"
header = b" ".join(b"%d %d" % pair for pair in offsets) + b"\n"
objstm = stream(b"/Type /ObjStm /N 3 /First %d" % len(header), header + inner)
out = b"%PDF-1.5\n"
positions = {}
positions[4] = len(out)
out += b"4 0 obj\n" + stream(b"", content) + b"\nendobj\n"
positions[5] = len(out)
out += b"5 0 obj\n" + objstm + b"\nendobj\n"
xref_position = len(out)
# Cross-reference stream: type, field 2, field 3 for objects 0..6.
rows = bytearray()
def row(kind, a, b):
    rows.extend(bytes([kind, (a >> 8) & 255, a & 255, b]))
row(0, 0, 255)
row(2, 5, 0)
row(2, 5, 1)
row(2, 5, 2)
row(1, positions[4], 0)
row(1, positions[5], 0)
row(1, xref_position, 0)
out += (b"6 0 obj\n" + stream(b"/Type /XRef /Size 7 /W [1 2 1] /Root 1 0 R", bytes(rows)) + b"\nendobj\n")
out += b"startxref\n%d\n%%%%EOF\n" % xref_position
open("pdf-objstm.pdf", "wb").write(out)
PY
pdftoppm -r 72 pdf-objstm.pdf "$WORK/objstm-ref"
magick "$WORK"/objstm-ref*.ppm -depth 8 -alpha on rgba:pdf-objstm.rgba
