#!/bin/bash
# Regenerates the pictures the reader tests use, and the pixels each must decode to. Needs ImageMagick, python3 and
# the libjpeg tools (cjpeg, djpeg). The reference pixels (*.rgba, 8-bit red, green, blue, opacity, row by row) come
# from ImageMagick for PNG and from djpeg (no smoothing, integer transform) for JPEG, never from this program.
set -e
cd "$(dirname "$0")"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT

magick -seed 3 -size 13x11 plasma:fractal -depth 8 "$WORK/base.png"
magick "$WORK/base.png" \( -size 13x11 gradient:black-white \) -compose CopyOpacity -composite \
  -define png:color-type=6 "$WORK/alpha.png"

png() { # name source options...
  local name=$1 source=$2
  shift 2
  magick "$WORK/$source" "$@" "png-$name.png"
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
magick "$WORK/src.ppm" -colorspace CMYK -quality 90 jpeg-cmyk.jpg

for file in jpeg-*.jpg; do
  case $file in
    jpeg-cmyk.jpg) magick "$file" -depth 8 "rgba:${file%.jpg}.rgba" ;;
    *) djpeg -nosmooth -dct int -ppm "$file" > "$WORK/ref.ppm"
       magick "$WORK/ref.ppm" -depth 8 -alpha on "rgba:${file%.jpg}.rgba" ;;
  esac
done
