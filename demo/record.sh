#!/usr/bin/env bash
# Record every demo scene, then encode them for the README (MP4 + GIF in media/)
# and one combined clip for the portfolio (VP9 WebM + poster in media/portfolio/).
# Needs racket (with web-server-lib), node and ffmpeg (libx264, libvpx-vp9).
set -euo pipefail
cd "$(dirname "$0")/.."

[ -d demo/node_modules ] || (cd demo && npm install --silent)
node demo/record.mjs

mkdir -p media/portfolio
SCENES=(overview node route-short route-long distance cycle-3 cycle-5 errors)
for s in "${SCENES[@]}"; do
  # Constant 30 fps master from the timestamped PNG frames.
  ffmpeg -hide_banner -loglevel error -y -f concat -i "demo/out/$s/frames.txt" \
    -vf "fps=30,format=yuv420p" -c:v libx264 -crf 16 -preset fast "demo/out/$s.mp4"
  ffmpeg -hide_banner -loglevel error -y -i "demo/out/$s.mp4" -c:v libx264 -crf 28 -preset slow \
    -vf "scale=1280:-2" -movflags +faststart "media/$s.mp4"
  ffmpeg -hide_banner -loglevel error -y -i "demo/out/$s.mp4" \
    -vf "fps=8,scale=720:-1:flags=lanczos,split[a][b];[a]palettegen=max_colors=48:stats_mode=diff[p];[b][p]paletteuse=dither=none:diff_mode=rectangle" \
    "media/$s.gif"
  echo "encoded $s"
done

# Portfolio clip, 1024x640 (16:10) like the other project videos: the overview map, one full
# route search with the typing, then the result map of every other scene.
list=demo/out/portfolio.txt
: > "$list"
part() { # scene, seconds from the end (0 = whole scene)
  local f="demo/out/part-$1.mp4"
  if [ "$2" = 0 ]; then cp "demo/out/$1.mp4" "$f"; else ffmpeg -hide_banner -loglevel error -y -sseof "-$2" -i "demo/out/$1.mp4" -c:v libx264 -crf 16 -preset fast "$f"; fi
  echo "file 'part-$1.mp4'" >> "$list"
}
part overview 3.5
part route-short 0
part route-long 3.5
part distance 3.5
part cycle-3 4
part cycle-5 4
ffmpeg -hide_banner -loglevel error -y -f concat -safe 0 -i "$list" \
  -vf "scale=1024:640:flags=lanczos,fps=30" -an -c:v libvpx-vp9 -b:v 0 -crf 42 -row-mt 1 \
  -deadline good -cpu-used 2 media/portfolio/openmapping.webm
ffmpeg -hide_banner -loglevel error -y -sseof -1 -i media/portfolio/openmapping.webm -frames:v 1 \
  -vf "scale=1024:640" media/portfolio/openmapping.png
du -h media/*.mp4 media/*.gif media/portfolio/*
