#!/usr/bin/env python3
"""Clip an OpenStreetMap export to its <bounds> and keep only the routable ways.

The OSM API returns every way that crosses the requested box with all of its nodes,
so a long street can stretch the map far outside the area. This keeps the nodes
inside the box and splits each highway into the runs of consecutive inside nodes.

Usage: demo/clip-osm.py raw.osm maps/bordeaux.osm
"""
import sys
import xml.etree.ElementTree as ET

SKIPPED = {"construction", "proposed", "abandoned", "razed"}


def main(src, dst):
    root = ET.parse(src).getroot()
    b = root.find("bounds")
    box = [float(b.get(k)) for k in ("minlat", "minlon", "maxlat", "maxlon")]
    inside = {}
    for n in root.findall("node"):
        lat, lon = float(n.get("lat")), float(n.get("lon"))
        if box[0] <= lat <= box[2] and box[1] <= lon <= box[3]:
            inside[n.get("id")] = (n.get("lat"), n.get("lon"))

    out = ET.Element("osm", {"version": "0.6", "generator": "openmapping clip-osm.py",
                             "copyright": "OpenStreetMap and contributors",
                             "attribution": "http://www.openstreetmap.org/copyright",
                             "license": "http://opendatacommons.org/licenses/odbl/1-0/"})
    ET.SubElement(out, "bounds", {k: b.get(k) for k in ("minlat", "minlon", "maxlat", "maxlon")})

    runs = []
    for w in root.findall("way"):
        tags = {t.get("k"): t.get("v") for t in w.findall("tag")}
        if "highway" not in tags or tags["highway"] in SKIPPED:
            continue
        run = []
        for nd in [nd.get("ref") for nd in w.findall("nd")] + [None]:
            if nd in inside:
                run.append(nd)
                continue
            if len(run) >= 2:
                runs.append((w.get("id"), len(runs), run, tags))
            run = []

    used = sorted({nd for _, _, run, _ in runs for nd in run}, key=int)
    for nid in used:
        lat, lon = inside[nid]
        ET.SubElement(out, "node", {"id": nid, "lat": lat, "lon": lon})
    for wid, k, run, tags in runs:
        way = ET.SubElement(out, "way", {"id": f"{wid}{k:03d}"})
        for nd in run:
            ET.SubElement(way, "nd", {"ref": nd})
        # The Racket parser expects k before v on each tag.
        ET.SubElement(way, "tag", {"k": "highway", "v": tags["highway"]})
        if "name" in tags:
            ET.SubElement(way, "tag", {"k": "name", "v": tags["name"]})

    ET.indent(out, space=" ")
    ET.ElementTree(out).write(dst, encoding="UTF-8", xml_declaration=True)
    print(f"{len(used)} nodes, {len(runs)} ways -> {dst}")


if __name__ == "__main__":
    main(*sys.argv[1:3])
