# open-mapping

A small **routing engine in Racket** built on **OpenStreetMap** data: it parses `.osm` files into a graph, computes shortest paths (**Dijkstra**, **A\***) and short cycles (**travelling salesman**), and renders the result as **SVG** through a local web server.

> School project, ENSEIRB-MATMECA (semester 6), by Emeric Duchemin, Laurent Genty, Julien Miens and Tanguy Pemeja. Project report: [`Projet_S6_Mapping.pdf`](Projet_S6_Mapping.pdf) (French).

## Features

- **OSM parsing**: keeps only the routable ways from the raw map.
- **Graph construction** with GPS distances between nodes.
- **Shortest path** between two nodes (Dijkstra and A\*).
- **Travelling salesman**: shortest cycle through a set of nodes (computed on small sets, 3 nodes).
- **Web UI** served on `localhost:9000` that draws the map and the computed route as SVG.

## Getting started

Requirements: [Racket](https://racket-lang.org/).

```bash
racket src/server.rkt maps/map2.osm
```

Then open:

- `http://localhost:9000/route?start=<node-id>&end=<node-id>`: shortest path
- `http://localhost:9000/cycle?nodes=<id>,<id>,<id>`: cycle through the given nodes

Any map in `maps/` works (ENSEIRB campus, New York, Macapá…).

## Tests

```bash
make test
```

## Project structure

```
src/
  parsing.rkt             # filter OSM ways
  graph_construction.rkt  # build the graph from the filtered data
  graph.rkt               # graph structure and helpers
  gps.rkt                 # distance functions
  Dijkstra.rkt            # Dijkstra + helpers for the TSP
  route.rkt               # path between two points
  travelling.rkt          # travelling salesman
  svg.rkt                 # SVG rendering
  server.rkt              # web server
maps/                     # sample .osm maps
test/all-tests.rkt        # unit tests
```
