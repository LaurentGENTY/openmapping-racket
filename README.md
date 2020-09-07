# oppen-mapping project : Helping you to find your way home

This project implements several path-finding methods such as the A* algorithm, the Dijkstra algorithm, and the travelling-Salesman problem. In order to answer this differents algorithm a graph structure has been implemented.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

You need :
* racket
* internet browser
* internet connection 
* time 

### Installing

Just download the code from the Git repository

## Makefile 
You can :
* run the tests :
** make test
*clean the repository
** make clean
* clean the repository and then run the tests
** make

## Running test 
To run the different tets you can 
* To run the unitarian tests :
  ** racket tests/all-test
*To run the tests on big maps 
  ** racket tests/test-paths
You can also use the Makefile :
make test  

## Running server 
You can just write down these lines to launch the server from the root of the project repository : racket src/server maps/maps2.osm
You can also easily change map2.osm by another existing map.  
Notice that the cyles can only be computed with three nodes in the cycle, and only if a cycle exist.

### Tree structure of the repository
*/maps 
      ** several maps are given
*/src 
      ** Dijkstra.rkt -> implements the Dijkstra algorithm and sevreal useful functions for the travelling-Salesman problem
      ** gps.rkt -> implements the distance functions
      ** graph.rkt -> implements the different function useful to maipulate graphs and implements the graph structure
      ** parsing.rkt -> provide the functions which filters the list given by open street map to keep only the right ways
      ** graph_construction.rkt -> provide the functions which construct based on a filtered list the graph 
      ** route.rkt -> implements the functions which compute the way between two points
      ** server.rkt -> file which contains the running server code 
      ** svg.rkt -> Functions wich construct a svg (image displayed by the server)
      ** travelling.rkt -> Relative file about the Travelling-Salesman problem
*/tests
      ** all-tests.rkt -> provide unitarian tests
      ** test-graphs -> tests the different functions (routing) on a big graph
*/rapport 
*/README.md
*/Makefile

## Built With

* drracket : IDE

## Authors

Students of the ENSEIRB-Matmeca :
* Emeric DUCHEMIN
* Laurent GENTY
* Julien MIENS
* Tanguy PEMEJA

## Acknowledgments

* Special thanks to G. Chambres
