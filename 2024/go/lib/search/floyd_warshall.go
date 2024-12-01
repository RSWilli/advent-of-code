package aocsearch

type Edge struct {
	From int
	To   int
}

// FloydWarshall implements the all pairs shortest path algorithm
//
// the input map will be modified. Nodes are expected to be numbered 0-[nodes]. Unset edges are treated as infinity.
func FloydWarshall[N comparable](nodes int, edges map[Edge]int) {
	for k := range nodes {
		for i := range nodes {
			for j := range nodes {
				iToJ, iToJOk := edges[Edge{From: i, To: j}]

				iToK, iToKOk := edges[Edge{From: i, To: k}]
				kToJ, kToJOk := edges[Edge{From: k, To: j}]

				// if the way i->k->j is shorter than i->j, remember it

				if !iToKOk || !kToJOk {
					continue // one of the edges is infinite
				}

				if !iToJOk || (iToJ > iToK+kToJ) {
					edges[Edge{From: i, To: j}] = iToK + kToJ
				}
			}
		}
	}
}
