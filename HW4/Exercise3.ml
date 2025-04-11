type treasure = StarBox | NameBox of string
type key = Bar | Node of key * key
type map = End of treasure | Branch of map * map | Guide of string * map

let getReady (m : map) : key list = []
