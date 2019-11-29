type start_resource = string
type start_amt = int
type end_resource = string
type end_amt = int
type structure = string

type command = 
  | Invalid 
  | Show
  | Buy
  | Build of structure
  | MarineTrade of start_resource * end_resource
  | PlayerTrade of start_resource * start_amt * end_resource * end_amt

val parse : string -> command