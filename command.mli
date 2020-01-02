type start_resource = string
type start_amt = int
type end_resource = string
type end_amt = int
type structure = string

type command = 
  | Quit
  | Invalid 
  | Show
  | Buy
  | BuildRoad of int * int 
  | BuildSettlement of int 
  | BuildCity of int 
  | UpgradeSettlment of int
  | MarineTrade of start_resource * end_resource
  | PlayerTrade of start_resource * start_amt * end_resource * end_amt

val parse : string -> command

type affirmation = 
  | NotAffirmative 
  | Accept 
  | Reject 

val parse_affirmative : string -> affirmation
