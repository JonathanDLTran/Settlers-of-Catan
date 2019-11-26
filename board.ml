type player = 
  | Player1
  | Player2

type resource = string

type tile_val = int

type settlement = 
  | Uninhabited
  | Town
  | City

type roadway = 
  | Road
  | NoRoad

type hex = {
  resource : resource;
  value : tile_val;
}
