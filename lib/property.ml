type t = {
  name : string;
  pos : int;
  cost : int;
  base_rent : int;
  color : ANSITerminal.style list;
  house_cost : int;
  h1_rent : int;
  h2_rent : int;
  h3_rent : int;
  h4_rent : int;
  hotel_rent : int;
  mortgage : int;
  level : int ref;
}

let get_name p = p.name
let get_pos p = p.pos
let get_cost p = p.cost
let get_house_cost p = p.house_cost

let get_rent p =
  if !(p.level) = 1 then p.base_rent
  else if !(p.level) = 2 then p.base_rent * 2
  else if !(p.level) = 3 then p.h1_rent
  else if !(p.level) = 4 then p.h2_rent
  else if !(p.level) = 5 then p.h3_rent
  else if !(p.level) = 6 then p.h4_rent
  else p.hotel_rent

let get_color p = p.color
let ( = ) p1 p2 = p1.name = p2.name

let create_property name pos cost base_rent color house_cost h1_rent h2_rent
    h3_rent h4_rent hotel_rent mortgage =
  {
    name;
    pos;
    cost;
    base_rent;
    color;
    house_cost;
    h1_rent;
    h2_rent;
    h3_rent;
    h4_rent;
    hotel_rent;
    mortgage;
    level = ref 1;
  }

let upgrade_level p = p.level := !(p.level) + 1
let get_level p = !(p.level)
