type t = {
  name : string;
  position : int;
  properties : Property.t list;
  money : int;
  in_jail : bool;
  rounds_in_jail : int;
}

let empty =
  {
    name = "";
    position = -1;
    properties = [];
    money = -1;
    in_jail = false;
    rounds_in_jail = 0;
  }

let create_player name =
  {
    name = String.uppercase_ascii name;
    position = 0;
    properties = [];
    money = 1500;
    in_jail = false;
    rounds_in_jail = 0;
  }

let is_empty player =
  player.name = "" && player.position = -1 && player.money = -1

let get_name player = player.name
let get_position player = player.position

let set_position player new_pos =
  {
    name = player.name;
    position = new_pos;
    properties = player.properties;
    money = player.money;
    in_jail = player.in_jail;
    rounds_in_jail = player.rounds_in_jail;
  }

let get_properties player = player.properties

let add_property player property =
  {
    name = player.name;
    position = player.position;
    properties = property :: player.properties;
    money = player.money;
    in_jail = player.in_jail;
    rounds_in_jail = player.rounds_in_jail;
  }

let remove_property player property =
  {
    name = player.name;
    position = player.position;
    properties = List.filter (fun x -> x <> property) player.properties;
    money = player.money;
    in_jail = player.in_jail;
    rounds_in_jail = player.rounds_in_jail;
  }

let has_set player set_color : bool =
  let open ANSITerminal in
  let property_list = get_properties player in
  let properties_of_color =
    List.filter
      (fun property -> Property.get_color property = set_color)
      property_list
  in
  if set_color = [ yellow; on_red ] || set_color = [ blue ] then
    List.length properties_of_color = 2
  else List.length properties_of_color = 3

let has_any_set player : bool =
  let open ANSITerminal in
  if has_set player [ yellow; on_red ] then true
  else if has_set player [ cyan ] then true
  else if has_set player [ magenta ] then true
  else if has_set player [ red; on_yellow ] then true
  else if has_set player [ red ] then true
  else if has_set player [ yellow ] then true
  else if has_set player [ green ] then true
  else if has_set player [ blue ] then true
  else false

let get_money player = player.money

let add_money player money =
  {
    name = player.name;
    position = player.position;
    properties = player.properties;
    money = player.money + money;
    in_jail = player.in_jail;
    rounds_in_jail = player.rounds_in_jail;
  }

let remove_money player money =
  {
    name = player.name;
    position = player.position;
    properties = player.properties;
    money = player.money - money;
    in_jail = player.in_jail;
    rounds_in_jail = player.rounds_in_jail;
  }

let is_in_jail player = player.in_jail

let set_jail player in_jail =
  if in_jail = false then
    {
      name = player.name;
      position = player.position;
      properties = player.properties;
      money = player.money;
      in_jail;
      rounds_in_jail = 0;
    }
  else
    {
      name = player.name;
      position = player.position;
      properties = player.properties;
      money = player.money;
      in_jail;
      rounds_in_jail = player.rounds_in_jail;
    }

let get_rounds_in_jail player = player.rounds_in_jail

let set_rounds_in_jail player rounds =
  {
    name = player.name;
    position = player.position;
    properties = player.properties;
    money = player.money;
    in_jail = player.in_jail;
    rounds_in_jail = rounds;
  }
