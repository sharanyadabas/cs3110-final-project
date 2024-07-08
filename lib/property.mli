type t

val get_name : t -> string
(** [get_name] returns the name of the property. *)

val get_pos : t -> int
(** [get_pos] returns the position of the property on the board. *)

val get_cost : t -> int
(** [get_cost] returns the cost to purchase the property. *)

val get_house_cost : t -> int
(** [get_house_cost] is the cost of buying a house/hotel for the property. *)

val get_rent : t -> int
(** [get_rent] returns the rent that a player landing on the property must pay
    based on the property's level. *)

val get_color : t -> ANSITerminal.style list
(** [get_color] returns the color group that the property falls under. *)

val ( = ) : t -> t -> bool
(** [p1 = p2] returns true if property 1 and property 2 are the same property.
    Two properties are the same property if they have the same name. *)

val create_property :
  string ->
  int ->
  int ->
  int ->
  ANSITerminal.style list ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  int ->
  t
(** [create_property name pos cost base_rent color house_cost h1_rent h2_rent
    h3_rent h4_rent hotel_rent mortgage]
    creates a property with the given [name], position [pos], [cost],
    [base_rent], [color], [house_cost], [mortgage], and rents with the given
    number of houses/hotels specified by [h1_rent], [h2_rent], [h3_rent],
    [h4_rent], [hotel_rent]. *)

val upgrade_level : t -> unit
(** [upgrade_level p] is the property [p] with its level upgraded unless it
    already has a max level of 8, if so it prints a message. *)

val get_level : t -> int
(** [get_level p] returns the level of the property. *)
