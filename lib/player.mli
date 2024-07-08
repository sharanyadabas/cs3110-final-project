type t
(** [d] is the type that represents a player of the Monopoly game *)

val empty : t
(** [empty] is a player with no name and negative money and index values to
    indicitate that it is not a real player. *)

val is_empty : t -> bool
(** [is_empty player] is whether [player] is an empty player determined by
    having no name, negative money, and negative position. *)

val create_player : string -> t
(** [create_player name] is a player starting at index of 0 with the identifier
    [name] starting with 1500 dollars and no properties. *)

val get_name : t -> string
(** [get_name player] is the name of [player]. *)

val get_position : t -> int
(** [get_position player] is the index of [player] on the board. *)

val set_position : t -> int -> t
(** [set_position player new_pos] is a player with the fields of [player] but
    with index updated on the board accoring to [new_pos]. *)

val get_properties : t -> Property.t list
(** [get_properties player] is the list of properties owned by [player]. *)

val add_property : t -> Property.t -> t
(** [add_property player property] is a player with the fields of [player] but
    with [property] added to their properties. *)

val remove_property : t -> Property.t -> t
(** [add_property player property] is a player with the fields of [player] but
    with [property] removed from their properties. *)

val has_set : t -> ANSITerminal.style list -> bool
(** [has_set player set_color] is whether or not the player has all the
    properties of that [set_color] color*)

val has_any_set : t -> bool
(** [has_any_set player] is whether or not the player has a set of any color*)

val get_money : t -> int
(** [get_properties player] is the amount of money owned by [player]. *)

val add_money : t -> int -> t
(** [add_money player money] is a player with the fields of [player] but with
    [money] added to their money. *)

val remove_money : t -> int -> t
(** [add_money player money] is a player with the fields of [player] but with
    [money] removed from their money. *)

val is_in_jail : t -> bool
(** [is_in_jail player] is whether the [player] is in Jail or not. *)

val set_jail : t -> bool -> t
(** [set_jail player in_jail] sets the [player]'s Jail status according to
    [in_jail]. *)

val get_rounds_in_jail : t -> int
(** [get_rounds_in_jail player] is how many rounds [player] has been in Jail. *)

val set_rounds_in_jail : t -> int -> t
(** [set_rounds_in_jail player rounds] sets the [player]'s number of rounds in
    jail to [rounds]. *)
