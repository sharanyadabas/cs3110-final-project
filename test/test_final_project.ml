open OUnit2
open Final_project
open ANSITerminal

let new_player = Player.create_player "Bob"

let new_player_tests =
  "test suite for new player"
  >::: [
         ( "testing position" >:: fun _ ->
           assert_equal Player.(get_position new_player) 0 );
         ( "testing properties" >:: fun _ ->
           assert_equal Player.(get_properties new_player) [] );
         ( "testing money" >:: fun _ ->
           assert_equal Player.(get_money new_player) 1500 );
         ( "testing name" >:: fun _ ->
           assert_equal Player.(get_name new_player) "BOB" );
         ( "testing in_jail" >:: fun _ ->
           assert_equal Player.(is_in_jail new_player) false );
         ( "testing rounds in jail" >:: fun _ ->
           assert_equal Player.(get_rounds_in_jail new_player) 0 );
         ( "testing has set" >:: fun _ ->
           assert_equal Player.(has_any_set new_player) false );
         ( "testing empty" >:: fun _ ->
           assert_equal Player.(is_empty new_player) false );
       ]

let property_to_add1 =
  Property.create_property "prop1" 1 1 1 [ blue ] 0 1 1 1 1 1 1

let property_to_add2 =
  Property.create_property "prop2" 1 1 1 [ blue ] 0 1 1 1 1 1 1

let () = Property.upgrade_level property_to_add2

let changed_player =
  Player.(
    add_money
      (add_property
         (add_property (set_position new_player 15) property_to_add1)
         property_to_add2)
      50000)

let modified_player_tests =
  "test suite for modified player"
  >::: [
         ( "testing position" >:: fun _ ->
           assert_equal Player.(get_position changed_player) 15 );
         ( "testing properties 1" >:: fun _ ->
           assert_equal
             (List.is_empty Player.(get_properties changed_player))
             false );
         ( "testing properties 2" >:: fun _ ->
           assert_equal
             (Property.get_name
                (List.nth Player.(get_properties changed_player) 0))
             "prop2" );
         ( "testing properties 3" >:: fun _ ->
           assert_equal
             (Property.get_pos
                (List.nth Player.(get_properties changed_player) 0))
             1 );
         ( "testing properties 4" >:: fun _ ->
           assert_equal
             (Property.get_cost
                (List.nth Player.(get_properties changed_player) 0))
             1 );
         ( "testing properties 5" >:: fun _ ->
           assert_equal
             (Property.get_rent
                (List.nth Player.(get_properties changed_player) 1))
             1 );
         ( "testing properties 6" >:: fun _ ->
           assert_equal
             (Property.get_rent
                (List.nth Player.(get_properties changed_player) 0))
             2 );
         ( "testing money" >:: fun _ ->
           assert_equal Player.(get_money changed_player) 51500 );
         ( "testing name" >:: fun _ ->
           assert_equal Player.(get_name changed_player) "BOB" );
         ( "testing any set" >:: fun _ ->
           assert_equal Player.(has_any_set changed_player) true );
         ( "testing specific set" >:: fun _ ->
           assert_equal Player.(has_set changed_player [ blue ]) true );
         ( "testing other set" >:: fun _ ->
           assert_equal Player.(has_set changed_player [ green ]) false );
       ]

let changed_player_2 =
  Player.(
    remove_money
      (remove_property
         (remove_property (set_position changed_player ~-20) property_to_add1)
         property_to_add2)
      52000)

let modified_player_tests_2 =
  "test suite for modified player 2"
  >::: [
         ( "testing position" >:: fun _ ->
           assert_equal Player.(get_position changed_player_2) ~-20 );
         ( "testing properties" >:: fun _ ->
           assert_equal
             (List.is_empty Player.(get_properties changed_player_2))
             true );
         ( "testing money" >:: fun _ ->
           assert_equal Player.(get_money changed_player_2) ~-500 );
         ( "testing name" >:: fun _ ->
           assert_equal Player.(get_name changed_player_2) "BOB" );
       ]

let empty_player = Player.empty

let empty_player_tests =
  "test suite for empty player"
  >::: [
         ( "testing position" >:: fun _ ->
           assert_equal Player.(get_position empty_player) ~-1 );
         ( "testing properties" >:: fun _ ->
           assert_equal
             (List.is_empty Player.(get_properties empty_player))
             true );
         ( "testing money" >:: fun _ ->
           assert_equal Player.(get_money empty_player) ~-1 );
         ( "testing name" >:: fun _ ->
           assert_equal Player.(get_name empty_player) "" );
         ( "testing is empty" >:: fun _ ->
           assert_equal Player.(is_empty empty_player) true );
         ( "testing has any set" >:: fun _ ->
           assert_equal Player.(has_any_set empty_player) false );
         ( "testing in jail" >:: fun _ ->
           assert_equal Player.(is_in_jail empty_player) false );
         ( "testing rounds in jail" >:: fun _ ->
           assert_equal Player.(get_rounds_in_jail empty_player) 0 );
       ]

let prop1 =
  Property.create_property "Prop 1" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let prop2 = Property.create_property "Prop 2" 1 100 20 [ green ] 0 1 1 1 1 1 1
let prop3 = Property.create_property "Prop 1" 1 100 20 [ green ] 0 1 1 1 1 1 1

let prop_extr =
  Property.create_property "RRRRRRRR" 99999999999 0 9999 [ default ] 0 1 1 1 1 1
    1

let test_property =
  "test suite for property"
  >::: [
         ( "get_name" >:: fun _ ->
           assert_equal "Prop 1" (Property.get_name prop1) );
         ("get_pos" >:: fun _ -> assert_equal 1 (Property.get_pos prop1));
         ( "get_pos extr" >:: fun _ ->
           assert_equal 99999999999 (Property.get_pos prop_extr) );
         ("get_cost" >:: fun _ -> assert_equal 100 (Property.get_cost prop1));
         ( "get_cost extr" >:: fun _ ->
           assert_equal 0 (Property.get_cost prop_extr) );
         ("get_rent" >:: fun _ -> assert_equal 20 (Property.get_rent prop1));
         ( "get_rent extr" >:: fun _ ->
           assert_equal 9999 (Property.get_rent prop_extr) );
         ( "get_color" >:: fun _ ->
           assert_equal [ cyan ] (Property.get_color prop1) );
         ( "get_house_cost" >:: fun _ ->
           assert_equal 15 (Property.get_house_cost prop1) );
         ("( = )" >:: fun _ -> assert_equal false Property.(prop1 = prop2));
         ("( = )" >:: fun _ -> assert_equal true Property.(prop1 = prop1));
         ("( = )" >:: fun _ -> assert_equal true Property.(prop1 = prop3));
       ]

let upgrade_n_times prop n =
  for _ = 0 to n - 1 do
    Property.upgrade_level prop
  done

let prop_lvl1 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let prop_lvl2 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let () = upgrade_n_times prop_lvl2 1

let prop_lvl3 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let () = upgrade_n_times prop_lvl3 2

let prop_lvl4 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let () = upgrade_n_times prop_lvl4 3

let prop_lvl5 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let () = upgrade_n_times prop_lvl5 4

let prop_lvl6 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let () = upgrade_n_times prop_lvl6 5

let prop_lvl7 =
  Property.create_property "Prop" 1 100 20 [ cyan ] 15 60 80 100 120 140 160

let () = upgrade_n_times prop_lvl7 6

let test_property_level =
  "test suite for property levels"
  >::: [
         ("lvl1" >:: fun _ -> assert_equal 20 (Property.get_rent prop_lvl1));
         ("lvl2" >:: fun _ -> assert_equal 40 (Property.get_rent prop_lvl2));
         ("lvl3" >:: fun _ -> assert_equal 60 (Property.get_rent prop_lvl3));
         ("lvl4" >:: fun _ -> assert_equal 80 (Property.get_rent prop_lvl4));
         ("lvl5" >:: fun _ -> assert_equal 100 (Property.get_rent prop_lvl5));
         ("lvl6" >:: fun _ -> assert_equal 120 (Property.get_rent prop_lvl6));
         ("lvl7" >:: fun _ -> assert_equal 140 (Property.get_rent prop_lvl7));
       ]

let _ = run_test_tt_main new_player_tests
let _ = run_test_tt_main modified_player_tests
let _ = run_test_tt_main modified_player_tests_2
let _ = run_test_tt_main empty_player_tests
let _ = run_test_tt_main test_property
let _ = run_test_tt_main test_property_level
