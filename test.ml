open OUnit2;;
open Hz_model;;
open Hz_model.Action;;
open Hz_model.Model;;

let test1 test_ctxt = assert_equal 1 1 (* (performSyn ((FocusedE EmptyHole),(Num)) Del) *)


let suite =
  "suite">:::
  ["test1">:: test1;

  ]
;;


let () =
  run_test_tt_main suite;;
;;
