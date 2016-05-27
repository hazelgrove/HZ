open OUnit2;;
open Hz_model;;
open Hz_model.Action;;
open Hz_model.Model;;
open Hz_model.Model.ZExp;;
open Hz_model.Model.ZType;;
open Hz_model.Model.HType;;
open Hz_model.Model.HExp;;

let test1 test_ctxt = assert_equal (ZExp.FocusedE EmptyHole,HType.Num) (performSyn (ZExp.FocusedE (NumLit 1),Num) Del)

let test13a test_ctxt = assert_equal (ZExp.RightAsc ((NumLit 1),(ZType.FirstArrow (ZType.FocusedT (HType.Num),HType.Num)) ),HType.Num) (performSyn (ZExp.RightAsc ((NumLit 1),(ZType.FocusedT (Arrow (HType.Num,HType.Num)))),HType.Num) (Move FirstChild))

let test13b test_ctxt = assert_equal (ZExp.RightAsc ((NumLit 1),(ZType.FocusedT (Arrow (HType.Num,HType.Num)))),HType.Num) (performSyn (ZExp.RightAsc ((NumLit 1),(ZType.FirstArrow (ZType.FocusedT (HType.Num),HType.Num)) ),HType.Num) (Move Parent))

let test13c test_ctxt = assert_equal (ZExp.RightAsc ((NumLit 1),(ZType.FocusedT (Arrow (HType.Num,HType.Num)))),HType.Num) (performSyn (ZExp.RightAsc ((NumLit 1),(ZType.SecondArrow (HType.Num,ZType.FocusedT (HType.Num))) ),HType.Num) (Move Parent))



let suite =
  "suite">:::
  ["test1">:: test1;
  "test13a">:: test13a;
  "test13b">:: test13b;
  "test13c">:: test13c;

  ]
;;


let () =
  run_test_tt_main suite;;
;;
