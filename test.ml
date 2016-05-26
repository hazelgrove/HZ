open OUnit2;;

open Model;;

let emptyHole = Hole "";;
let testHole = Hole "test";;

let test1 test_ctxt = assert_equal 1 1

let test2 test_ctxt = assert_equal emptyHole emptyHole

let test3 test_ctxt = assert_equal testHole testHole

let emptyPair = Pair (emptyHole,emptyHole) 

let test4 test_ctxt = assert_equal emptyPair (Pair ((Hole ""),(Hole "")))

let testPair = Pair (testHole,testHole)

let test5 test_ctxt = assert_equal testPair (Pair ((Hole "test"),(Hole "test")))

let nestedPair = Pair (Pair (testHole,testHole),emptyHole)

let test6 test_ctxt = assert_equal nestedPair (Pair (Pair ((Hole "test"),(Hole "test")),(Hole "")))

let nestedNestedPair = Pair (nestedPair,testHole)

let test7 test_ctxt = assert_equal nestedNestedPair  (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Hole "test")))

let nestedNestedPair2 = Pair (nestedPair,nestedPair)

let test8 test_ctxt = assert_equal nestedNestedPair2  (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Pair (Pair ((Hole "test"),(Hole "test")),(Hole "")))))

open Sel
open StringSel

let simpleStringSel = {startIdx = 0; endIdx = 1}

let testSel1 test_ctxt = assert_equal simpleStringSel {startIdx = 0; endIdx = 1}

let testSel2 test_ctxt = assert_equal (direction_of simpleStringSel) Right

open Models
(* Create BModel *)
let hexpSel = HSel.(InFst (OutPair Left))
let bmodel1 = BModel.make (nestedPair,hexpSel)  (* (Pair (testHole,testHole),emptyHole)    (|('test','test'),'') *)
(* Sample output  (|("test","test"),"") *)
let testABSBMODEL test_ctxt = assert_equal (BModel.show bmodel1) (nestedPair,hexpSel)

module BModelStringView = StringView (AbsBModel)

let testViewHExpView1 test_ctxt = assert_equal (BModelStringView.viewHExp testHole) "'test'" 

let testViewHExpView2 test_ctxt = assert_equal (BModelStringView.viewHExp emptyPair) "('','')" 

(* (Pair ((Pair (Pair ((Hole "test"),(Hole "test")),(Hole ""))),(Hole "test"))) *)
let testViewHExpView3 test_ctxt = assert_equal (BModelStringView.viewHExp nestedNestedPair) "((('test','test'),''),'test')" 

let testABSMODELView1 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "(|('test','test'),'')" (BModelStringView.view (AbsBModel.of_b bmodel1)) 

let hexpSel = HSel.(InSnd (OutPair Right))
(*   ((('test','test'),''),('test,'test'),'') *)
let bmodel2 = BModel.make (nestedNestedPair2,hexpSel)

let testABSMODELView2 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "((('test','test'),''),(('test','test'),'')|)" (BModelStringView.view (AbsBModel.of_b bmodel2)) 

let hexpSel3 = HSel.(InFst (InFst (OutPair Left)))
(*   ((('test','test'),''),('test,'test'),'') *)
let bmodel3 = BModel.make (nestedNestedPair2,hexpSel3)

let testABSMODELView3 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "((|('test','test'),''),(('test','test'),''))" (BModelStringView.view (AbsBModel.of_b bmodel3)) 



let hexpSel4 = HSel.(PairSelected Left)
(*   ((('test','test'),''),('test,'test'),'') *)
let bmodel4 = BModel.make (testPair,hexpSel4)

let testABSMODELView4 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "|('test','test')}" (BModelStringView.view (AbsBModel.of_b bmodel4)) 


let hexpSel5 = HSel.(InFst (InHole {startIdx=1; endIdx=2}))
(*   ('te|st,'test') *)
let bmodel5 = BModel.make (testPair,hexpSel5)

let testABSMODELView5 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "('t|e}st','test')" (BModelStringView.view (AbsBModel.of_b bmodel5)) 

let hexpSel6 = HSel.(InFst (InHole {startIdx=0; endIdx=4}))
(*   ('te|st,'test') *)
let bmodel6 = BModel.make (testPair,hexpSel6)

let testABSMODELView6 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "('|test}','test')" (BModelStringView.view (AbsBModel.of_b bmodel6)) 

let hexpSel7 = HSel.(InFst (InHole {startIdx=4; endIdx=0}))
(*   ('te|st,'test') *)
let bmodel7 = BModel.make (testPair,hexpSel7)

let testABSMODELView7 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)  "('{test|','test')" (BModelStringView.view (AbsBModel.of_b bmodel7)) 


(* finish ViewString DONE *)

let hexpS1 = HSel.(InFst (InHole {startIdx=1; endIdx=2}))
let testValidSel1 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS1)) 

let hexpS2 = HSel.(InHole {startIdx=(-1); endIdx=2})
let testValidSel2 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS2)) 

let hexpS3 = HSel.(InHole {startIdx=1; endIdx=6})
let testValidSel3 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS3)) 

let hexpS4 = HSel.(InHole {startIdx=1; endIdx=6})
let testValidSel4 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS4)) 

let hexpS5 = HSel.(OutPair Left)
let testValidSel5 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS5)) 

let hexpS6 = HSel.(PairSelected Left)
let testValidSel6 test_ctxt = assert_equal false (HSel.valid_for (testHole , hexpS6)) 

let hexpS7 = HSel.(PairSelected Left)
let testValidSel7 test_ctxt = assert_equal true (HSel.valid_for (testPair , hexpS7)) 

let hexpS8 = HSel.(OutPair Left)
let testValidSel8 test_ctxt = assert_equal true (HSel.valid_for (testPair , hexpS8)) 

let hexpS9 = HSel.(InHole {startIdx=1; endIdx=3})
let testValidSel9 test_ctxt = assert_equal false (HSel.valid_for (testPair , hexpS9)) 

let hexpS10 = HSel.(InSnd (InFst (InHole {startIdx=1; endIdx=2})))
let testValidSel10 test_ctxt = assert_equal false (HSel.valid_for (nestedNestedPair2 , hexpS10)) 


(* Test actions *)
let hexpAction1 = HSel.(OutPair Left)
let bmodelAction1 = BModel.make (testPair,hexpAction1)
let testAction1 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)   "('|}',('test','test'))"  (BModelStringView.view (AbsBModel.execute (AbsBModel.of_b bmodelAction1) Action.NewPair))

let hexpAction2 = HSel.(OutPair Right)
let bmodelAction2 = BModel.make (testPair,hexpAction2)
let testAction2 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)   "(('test','test'),'|}')"  (BModelStringView.view (AbsBModel.execute (AbsBModel.of_b bmodelAction2) Action.NewPair))

let hexpAction3 = HSel.(InFst (InHole {startIdx=0; endIdx=4}))
let bmodelAction3 = BModel.make (testPair,hexpAction3)
let testAction3 test_ctxt = assert_equal ~printer:(fun p -> Printf.sprintf "%s" p)   "('Ente|}rText','test')"  (BModelStringView.view (AbsBModel.execute (AbsBModel.of_b bmodelAction3) (Action.EnterString "EnterText")))




(* Test string selection methods *)

(* Name the test cases and group them together *)

(* add number to hexp *)
let hexpPair = (Pair ((Val 1),(Val 2)))
let simpleHsel =  HSel.(OutPair Left)
let bmodelPairs = BModel.make (hexpPair,simpleHsel)

let testAddNumber1 test_ctxt = assert_equal (Val 1) (Val 1)
let testAddNumber2 test_ctxt = assert_equal (Val 99)  (Val 99)
let testAddNumber3 test_ctxt = assert_equal  (Pair (Pair ((Val 99),(Val 1)),(Hole "test")))   (Pair (Pair ((Val 99),(Val 1)),(Hole "test"))) 
let testAddNumber4 test_ctxt = assert_equal  (AbsBModel.of_b bmodelPairs) (AbsBModel.of_b bmodelPairs) 
let testAddNumber5 test_ctxt = assert_equal  ~printer:(fun p -> Printf.sprintf "%s" p) "|(1,2)" (BModelStringView.view (AbsBModel.of_b bmodelPairs)) 

(* add is_complete to hexp AKA, there are no holes *)
let testAddNumber6 test_ctxt = assert_equal  true (HExp.is_complete hexpPair)
let testAddNumber7 test_ctxt = assert_equal  false (HExp.is_complete emptyHole)
let testAddNumber8 test_ctxt = assert_equal  false (HExp.is_complete nestedNestedPair)

(* Convert Hole with a parsable Number to an int *)
let testAddNumber9 test_ctxt = assert_equal  (Val 1) (HExp.convert_holes_to_val (Hole "1"))
let testAddNumber10 test_ctxt = assert_equal  (Hole "asdf") (HExp.convert_holes_to_val (Hole "asdf"))
let testAddNumber11 test_ctxt = assert_equal  (Val 9) (HExp.convert_holes_to_val (Val 9))
let testAddNumber12 test_ctxt = assert_equal  (Pair ((Hole ""),(Hole ""))) (HExp.convert_holes_to_val emptyPair)
let testAddNumber13 test_ctxt = assert_equal  (Pair ((Val 3),(Val 4))) (HExp.convert_holes_to_val (Pair ((Hole "3"),(Hole "4"))))
let testAddNumber14 test_ctxt = assert_equal  (Pair ((Val 3),(Val 7))) (HExp.convert_holes_to_val (Pair ((Hole "3"),(Val 7))))

(* Add addition to evaluate expression *)
(* Should this be an operator in Pair? a new language construct?  *)




let suite =
  "suite">:::
  ["test1">:: test1;
   "test2">:: test2;
   "test3">:: test3;
   "test4">:: test4;
   "test5">:: test5;
   "test6">:: test6;
   "test7">:: test7;
   "test8">:: test8;
   "testSel1">:: testSel1;
   "testSel2">:: testSel2;
   "testABSBMODEL">:: testABSBMODEL;
   "testViewHExpView1">:: testViewHExpView1;
   "testViewHExpView2">:: testViewHExpView2;
   "testViewHExpView3">:: testViewHExpView3;
   "testABSMODELView1">:: testABSMODELView1;
   "testABSMODELView2">:: testABSMODELView2;
   "testABSMODELView3">:: testABSMODELView3;
   "testABSMODELView4">:: testABSMODELView4;
   "testABSMODELView5">:: testABSMODELView5;
   "testABSMODELView6">:: testABSMODELView6;
   "testABSMODELView7">:: testABSMODELView7;
   "testValidSel1">:: testValidSel1;
   "testValidSel2">:: testValidSel2;
   "testValidSel3">:: testValidSel3;
   "testValidSel4">:: testValidSel4;
   "testValidSel5">:: testValidSel5;
   "testValidSel6">:: testValidSel6;
   "testValidSel7">:: testValidSel7;
   "testValidSel8">:: testValidSel8;
   "testValidSel9">:: testValidSel9;
   "testValidSel10">:: testValidSel10;
   "testAction1">:: testAction1;
   "testAction2">:: testAction2;
   "testAction3">:: testAction3;
   "testAddNumber1">:: testAddNumber1;
   "testAddNumber2">:: testAddNumber2;
   "testAddNumber3">:: testAddNumber3;
   "testAddNumber4">:: testAddNumber4;
   "testAddNumber5">:: testAddNumber5;
   "testAddNumber6">:: testAddNumber6;
   "testAddNumber7">:: testAddNumber7;
   "testAddNumber8">:: testAddNumber8;
   "testAddNumber9">:: testAddNumber9;
   "testAddNumber10">:: testAddNumber10;
   "testAddNumber11">:: testAddNumber11;
   "testAddNumber12">:: testAddNumber12;
   "testAddNumber13">:: testAddNumber13;
   "testAddNumber14">:: testAddNumber14;
   (* "testStringView">:: testStringView *)
  ]
;;


open Tyxml_js.Html5
open Dom_html
open Tyxml_js.To_dom
(* let pair = ToDom.of_div X.(
      div ~a:[a_class ["pair"]; a_contenteditable false] []
    ) in 
    Dom.appendChild pair (leftHole left_contents);
    Dom.appendChild pair (separator ());
    Dom.appendChild pair (rightHole ());
    pair *)

(* Test ToHTML View *)
let testMakeHTML5 test_ctxt = assert_equal  (Js.string "container") (Js.string "container")


let suiteToHTML =
  "suiteToHTML">:::
  ["testMakeHTML5">:: testMakeHTML5;
  ]
;;







let () =
  run_test_tt_main suite;;
(* run_test_tt_main suiteToHTML *)
;;
