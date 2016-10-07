open Hz_controller
open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model

open React;;
open Lwt.Infix;;

exception NotImplemented

module HTMLView = struct
  open Html

  let hzdiv str contents =  Html.(div ~a:[a_class ["HZElem";str]] contents)
  let hotdog = hzdiv  "hotdog" [pcdata "(||)"]
  let ascChar = hzdiv "asc" [pcdata ":"]
  let lAscChar =  hzdiv "lAsc" [pcdata "⊳"]
  let rAscChar =  hzdiv "rAsc" [pcdata "⊲"]

  let rec of_htype (htype : HTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match htype with
    | HTyp.Num -> hzdiv "Num" [pcdata "num"]
    | HTyp.Arrow (fst,snd) -> hzdiv "Arrow" [hzdiv "leftParens" [pcdata "("];of_htype (fst); hzdiv "arrow" [pcdata "->"]; of_htype (snd);hzdiv "rightParens" [pcdata ")"]]
    | HTyp.Hole ->  hzdiv  "Hole" [pcdata "(||)"]
    | HTyp.Sum (fst,snd) ->  hzdiv "Arrow" [hzdiv "leftParens" [pcdata "("];of_htype (fst); hzdiv "arrow" [pcdata "+"]; of_htype (snd);hzdiv "rightParens" [pcdata ")"]]

  let rec of_hexp (hexp : HExp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match hexp with
    | HExp.Lam (var,exp) -> hzdiv "Lam" [(div ~a:[a_class ["HZElem";"lambda"]] [pcdata "λ"]) ; hzdiv "hexp" [pcdata var]; (div ~a:[a_class ["HZElem";"dot"]] [pcdata "."]); hzdiv "hexp" [of_hexp exp]]
    | HExp.Asc (hexp,htype) -> hzdiv "Asc" [hzdiv "hexp" [of_hexp hexp]; ascChar; hzdiv "hexp" [of_htype htype]]
    | HExp.Var str -> hzdiv "Var" [pcdata str]
    | HExp.Ap (e1, e2) -> hzdiv "Ap" [of_hexp e1; hzdiv "leftParens" [pcdata "("]; of_hexp e2; hzdiv "rightParens" [pcdata ")"]]
    | HExp.NumLit num -> hzdiv "NumLit" [pcdata (string_of_int num)]
    | HExp.Plus (n1,n2) -> hzdiv "Plus" [hzdiv "leftParens" [pcdata "("];(of_hexp n1); (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]); (of_hexp n2);hzdiv "rightParens" [pcdata ")"]]
    | HExp.EmptyHole ->  hzdiv  "EmptyHole" [pcdata "(||)"]
    | HExp.NonEmptyHole hc -> hzdiv  "NonEmptyHole" [hzdiv "lNE" [pcdata "(|"]; of_hexp hc ;hzdiv "rNE" [pcdata "|)"]]
    | HExp.Inj (side,exp) -> hzdiv  "Inj" [hzdiv  "inj" [pcdata "inj"]; hzdiv "leftParens" [pcdata "("]; of_hexp exp; hzdiv "rightParens" [pcdata ")"]]
    | HExp.Case (e,(var1,exp1),(var2,exp2)) ->
      hzdiv  "Case" [
        hzdiv  "case" [pcdata "case"];
        hzdiv "leftParens" [pcdata "("];
        hzdiv "exp" [of_hexp e];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var1" [pcdata var1];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp1" [of_hexp exp1];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var2" [pcdata var2];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" [pcdata ")"]
      ]  (* case(e ̇, x.e ̇1, y.eˆ2) *)

  let rec of_ztype (ztype : ZTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match ztype with
    | ZTyp.CursorT htype ->  hzdiv "CursorT" [lAscChar ; (of_htype htype) ;rAscChar]
    | ZTyp.LeftArrow  (ztype, htype) ->  hzdiv "LeftArrow"  [hzdiv "leftParens" [pcdata "("];(of_ztype ztype); hzdiv "arrow" [pcdata "->"]; (of_htype htype);hzdiv "rightParens" [pcdata ")"];]
    | ZTyp.RightArrow (htype, ztype) ->  hzdiv "RightArrow" [hzdiv "leftParens" [pcdata "("];(of_htype htype); hzdiv "arrow" [pcdata "->"]; (of_ztype ztype);hzdiv "rightParens" [pcdata ")"];]
    | ZTyp.LeftSum (ztype, htype) -> hzdiv "LeftSum"  [hzdiv "leftParens" [pcdata "("];(of_ztype ztype); hzdiv "arrow" [pcdata "+"]; (of_htype htype);hzdiv "rightParens" [pcdata ")"];]
    | ZTyp.RightSum (htype,ztype) -> hzdiv "RightSum" [hzdiv "leftParens" [pcdata "("];(of_htype htype); hzdiv "arrow" [pcdata "+"]; (of_ztype ztype);hzdiv "rightParens" [pcdata ")"];]

  let rec of_zexp (zexp : ZExp.t ) :  [> Html_types.div ] Tyxml_js.Html.elt  =
    match zexp with
    | ZExp.RightAsc (e, asc) ->  hzdiv "RightAsc" [(of_hexp e) ; hzdiv "asc" [pcdata ":"]; (of_ztype asc)]
    | ZExp.LeftAsc (e, asc) ->   hzdiv "LeftAsc" [(of_zexp e) ; hzdiv "asc" [pcdata ":"]; (of_htype asc)]
    | ZExp.CursorE hexp -> hzdiv "CursorE" [lAscChar; (of_hexp hexp); rAscChar]
    | ZExp.LamZ (var,exp) -> hzdiv "LamZ" [(div ~a:[a_class ["HZElem";"lambda"]] [pcdata "λ"]) ;hzdiv "var" [pcdata var];(div ~a:[a_class ["HZElem";"dot"]] [pcdata "."]); hzdiv "hexp" [of_zexp exp]]
    | ZExp.LeftAp (e1,e2) -> hzdiv "LeftAp" [of_zexp e1; (div ~a:[a_class ["HZElem";"lparens"]] [pcdata "("]); of_hexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [pcdata ")"])]
    | ZExp.RightAp (e1,e2) ->  hzdiv "RightAp" [of_hexp e1; (div ~a:[a_class ["HZElem";"lparens"]] [pcdata "("]); of_zexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [pcdata ")"])]
    | ZExp.LeftPlus (num1,num2) -> hzdiv "LeftPlus" [(of_zexp num1); (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]); (of_hexp num2)]
    | ZExp.RightPlus (num1,num2) -> hzdiv "RightPlus" [(of_hexp num1); (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]); (of_zexp num2)]
    | ZExp.NonEmptyHoleZ e ->  hzdiv  "NonEmptyHoleZ" [hzdiv "lNZ" [pcdata "(|"]; of_zexp e ;hzdiv "rNZ" [pcdata "|)"]]
    | ZExp.InjZ (side,exp) -> hzdiv  "Inj" [hzdiv  "inj" [pcdata "inj"]; hzdiv "leftParens" [pcdata "("]; of_zexp exp; hzdiv "rightParens" [pcdata ")"]]
    | ZExp.CaseZ1 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv  "Case" [
        hzdiv  "case" [pcdata "case"];
        hzdiv "leftParens" [pcdata "("];
        hzdiv "exp2" [of_zexp e];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var1" [pcdata var1];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp1" [of_hexp exp1];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var2" [pcdata var2];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" [pcdata ")"]
      ]
    | ZExp.CaseZ2 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv  "Case" [
        hzdiv  "case" [pcdata "case"];
        hzdiv "leftParens" [pcdata "("];
        hzdiv "exp2" [of_hexp e];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var1" [pcdata var1];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp1" [of_zexp exp1];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var2" [pcdata var2];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" [pcdata ")"]
      ]
    | ZExp.CaseZ3 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv  "Case" [
        hzdiv  "case" [pcdata "case"];
        hzdiv "leftParens" [pcdata "("];
        hzdiv "exp2" [of_hexp e];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var1" [pcdata var1];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp1" [of_hexp exp1];
        hzdiv  "comma" [pcdata ","];
        hzdiv "var2" [pcdata var2];
        hzdiv  "dot" [pcdata "."];
        hzdiv "exp2" [of_zexp exp2];
        hzdiv "rightParens" [pcdata ")"]
      ]

end

(* TODO: put common utils somewhere sensible *)
exception No_value
let opt_get opt = match opt with Some x -> x | _ -> raise No_value

module Ev = Lwt_js_events
let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Ev.(async @@ (fun () -> ev elem handler))

(* create an input and a reactive signal tracking its
 * string value *)
let r_input attrs =
  let rs, rf = S.create "" in
  let key_handler evt =
    if evt##.keyCode = 13 then (
      (* Firebug.console##log(Js.string "test"); *)
      false
    ) else (Dom_html.stopPropagation evt; true)
  in

  let i_elt = Html5.(input ~a:[attrs; a_class ["form-control"]; a_onkeypress key_handler] () ) in
  let i_dom = To_dom.of_input i_elt in
  let _ = bind_event Ev.inputs i_dom (fun _ ->
      Lwt.return @@
      (rf (Js.to_string (i_dom##.value)))) in
  (rs, i_elt, i_dom)


module View = struct

  let keyActions (event) =
    match  char_of_int event##.keyCode with
    | 'w' -> Action.performSyn Ctx.empty (Action.Move (Action.Parent))
    | 'a' -> Action.performSyn Ctx.empty (Action.Move (Action.Child 1))
    | 'd' -> Action.performSyn Ctx.empty (Action.Move (Action.Child 2))
    | 's' -> Action.performSyn Ctx.empty (Action.Del)
    | 'j' -> Action.performSyn Ctx.empty (Action.Construct Action.SArrow)
    | 'k' -> Action.performSyn Ctx.empty (Action.Construct Action.SNum)
    |  'l' -> Action.performSyn Ctx.empty (Action.Construct Action.SAsc)
    |  'm' -> Action.performSyn Ctx.empty  (Action.Construct Action.SAp)
    | ',' -> Action.performSyn Ctx.empty (Action.Construct Action.SArg)
    |  ';' -> Action.performSyn Ctx.empty (Action.Construct Action.SPlus)
    |  '.' -> Action.performSyn Ctx.empty (Action.Finish)
    | _ -> raise NotImplemented
  (* |  -> Action.performSyn Ctx.empty *)

  let focus_on_id id =
    let e = Dom_html.getElementById(id) in
    Js.Opt.case (Dom_html.CoerceTo.input e)
      (fun e -> ()) (fun e -> e##focus)

  let clear_input id =
    let element = Dom_html.getElementById(id) in
    Js.Opt.case (Dom_html.CoerceTo.input element)
      (fun e -> ())
      (fun e -> e##.value := (Js.string "") )

  let view ((rs, rf) : Model.rp) =
    (* zexp view *)
    let zexp_view_rs = React.S.map (fun (zexp, _) ->
        [HTMLView.of_zexp zexp]) rs in
    let zexp_view = Html5.(R.Html5.div (ReactiveData.RList.from_signal zexp_view_rs)) in

    (* helper function for constructing simple action buttons *)
    let action_button action btn_label hot_key=
      bind_event Ev.keypresses Dom_html.document (fun evt ->
          Lwt.return @@ rf (
            Firebug.console##log(evt##.keyCode);
            Firebug.console##log(Js.string "hot_key");
            Firebug.console##log(hot_key);
            (if evt##.keyCode == hot_key then Action.performSyn Ctx.empty action else raise NotImplemented)
              (React.S.value rs) ) );
      Html5.(button ~a:[a_class ["btn";"btn-outline-primary"];
                        a_onclick (fun _ ->
                            rf (
                              Action.performSyn Ctx.empty action (React.S.value rs));
                            true);
                        R.filter_attrib
                          (a_disabled ())
                          (S.map (fun m ->
                               try
                                 let _ = Action.performSyn Ctx.empty action m in false
                               with Action.InvalidAction -> true
                                  | HExp.IllTyped -> true ) rs)
                       ] [pcdata btn_label]) in

    (* actions that take an input. the conversion function
     * goes from a string to an arg option where arg is
     * the action argument. *)
    let action_input_input_button action conv btn_label input_id match_function =
      let i_rs, i_elt, i_dom = r_input (Html.a_id (input_id^ "_1")) in
      let i_rs_2, i_elt_2, i_dom = r_input (Html.a_id (input_id^ "_2")) in
      bind_event Ev.keypresses Dom_html.document match_function;
      bind_event Ev.keypresses i_dom (fun e ->
          begin match  e##.keyCode with
            | _ -> begin
                let e = Dom_html.getElementById(btn_label) in
                Js.Opt.case (Dom_html.CoerceTo.input e)
                  (fun e -> ()) (fun e -> e##click)
              end
          end
        ; Lwt.return @@ ());
      (* <div class="well">
         <form class="form-inline">
         <div class="form-group">
          <input type="password" class="form-control input-medium" id="exampleInputPassword1" placeholder="Page path">
         			<input type="password" class="form-control input-medium" id="exampleInputPassword1" placeholder="Name">
         </div>
         <div class="form-group">
          <a href="#" class="btn btn-info">View</a>
          <a href="#" class="btn btn-primary">Edit</a>
          <a href="#" class="btn btn-danger">Delete</a>
         </div>
         </form>
         </div> *)
      Html5.(div  ~a:[a_class ["input-group"]] [
          i_elt;
          i_elt_2;
          span ~a:[a_class ["input-group-btn"]] [
            button ~a:[Html.a_class ["btn";"btn-default"];  a_id btn_label;
                       a_onclick (fun _ ->
                           let arg_1 = opt_get (conv (React.S.value i_rs)) in
                           let arg_2 = opt_get (conv (React.S.value i_rs_2)) in
                           rf (
                             Action.performSyn
                               Ctx.empty
                               (action (arg_1,arg_2))
                               (React.S.value rs));
                           clear_input input_id;
                           true
                         );
                       R.filter_attrib
                         (a_disabled ())
                         (S.l3 (fun s1 s2 m ->
                              match conv s1 with
                                Some arg ->
                                begin
                                  match conv s2 with
                                    Some arg2 ->
                                    begin
                                      try
                                        let _ = Action.performSyn Ctx.empty (action (arg,arg2)) m in
                                        false
                                      with Action.InvalidAction -> true
                                         | HExp.IllTyped -> true   end
                                  | _ -> true
                                end
                              | _ -> true) i_rs i_rs_2 rs)
                      ] [pcdata btn_label]
          ]
        ]) in

    (* actions that take an input. the conversion function
     * goes from a string to an arg option where arg is
     * the action argument. *)
    let action_input_button action conv btn_label input_id match_function =
      let i_rs, i_elt, i_dom = r_input (Html.a_id input_id) in
      bind_event Ev.keypresses Dom_html.document match_function;
      bind_event Ev.keypresses i_dom (fun e ->
          begin match  e##.keyCode with
            | _ -> begin
                let e = Dom_html.getElementById(btn_label) in
                Js.Opt.case (Dom_html.CoerceTo.input e)
                  (fun e -> ()) (fun e -> e##click)
              end
            (* | _ ->  *)
            (* raise NotImplemented *)
          end
        ; Lwt.return @@ ());
      Html5.(div  ~a:[a_class ["input-group"]] [
          i_elt;
          span ~a:[a_class ["input-group-btn"]] [
            button ~a:[Html.a_class ["btn";"btn-default"];  a_id btn_label;
                       a_onclick (fun _ ->
                           let arg = opt_get (conv (React.S.value i_rs)) in
                           rf (
                             Action.performSyn
                               Ctx.empty
                               (action arg)
                               (React.S.value rs));
                           clear_input input_id;
                           true
                         );
                       R.filter_attrib
                         (a_disabled ())
                         (S.l2 (fun s m ->
                              match conv s with
                                Some arg ->
                                begin try
                                    let _ = Action.performSyn Ctx.empty (action arg) m in false
                                  with Action.InvalidAction -> true
                                     | HExp.IllTyped -> true   end
                              | _ -> true) i_rs rs)
                      ] [pcdata btn_label]
          ]
        ]) in

    Html5.(
      div [ div  ~a:[a_class ["jumbotron"]]
              [ div  ~a:[a_class ["display-3"]] [pcdata "HZ"];
                div  ~a:[a_class ["subtext"]] [pcdata "(a reference implementation of Hazelnut)"];
                div ~a:[a_class ["Model"]] [zexp_view];];
            div ~a:[a_class ["row";"marketing"]] [
              div ~a:[a_class ["col-lg-3"; "col-md-3" ; "col-sm-3"]] [
                div ~a:[a_class ["panel";"panel-default"]] [
                  div ~a:[a_class ["panel-title"]] [pcdata "Movement"];
                  div ~a:[a_class ["panel-body"]] [
                    (action_button (Action.Move (Action.Child 1)) "move child 1 (1)" 49);
                    br ();
                    (action_button (Action.Move (Action.Child 2)) "move child 2 (2)" 50);
                    br ();
                    (action_button (Action.Move (Action.Child 3)) "move child 3 (3)" 51);
                    br ();
                    (action_button (Action.Move (Action.Parent)) "move parent (p)" 112);
                    (* br (); *)
                  ]
                ];
                div ~a:[a_class ["panel";"panel-default"]] [
                  div ~a:[a_class ["panel-title"]] [pcdata "Deletion"];
                  div ~a:[a_class ["panel-body"]] [
                    (action_button (Action.Del) "del (x)" 120);
                  ]
                ]
              ];
              div ~a:[a_class ["col-lg-3"; "col-md-3" ; "col-sm-3"]] [
                div ~a:[a_class ["panel";"panel-default"]] [
                  div ~a:[a_class ["panel-title"]] [pcdata "Types"];
                  div ~a:[a_class ["panel-body"]] [
                    (action_button (Action.Construct Action.SArrow) "construct arrow (>)" 62);
                    br ();
                    (action_button (Action.Construct Action.SNum) "construct num (n)" 110);
                    br ();
                    (action_button (Action.Construct Action.SSum) "construct sum (s)" 115);
                    br ();  ]
                ];
                div ~a:[a_class ["panel";"panel-default"]] [
                  div ~a:[a_class ["panel-title"]] [pcdata "Finishing"];
                  div ~a:[a_class ["panel-body"]] [
                    (action_button (Action.Finish) "finish (.)" 46)
                  ]
                ]
              ]
              ;
              div ~a:[a_class ["col-lg-6"; "col-md-6" ; "col-sm-6"]] [
                div ~a:[a_class ["panel";"panel-default"]] [
                  div ~a:[a_class ["panel-title"]] [pcdata "Constructs"];
                  div ~a:[a_class ["panel-body"]] [
                    (action_button (Action.Construct Action.SAsc) "construct asc (:)" 58);
                    br ();
                    (action_input_button
                       (fun v -> Action.Construct (Action.SVar v))
                       (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                       "construct var" "var_input" (fun evt ->
                           (match  char_of_int evt##.keyCode with
                            | 'v' ->focus_on_id "var_input";Dom_html.stopPropagation evt;
                            | _ -> () );
                           Lwt.return @@ rf ((React.S.value rs))));
                    (action_input_button
                       (fun v -> Action.Construct (Action.SLam v))
                       (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                       "construct lam" "lam_input" (fun evt ->
                           (match  char_of_int evt##.keyCode with
                            | '\\' -> Dom_html.stopPropagation evt; focus_on_id "lam_input";
                            | _ -> () );
                           Lwt.return @@ rf ((React.S.value rs))));
                    (action_button (Action.Construct Action.SAp) "construct ap ( ( )" 40);
                    br ();
                    (action_button (Action.Construct Action.SArg) "construct arg ( ) )" 41);
                    br ();
                    (action_input_button
                       (fun n -> Action.Construct (Action.SLit n))
                       (fun s -> try Some (int_of_string s) with Failure _ -> None)
                       "construct lit" "lit_input" (fun evt ->
                           (match  char_of_int evt##.keyCode with
                            | '#' -> Dom_html.stopPropagation evt;focus_on_id "lit_input";
                            | _ -> () );
                           Lwt.return @@ rf ((React.S.value rs))));
                    (action_button (Action.Construct Action.SPlus) "construct plus (+)" 43);
                    br ();
                    (action_button (Action.Construct (Action.SInj HExp.L)) "construct Inj L (l)" 108);
                    br ();
                    (action_button (Action.Construct (Action.SInj HExp.R)) "construct Inj R (r)" 114);
                    br ();
                    (action_input_input_button
                       (fun (v1,v2) -> Action.Construct (Action.SCase (v1,v2)))
                       (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                       "construct case" "case_input" (fun evt ->
                           (match  char_of_int evt##.keyCode with
                            | '\\' -> Dom_html.stopPropagation evt; focus_on_id "lam_input";
                            | _ -> () );
                           Lwt.return @@ rf ((React.S.value rs))));
                  ]
                ]
              ]
            ]
          ])
end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "container"))
      (fun () -> assert false)
  in
  let m = Model.empty in
  let rs, rf = React.S.create m in
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view (rs, rf))) ;
  (* bind_event Ev.keypresses doc (fun evt ->
      Lwt.return @@ rf (View.keyActions evt (React.S.value rs) ) ); *)
  (* Lwt.return @@ rf (Action.performSyn Ctx.empty (Action.Move (Action.Parent)) (React.S.value rs)) *)
  Lwt.return ()
let _ = Lwt_js_events.onload () >>= main
