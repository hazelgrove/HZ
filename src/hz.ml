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
  let hotdog = hzdiv "hotdog" []
  let ascChar = hzdiv "asc" []
  let lAscChar =  hzdiv "lAsc" []
  let rAscChar =  hzdiv "rAsc" []

  let rec of_htype (htype : HTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match htype with
    | HTyp.Num -> hzdiv "Num" []
    | HTyp.Arrow (fst,snd) -> hzdiv "Arrow" [hzdiv "leftParens" []; of_htype (fst); hzdiv "arrow" []; of_htype (snd); hzdiv "rightParens" []]
    | HTyp.Hole ->  hzdiv "Hole" []
    | HTyp.Sum (fst,snd) -> hzdiv "Sum" [hzdiv "leftParens" []; of_htype (fst); hzdiv "plusSign" []; of_htype (snd); hzdiv "rightParens" []]

  let rec of_hexp (hexp : HExp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match hexp with
    | HExp.Lam (var,exp) -> hzdiv "Lam" [(div ~a:[a_class ["HZElem";"lambda"]] []); hzdiv "hexp" [pcdata var]; (div ~a:[a_class ["HZElem";"dot"]] []); hzdiv "hexp" [of_hexp exp]]
    | HExp.Asc (hexp,htype) -> hzdiv "Asc" [hzdiv "hexp" [of_hexp hexp]; ascChar; hzdiv "hexp" [of_htype htype]]
    | HExp.Var str -> hzdiv "Var" [pcdata str]
    | HExp.Ap (e1, e2) -> hzdiv "Ap" [of_hexp e1; hzdiv "leftParens" []; of_hexp e2; hzdiv "rightParens" []]
    | HExp.NumLit num -> hzdiv "NumLit" [pcdata (string_of_int num)]
    | HExp.Plus (n1,n2) -> hzdiv "Plus" [hzdiv "leftParens" [];(of_hexp n1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_hexp n2); hzdiv "rightParens" []]
    | HExp.EmptyHole ->  hzdiv "EmptyHole" []
    | HExp.NonEmptyHole hc -> hzdiv "NonEmptyHole" [hzdiv "lNE" []; of_hexp hc; hzdiv "rNE" []]
    | HExp.Inj (side,exp) -> hzdiv "Inj" [hzdiv "inj" []; hzdiv "leftParens" []; of_hexp exp; hzdiv "rightParens" []]
    | HExp.Case (e,(var1,exp1),(var2,exp2)) ->
      hzdiv "Case" [
        hzdiv "case" [];
        hzdiv "leftParens" [];
        hzdiv "exp" [of_hexp e];
        hzdiv "comma" [];
        hzdiv "var1" [pcdata var1];
        hzdiv "dot" [];
        hzdiv "exp1" [of_hexp exp1];
        hzdiv "comma" [];
        hzdiv "var2" [pcdata var2];
        hzdiv "dot" [];
        hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" []
      ]  (* case(e ̇, x.e ̇1, y.eˆ2) *)

  let rec of_ztype (ztype : ZTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match ztype with
    | ZTyp.CursorT htype -> hzdiv "CursorT" [lAscChar; (of_htype htype) ;rAscChar]
    | ZTyp.LeftArrow  (ztype, htype) -> hzdiv "LeftArrow"  [hzdiv "leftParens" [];(of_ztype ztype); hzdiv "arrow" []; (of_htype htype); hzdiv "rightParens" []]
    | ZTyp.RightArrow (htype, ztype) -> hzdiv "RightArrow" [hzdiv "leftParens" [];(of_htype htype); hzdiv "arrow" []; (of_ztype ztype); hzdiv "rightParens" []]
    | ZTyp.LeftSum (ztype, htype) -> hzdiv "LeftSum"  [hzdiv "leftParens" [];(of_ztype ztype); hzdiv "plusSign" []; (of_htype htype); hzdiv "rightParens" []]
    | ZTyp.RightSum (htype,ztype) -> hzdiv "RightSum" [hzdiv "leftParens" [];(of_htype htype); hzdiv "plusSign" []; (of_ztype ztype); hzdiv "rightParens" []]

  let rec of_zexp (zexp : ZExp.t ) :  [> Html_types.div ] Tyxml_js.Html.elt  =
    match zexp with
    | ZExp.RightAsc (e, asc) -> hzdiv "RightAsc" [(of_hexp e); hzdiv "asc" []; (of_ztype asc)]
    | ZExp.LeftAsc (e, asc) -> hzdiv "LeftAsc" [(of_zexp e); hzdiv "asc" []; (of_htype asc)]
    | ZExp.CursorE hexp -> hzdiv "CursorE" [lAscChar; (of_hexp hexp); rAscChar]
    | ZExp.LamZ (var,exp) -> hzdiv "LamZ" [(div ~a:[a_class ["HZElem";"lambda"]] []); hzdiv "var" [pcdata var];(div ~a:[a_class ["HZElem";"dot"]] []); hzdiv "hexp" [of_zexp exp]]
    | ZExp.LeftAp (e1,e2) -> hzdiv "LeftAp" [of_zexp e1; (div ~a:[a_class ["HZElem";"lparens"]] []); of_hexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [])]
    | ZExp.RightAp (e1,e2) ->  hzdiv "RightAp" [of_hexp e1; (div ~a:[a_class ["HZElem";"lparens"]] []); of_zexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [])]
    | ZExp.LeftPlus (num1,num2) -> hzdiv "LeftPlus" [(of_zexp num1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_hexp num2)]
    | ZExp.RightPlus (num1,num2) -> hzdiv "RightPlus" [(of_hexp num1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_zexp num2)]
    | ZExp.NonEmptyHoleZ e ->  hzdiv "NonEmptyHoleZ" [hzdiv "lNZ" []; of_zexp e; hzdiv "rNZ" []]
    | ZExp.InjZ (side,exp) -> hzdiv "Inj" [hzdiv "inj" []; hzdiv "leftParens" []; of_zexp exp; hzdiv "rightParens" []]
    | ZExp.CaseZ1 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv "Case" [
        hzdiv "case" []; hzdiv "leftParens" []; hzdiv "exp2" [of_zexp e];
        hzdiv "comma" []; hzdiv "var1" [pcdata var1];
        hzdiv "dot" []; hzdiv "exp1" [of_hexp exp1];
        hzdiv "comma" []; hzdiv "var2" [pcdata var2];
        hzdiv "dot" []; hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" []
      ]
    | ZExp.CaseZ2 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv "Case" [
        hzdiv "case" []; hzdiv "leftParens" []; hzdiv "exp2" [of_hexp e];
        hzdiv "comma" []; hzdiv "var1" [pcdata var1];
        hzdiv "dot" []; hzdiv "exp1" [of_zexp exp1];
        hzdiv "comma" []; hzdiv "var2" [pcdata var2];
        hzdiv "dot" []; hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" []
      ]
    | ZExp.CaseZ3 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv "Case" [
        hzdiv "case" []; hzdiv "leftParens" []; hzdiv "exp2" [of_hexp e]; hzdiv "comma" [];
        hzdiv "var1" [pcdata var1]; hzdiv "dot" [];
        hzdiv "exp1" [of_hexp exp1]; hzdiv "comma" [];
        hzdiv "var2" [pcdata var2]; hzdiv "dot" []; hzdiv "exp2" [of_zexp exp2]; hzdiv "rightParens" []
      ]

end

(* TODO: put common utils somewhere sensible *)
exception No_value
let opt_get opt = match opt with Some x -> x | _ -> raise No_value

module Ev = Lwt_js_events

let blur_div id =
  let e = Dom_html.getElementById(id) in
  Js.Opt.case (Dom_html.CoerceTo.input e)
    (fun e -> ()) (fun e -> e##blur)

let click_button id =
  (* Firebug.console##log(Js.string id); *)
  let e = Dom_html.getElementById(id) in
  Js.Opt.case (Dom_html.CoerceTo.button e)
    (fun e -> ()) (fun e -> e##click)

let bind_event ev elem handler =
  let handler evt _ = handler evt in
  Ev.(async @@ (fun () -> ev elem handler))

(* create an input and a reactive signal tracking its
 * string value *)
let r_input attrs id =
  let rs, rf = S.create "" in
  (* let key_handler evt =
     Dom_html.stopPropagation evt
     (* if evt##.keyCode = 13 then (Firebug.console##log(Js.string "Enter")) else (Dom_html.stopPropagation evt); *)
     (* if evt##.keyCode = 27 then (Firebug.console##log(Js.string "esc");  blur_div id; true) else (Dom_html.stopPropagation evt; true) *)
     in *)
  let key_handler evt =
    if evt##.keyCode = 13 then (false) else (Dom_html.stopPropagation evt; true)
  in

  let esc_Handler evt =
    Firebug.console##log(Js.string "esc_Handler");
    Firebug.console##log(evt);
    if evt##.keyCode = 27 then (blur_div id; false) else (Dom_html.stopPropagation evt; true)
  in

  let i_elt = Html5.(input ~a:[attrs; a_class ["form-control"]; a_onkeypress key_handler; a_onkeyup esc_Handler] ();  ) in
  let i_dom = To_dom.of_input i_elt in
  let _ = bind_event Ev.inputs i_dom (fun _ ->
      Lwt.return @@
      (rf (Js.to_string (i_dom##.value)))) in
  (rs, i_elt, i_dom)


module View = struct

  let focus_on_id id =
    let e = Dom_html.getElementById(id) in
    Js.Opt.case (Dom_html.CoerceTo.input e)
      (fun e -> ()) (fun e -> e##focus)

  let click_on_id id =
    let e = Dom_html.getElementById(id) in
    Js.Opt.case (Dom_html.CoerceTo.input e)
      (fun e -> ()) (fun e -> e##click)

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
            (* Firebug.console##log(evt); *)
            (*   Firebug.console##log(Js.string "hot_key");
                 Firebug.console##log(hot_key); *)
            (if evt##.keyCode == hot_key then Action.performSyn Ctx.empty action else (raise NotImplemented))
              (* if evt##.keyCode == hot_key then Action.performSyn Ctx.empty action; (); *)
              (* if evt##.keyCode == hot_key then (
                 (Action.performSyn Ctx.empty action);
                 ()); *)
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
    let action_input_input_button action conv btn_label input_id key_code =
      let button_id = input_id ^"_button" in
      let input_id_1 = (input_id^ "_1") in
      let input_id_2 = (input_id^ "_2") in
      let i_rs, i_elt, i_dom = r_input (Html.a_id input_id_1) input_id_1 in
      let i_rs_2, i_elt_2, i_dom2 = r_input (Html.a_id input_id_2) input_id_2 in
      (* bind_event Ev.keypresses Dom_html.document match_function; *)
      bind_event Ev.keyups Dom_html.document (fun evt ->
          if evt##.keyCode = key_code then (focus_on_id input_id_1);
          Lwt.return @@  rf ((React.S.value rs))) ;
      bind_event Ev.keypresses i_dom (fun evt ->
          begin
            if evt##.keyCode = 13 then (click_button button_id; blur_div input_id_1) else ()
          end;
          Lwt.return @@ ());
      bind_event Ev.keypresses i_dom2 (fun evt ->
          begin
            if evt##.keyCode = 13 then (click_button button_id; blur_div input_id_2) else ()
          end;
          Lwt.return @@ ());
      Html5.(div  ~a:[a_class ["input-group"]] [
          i_elt;
          i_elt_2;
          span ~a:[a_class ["input-group-btn"]] [
            button ~a:[Html.a_class ["btn";"btn-default"];  a_id button_id;
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
    let action_input_button action conv btn_label input_id key_code =
      let button_id = input_id ^"_button" in
      let i_rs, i_elt, i_dom = r_input (Html.a_id input_id) input_id in
      bind_event Ev.keyups Dom_html.document (fun evt ->
          (* Firebug.console##log(evt);
             Firebug.console##log(key_code); *)
          if evt##.keyCode = key_code then (focus_on_id input_id);
          Lwt.return @@  rf ((React.S.value rs))) ;
      bind_event Ev.keypresses i_dom (fun evt ->
          begin
            if evt##.keyCode = 13 then (click_button button_id; blur_div input_id) else ()
          end;
          Lwt.return @@ ());
      Html5.(div  ~a:[a_class ["input-group"]] [
          i_elt;
          span ~a:[a_class ["input-group-btn"]] [
            button ~a:[Html.a_class ["btn";"btn-default"];  a_id (button_id);
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
                div ~a:[a_class ["Model"]] [zexp_view]];
            div ~a:[a_class ["row";"marketing"]] [
              div ~a:[a_class ["col-lg-3"; "col-md-3"; "col-sm-3"]] [
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
              div ~a:[a_class ["col-lg-3"; "col-md-3"; "col-sm-3"]] [
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
              div ~a:[a_class ["col-lg-6"; "col-md-6"; "col-sm-6"]] [
                div ~a:[a_class ["panel";"panel-default"]] [
                  div ~a:[a_class ["panel-title"]] [pcdata "Construction"];
                  div ~a:[a_class ["panel-body"]] [
                    (action_button (Action.Construct Action.SAsc) "construct asc (:)" 58);
                    br ();
                    (action_input_button
                       (fun v -> Action.Construct (Action.SVar v))
                       (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                       "construct var (v)" "var_input" 86);
                    (action_input_button
                       (fun v -> Action.Construct (Action.SLam v))
                       (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                       "construct lam (\\)" "lam_input" 220);
                    (action_button (Action.Construct Action.SAp) "construct ap ( ( )" 40);
                    br ();
                    (action_input_button
                       (fun n -> Action.Construct (Action.SLit n))
                       (fun s -> try Some (int_of_string s) with Failure _ -> None)
                       "construct lit (#)" "lit_input" 51);
                    (action_button (Action.Construct Action.SPlus) "construct plus (+)" 43);
                    br ();
                    (action_button (Action.Construct (Action.SInj HExp.L)) "construct inj L (l)" 108);
                    br ();
                    (action_button (Action.Construct (Action.SInj HExp.R)) "construct inj R (r)" 114);
                    br ();
                    (action_input_input_button
                       (fun (v1,v2) -> Action.Construct (Action.SCase (v1,v2)))
                       (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                       "construct case (c)" "case_input" 67);
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
  Lwt.return ()
let _ = Lwt_js_events.onload () >>= main
