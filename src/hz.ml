open Hz_controller
open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model

open React;;

module Ev = Dom_html.Event

(* generates divs from terms *)
module HTMLView = struct
  open Html

  let hzdiv str contents =  Html.(div ~a:[a_class ["HZElem";str]] contents)

  let rec of_htype (htype : HTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match htype with
    | HTyp.Num -> hzdiv "Num" []
    | HTyp.Arrow (fst,snd) -> hzdiv "Arrow" [hzdiv "leftParens" []; of_htype (fst); hzdiv "arrow" []; of_htype (snd); hzdiv "rightParens" []]
    | HTyp.Hole ->  hzdiv "Hole" []
    | HTyp.Sum (fst,snd) -> hzdiv "Sum" [hzdiv "leftParens" []; of_htype (fst); hzdiv "plusSign" []; of_htype (snd); hzdiv "rightParens" []]

  let rec of_hexp (hexp : HExp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match hexp with
    | HExp.Lam (var,exp) -> hzdiv "Lam" [(div ~a:[a_class ["HZElem";"lambda"]] []); hzdiv "hexp" [pcdata var]; (div ~a:[a_class ["HZElem";"dot"]] []); hzdiv "hexp" [of_hexp exp]]
    | HExp.Asc (hexp,htype) -> hzdiv "Asc" [hzdiv "hexp" [of_hexp hexp]; hzdiv "asc" []; hzdiv "hexp" [of_htype htype]]
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
      ]

  let lAscChar =  hzdiv "lAsc" []
  let rAscChar =  hzdiv "rAsc" []

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
    | ZExp.LeftAp (e1,e2) -> hzdiv "LeftAp" [of_zexp e1; (div ~a:[a_class ["HZElem";"leftParens"]] []); of_hexp e2; (div ~a:[a_class ["HZElem";"rightParens"]] [])]
    | ZExp.RightAp (e1,e2) ->  hzdiv "RightAp" [of_hexp e1; (div ~a:[a_class ["HZElem";"leftParens"]] []); of_zexp e2; (div ~a:[a_class ["HZElem";"rightParens"]] [])]
    | ZExp.LeftPlus (num1,num2) -> hzdiv "LeftPlus" [hzdiv "leftParens" [];(of_zexp num1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_hexp num2); hzdiv "rightParens" []]
    | ZExp.RightPlus (num1,num2) -> hzdiv "RightPlus" [hzdiv "leftParens" [];(of_hexp num1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_zexp num2); hzdiv "rightParens" []]
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

module Util = struct
  exception No_value
  let opt_get opt = match opt with Some x -> x | _ -> raise No_value
end

module JSUtil = struct
  let listen_to ev elem f =
    Dom_html.addEventListener
      elem
      ev
      (Dom_html.handler f)
      Js._false

  let listen_to_t ev elem f =
    listen_to ev elem (fun evt ->
        f evt; Js._true)

  (* create an input and a reactive signal tracking its
   * string value *)
  let r_input id =
    let rs, rf = S.create "" in
    let i_elt = Html5.(input ~a:[a_id id; a_class ["form-control"]; a_placeholder "Enter var + press Enter"]  ();  ) in
    let i_dom = To_dom.of_input i_elt in
    let _ = listen_to_t Ev.input i_dom (fun _ ->
        rf (Js.to_string (i_dom##.value))) in
    ((rs, rf), i_elt, i_dom)
end

(* generates the action palette *)
module ActionPalette = struct
  let make_palette ((rs, rf) : Model.rp) =
    let doAction action =
      rf (Action.performSyn Ctx.empty action (React.S.value rs)) in

    (* helper function for constructing simple action buttons *)
    let action_button action btn_label key_code =
      let _ = JSUtil.listen_to_t Ev.keypress Dom_html.document (fun evt ->
          if evt##.keyCode = key_code then
            try
              doAction action
            with Action.InvalidAction -> ()
          else ()) in
      Html5.(button ~a:[a_class ["btn";"btn-outline-primary"];
                        a_onclick (fun _ ->
                            doAction action;
                            true);
                        R.filter_attrib
                          (a_disabled ())
                          (S.map (fun m ->
                               try
                                 let _ = Action.performSyn Ctx.empty action m in false
                               with Action.InvalidAction -> true
                             ) rs)
                       ] [pcdata btn_label]) in

    (* actions that take an input. the conversion function
     * goes from a string to an arg option where arg is
     * the action argument. *)
    let action_input_button action conv btn_label input_id key_code =
      (* create reactive input box *)
      let (i_rs, i_rf), i_elt, i_dom = JSUtil.r_input input_id in
      let clear_input () =
        i_dom##.value := (Js.string "");
        i_rf "" in
      let button_elt = Html5.(button ~a:[
          a_class ["btn"; "btn-default"];
          a_id (input_id ^ "_button");
          a_onclick (fun _ ->
              let arg = Util.opt_get (conv (React.S.value i_rs)) in
              doAction (action arg);
              clear_input ();
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
                   end
                 | _ -> true) i_rs rs)
        ] [pcdata btn_label]) in
      let button_dom = To_dom.of_button button_elt in
      let _ = JSUtil.listen_to Ev.keypress Dom_html.document (fun evt ->
          let evt_key = evt##.keyCode in
          (* let _ = Firebug.console##log evt_key in *)
          (* let _ = Firebug.console##log key_code in *)
          if evt_key = key_code then
            begin
              i_dom##focus;
              Dom_html.stopPropagation evt;
              Js._false
            end
          else
            Js._true
        ) in
      let _ = JSUtil.listen_to Ev.keypress i_dom (fun evt ->
          let evt_key = evt##.keyCode in
          match evt_key with
          | 13 -> begin
              button_dom##click;
              i_dom##blur;
              Js._false
            end
          | 27 -> begin
              i_dom##blur;
              Js._false
            end
          | _ -> Dom_html.stopPropagation evt; Js._true
        ) in
      Html5.(div ~a:[a_class ["input-group"]] [
          span ~a:[a_class ["input-group-btn"]] [
            button_elt];i_elt;
        ]) in

    (* actions that takes two inputs. the conversion function
     * goes from a pair of strings to an arg option where arg is
     * the action argument. *)
    let action_input_input_button action conv btn_label input_id key_code =
      let input_id_1 = (input_id ^ "_1") in
      let input_id_2 = (input_id ^ "_2") in
      let (i_rs_1, i_rf_1), i_elt_1, i_dom_1 = JSUtil.r_input input_id_1 in
      let (i_rs_2, i_rf_2), i_elt_2, i_dom_2 = JSUtil.r_input input_id_2 in
      let clear_input () =
        i_dom_1##.value := (Js.string ""); i_rf_1 "";
        i_dom_2##.value := (Js.string ""); i_rf_2 "" in
      let button_elt = Html5.(button ~a:[
          a_class ["btn"; "btn-default"];
          a_id (input_id ^ "_button");
          a_onclick (fun _ ->
              let i1 = React.S.value i_rs_1 in
              let i2 = React.S.value i_rs_2 in
              let arg = Util.opt_get (conv (i1, i2)) in
              doAction (action arg);
              clear_input ();
              true
            );
          R.filter_attrib
            (a_disabled ())
            (S.l3 (fun s1 s2 m ->
                 match conv (s1, s2) with
                   Some arg ->
                   begin try
                       let _ = Action.performSyn Ctx.empty (action arg) m in false
                     with Action.InvalidAction -> true
                   end
                 | _ -> true) i_rs_1 i_rs_2 rs)
        ] [pcdata btn_label]) in
      let button_dom = To_dom.of_button button_elt in
      let _ = JSUtil.listen_to Ev.keypress Dom_html.document (fun evt ->
          let evt_key = evt##.keyCode in
          if evt_key = key_code then
            begin
              i_dom_1##focus;
              Dom_html.stopPropagation evt;
              Js._false
            end
          else
            Js._true
        ) in
      let i_keypress_listener i_dom = JSUtil.listen_to Ev.keypress i_dom (fun evt ->
          let evt_key = evt##.keyCode in
          match evt_key with
          | 13 -> begin
              button_dom##click;
              i_dom##blur;
              Js._false
            end
          | 27 -> begin
              i_dom##blur;
              Js._false
            end
          | _ -> Dom_html.stopPropagation evt; Js._true
        ) in
      let _ = i_keypress_listener i_dom_1 in
      let _ = i_keypress_listener i_dom_2 in
      Html5.(div ~a:[a_class ["input-group"]] [
          span ~a:[a_class ["input-group-btn"]] [
            button_elt];
          i_elt_1;
          i_elt_2;
        ]) in

    Html5.(div ~a:[a_class ["row";"marketing"]] [
        div ~a:[a_class ["col-lg-3"; "col-md-3"; "col-sm-3"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Movement"];
            div ~a:[a_class ["panel-body"]] [
              (action_button (Action.Move (Action.Child 1)) "<b>move child 1 </b> (1)" 49);
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
            div ~a:[a_class ["panel-title"]] [pcdata "Type Construction"];
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
            div ~a:[a_class ["panel-title"]] [pcdata "Expression Construction"];
            div ~a:[a_class ["panel-body"]] [
              (action_button (Action.Construct Action.SAsc) "construct asc (:)" 58);
              br ();
              (action_input_button
                 (fun v -> Action.Construct (Action.SVar v))
                 (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                 "construct var (v)" "var_input" 118);
              (action_input_button
                 (fun v -> Action.Construct (Action.SLam v))
                 (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                 "construct lam (\\)" "lam_input" 92);
              (action_button (Action.Construct Action.SAp) "construct ap ( ( )" 40);
              br ();
              (action_input_button
                 (fun n -> Action.Construct (Action.SLit n))
                 (fun s -> try Some (int_of_string s) with Failure _ -> None)
                 "construct lit (#)" "lit_input" 35);
              (action_button (Action.Construct Action.SPlus) "construct plus (+)" 43);
              br ();
              (action_button (Action.Construct (Action.SInj HExp.L)) "construct inj L (l)" 108);
              br ();
              (action_button (Action.Construct (Action.SInj HExp.R)) "construct inj R (r)" 114);
              br ();
              (action_input_input_button
                 (fun (v1,v2) -> Action.Construct (Action.SCase (v1,v2)))
                 (fun (s1, s2) ->
                    let s1_empty = String.compare s1 "" in
                    let s2_empty = String.compare s2 "" in
                    match s1_empty, s2_empty with
                    | 0, _ -> None
                    | _, 0 -> None
                    | _ -> Some (s1, s2))
                 "construct case (c)" "case_input" 99);
            ]
          ]
        ]
      ])
end

(* generates the view *)
module View = struct
  let view ((rs, rf) : Model.rp) =
    (* zexp view *)
    let zexp_view_rs = React.S.map (fun (zexp, _) ->
        [HTMLView.of_zexp zexp]) rs in
    let zexp_view = Html5.(R.Html5.div (ReactiveData.RList.from_signal zexp_view_rs)) in

    Html5.(
      div [ div ~a:[a_class ["jumbotron"]]
              [ div ~a:[a_class ["headerTextAndLogo"]] [
                    div ~a:[a_class ["display-3"]] [pcdata "HZ"];
                    img ~a:[a_id "logo"] ~alt:("Logo") ~src:(Xml.uri_of_string ("imgs/hazel-logo.png")) ()
                  ];
                div ~a:[a_class ["subtext"]] [pcdata "(a reference implementation of Hazelnut)"];
                div ~a:[a_class ["Model"]] [zexp_view]];
            ActionPalette.make_palette (rs, rf)
          ])
end


let doc = Dom_html.document

let main _ =
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "container"))
      (fun () -> assert false)
  in
  let m = Model.empty in
  let rs, rf = React.S.create m in
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view (rs, rf))) ;
  Js._true

let _ = Dom_html.addEventListener
    doc
    Dom_html.Event.domContentLoaded
    (Dom_html.handler main)
    Js._true
