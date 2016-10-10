open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model
open Hz_view

open React

module Ev = Dom_html.Event

module Util = struct
  exception No_value
  let opt_get opt = match opt with Some x -> x | _ -> raise No_value
end

module JSUtil = struct
  let getElementByIdForce id = 
    let doc = Dom_html.document in  
    Js.Opt.get (doc##getElementById(Js.string id))
      (fun () -> assert false)

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
  let r_input id placeholder_str=
    let rs, rf = S.create "" in
    let i_elt = Html5.(input ~a:[a_id id; a_class ["form-control"]; a_placeholder placeholder_str]  ();  ) in
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

    let keycode_1 = 49 in
    let keycode_2 = 50 in
    let keycode_enter = 13 in
    let keycode_esc = 27 in
    let keycode_p = 112 in
    let keycode_x = 120 in
    let keycode_greaterThan = 62 in
    let keycode_n = 110 in
    let keycode_s = 115 in
    let keycode_dot = 46 in
    let keycode_colon = 58 in
    let keycode_v = 118 in
    let keycode_backslash = 92 in
    let keycode_openParens = 40 in
    let keycode_hashtag = 35 in
    let keycode_plus = 43 in
    let keycode_l = 108 in
    let keycode_r = 114 in
    let keycode_c = 99 in

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
     * goes from a string (the input value) to an arg option where arg is
     * the action argument. *)
    let action_input_button action conv btn_label input_id key_code placeholder_str=
      (* create reactive input box *)
      let (i_rs, i_rf), i_elt, i_dom = JSUtil.r_input input_id placeholder_str in
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

    (* actions that take two inputs. the conversion function
     * goes from a pair of strings to an arg option where arg is
     * the action argument. *)
    let action_input_input_button action conv btn_label input_id key_code placeholder_str=
      let input_id_1 = (input_id ^ "_1") in
      let input_id_2 = (input_id ^ "_2") in
      let (i_rs_1, i_rf_1), i_elt_1, i_dom_1 = JSUtil.r_input input_id_1 placeholder_str in
      let (i_rs_2, i_rf_2), i_elt_2, i_dom_2 = JSUtil.r_input input_id_2 placeholder_str in
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
              (action_button (Action.Move (Action.Child 1)) "move child 1 [1]" 49);
              br ();
              (action_button (Action.Move (Action.Child 2)) "move child 2 [2]" 50);
              br ();
              (action_button (Action.Move (Action.Child 3)) "move child 3 [3]" 51);
              br ();
              (action_button (Action.Move (Action.Parent)) "move parent [p]" 112);
            ]
          ];
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Deletion"];
            div ~a:[a_class ["panel-body"]] [
              (action_button (Action.Del) "del [x]" 120);
            ]
          ]
        ];
        div ~a:[a_class ["col-lg-3"; "col-md-3"; "col-sm-3"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Type Construction"];
            div ~a:[a_class ["panel-body"]] [
              (action_button (Action.Construct Action.SArrow) "construct arrow [>]" 62);
              br ();
              (action_button (Action.Construct Action.SNum) "construct num [n]" 110);
              br ();
              (action_button (Action.Construct Action.SSum) "construct sum [s]" 115);
              br ();  ]
          ];
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Finishing"];
            div ~a:[a_class ["panel-body"]] [
              (action_button (Action.Finish) "finish [.]" 46)
            ]
          ]
        ]
        ;
        div ~a:[a_class ["col-lg-6"; "col-md-6"; "col-sm-6"]] [
          div ~a:[a_class ["panel";"panel-default"]] [
            div ~a:[a_class ["panel-title"]] [pcdata "Expression Construction"];
            div ~a:[a_class ["panel-body"]] [
              (action_button (Action.Construct Action.SAsc) "construct asc [:]" 58);
              br ();
              (action_input_button
                 (fun v -> Action.Construct (Action.SVar v))
                 (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                 "construct var [v]" "var_input" 118 "Enter var + press Enter");
              (action_input_button
                 (fun v -> Action.Construct (Action.SLam v))
                 (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                 "construct lam [\\]" "lam_input" 92 "Enter var + press Enter");
              (action_button (Action.Construct Action.SAp) "construct ap [(]" 40);
              br ();
              (action_input_button
                 (fun n -> Action.Construct (Action.SLit n))
                 (fun s ->
                    try
                      let i = int_of_string s in
                      if i < 0 then None else Some i
                    with Failure _ -> None)
                 "construct lit [#]" "lit_input" 35 "Enter num + press Enter");
              (action_button (Action.Construct Action.SPlus) "construct plus [+]" 43);
              br ();
              (action_button (Action.Construct (Action.SInj HExp.L)) "construct inj L [l]" 108);
              br ();
              (action_button (Action.Construct (Action.SInj HExp.R)) "construct inj R [r]" 114);
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
                 "construct case [c]" "case_input" 99 "Enter var + press Enter");
            ]
          ]
        ]
      ])
end

(* generates the view *)
module AppView = struct
  let view ((rs, rf) : Model.rp) =
    (* zexp view *)
    let zexp_view_rs = React.S.map (fun (zexp, _) ->
        [HTMLView.of_zexp zexp]) rs in
    let zexp_view = Html5.(R.Html5.div (ReactiveData.RList.from_signal zexp_view_rs)) in

    Tyxml_js.To_dom.of_div Html5.(
      div [ div ~a:[a_class ["jumbotron"]]
              [ div ~a:[a_class ["headerTextAndLogo"]] [
                    div ~a:[a_class ["display-3"]] [pcdata "HZ"];
                    img ~a:[a_id "logo"] ~alt:("Logo") ~src:(Xml.uri_of_string ("imgs/hazel-logo.png")) ()
                  ];
                div ~a:[a_class ["subtext"]] [
                  pcdata "(a reference implementation of "; 
                  a ~a:[a_href "https://arxiv.org/abs/1607.04180"] [pcdata "Hazelnut"]; pcdata ")"];
                div ~a:[a_class ["Model"]] [zexp_view]];
            ActionPalette.make_palette (rs, rf)
          ])
end

(* execution starts here *)
let _ = JSUtil.listen_to_t 
    Dom_html.Event.domContentLoaded
    Dom_html.document
    (fun _ -> 
      let rs, rf = React.S.create (Model.empty) in
      let parent = JSUtil.getElementByIdForce "container" in 
      Dom.appendChild parent (AppView.view (rs, rf)))

