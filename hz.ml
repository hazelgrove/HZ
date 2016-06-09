open Hz_controller
open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model

module View = struct

  let rec stringFromHType (htype : HType.t ) : string = match htype with
    | HType.Num -> "num"
    | HType.Arrow (fst,snd) -> "(" ^ stringFromHType (fst) ^ "->" ^ stringFromHType (snd) ^ ")"
    | HType.Hole -> "(||)"  

  let rec stringFromHExp (hexp : HExp.t ) : string = match hexp with
    | HExp.Asc (hexp,htype) -> (stringFromHExp hexp) ^ " : " ^ (stringFromHType htype)
    | HExp.Var str -> str
    | HExp.Lam (var,exp) -> "λ" ^  var ^ "." ^ (stringFromHExp exp)
    | HExp.Ap (e1, e2) -> (stringFromHExp e1) ^ "(" ^ (stringFromHExp e2) ^ ")"
    | HExp.NumLit num -> string_of_int num
    | HExp.Plus (n1,n2) -> (stringFromHExp n1) ^ " + " ^ (stringFromHExp n2)
    | HExp.EmptyHole ->  "(||)" 
    | HExp.NonEmptyHole hc -> "(|" ^ (stringFromHExp hc) ^ "|)"

  let rec stringFromZType (ztype : ZType.t ) : string = match ztype with
    | ZType.FocusedT htype -> "⊳" ^ stringFromHType htype ^ "⊲"
    | ZType.LeftArrow  (ztype, htype) -> "(" ^ stringFromZType ztype  ^ " -> " ^ stringFromHType htype ^ ")"
    | ZType.RightArrow (htype, ztype) -> "(" ^ stringFromHType htype ^ " -> " ^ stringFromZType ztype ^ ")"

  let rec stringFromZExp (zexp : ZExp.t ) : string = match zexp with
    | ZExp.FocusedE hexp -> "⊳" ^ stringFromHExp hexp ^ "⊲"
    | ZExp.LeftAsc (e, asc) -> (* "LA" ^ *)  stringFromZExp e ^ " : " ^ stringFromHType asc 
    | ZExp.RightAsc (e, asc) -> stringFromHExp e ^ " : " ^ stringFromZType asc
    | ZExp.LamZ (var,exp) -> "λ" ^  var ^ "." ^ (stringFromZExp exp)
    | ZExp.LeftAp (e1,e2) -> stringFromZExp e1 ^ "(" ^ stringFromHExp e2 ^ ")"
    | ZExp.RightAp (e1,e2) -> stringFromHExp e1 ^ "(" ^ stringFromZExp e2 ^ ")"
    | ZExp.LeftPlus (num1,num2) -> stringFromZExp num1 ^ " + " ^ stringFromHExp num2
    | ZExp.RightPlus (num1,num2) -> stringFromHExp num1  ^ " + " ^ stringFromZExp num2
    | ZExp.NonEmptyHoleZ e -> "(|" ^ stringFromZExp e ^ "|)"

  let viewSignal (rs, rf) = (React.S.map (fun ((zexp,htype) : Model.t) -> stringFromZExp zexp) rs)

  let calculateActiveButtons (rs, rf) = 
    let mOld = React.S.value rs in
    let performSyn = Action.performSyn Ctx.empty in 
    let _ = Js.Unsafe.fun_call (Js.Unsafe.variable "enableAll") [|Js.Unsafe.inject "test"|] in 
    let _ = try (performSyn Action.Del mOld) with
      | Action.InvalidAction -> begin
          Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "delButton"|]
        end
    in let _ = try (performSyn (Action.Move Action.FirstChild) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "moveLeftChildButton"|]
          end
    in let _ = try (performSyn (Action.Move Action.Parent) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "moveParentButton"|]
          end
    in let _ = try (performSyn (Action.Move Action.NextSib) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "moveNextSibButton"|]
          end
    in let _ = try (performSyn (Action.Move Action.PrevSib) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "movePrevSibButton"|]
          end
    in let _ = try (performSyn (Action.Construct Action.SPlus) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addPlusButton"|]
          end
        (* TODO: validate numbers, variables when text box changes *)
    in let _ = try (performSyn (Action.Construct (Action.SNumLit 1)) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "num_input_id"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SLam "lamStr")) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "lam_input_id"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SAsc)) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addAscButton"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SAp)) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addAppButton"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SVar "varStr")) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addVarButton"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SNum)) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addNumButton"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SArrow)) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addArrowButton"|]
          end         
    in () 



  let get_value dom = Js.to_string ((Tyxml_js.To_dom.of_input dom)##value)
  let set_value e s = (Tyxml_js.To_dom.of_input e)##value <- Js.string s


  let alert str = Dom_html.window##alert(Js.string str)

  type handler = (Dom_html.element Js.t, Dom_html.event Js.t) Dom_html.event_listener


  (* let get_element_by_id id =
     Js.Opt.get (Dom_html.document##getElementById (Js.string id) : Dom.input)
      (fun _ -> assert false)
  *)
  let get_el s = Js.Opt.get (Dom_html.document##getElementById(Js.string s))
      (fun _ -> assert false)

  let get x = Js.Opt.get x (fun _ -> assert false)

  let get_input s = 
    get (Dom_html.CoerceTo.input (get_el s))


  let viewModel (rs, rf) =
    R.Html5.(pcdata (viewSignal (rs,rf))) 

  let viewActions (rs, rf) =
    let onClickDel evt =
      Controller.update (Action.Del) (rs, rf); 
      calculateActiveButtons (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [
        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "delButton"; a_onclick onClickDel] [pcdata "del"]
          ]
        ]
      ]
      )






  let moveActions (rs, rf) =
    let onClickMoveLC evt =
      Controller.update (Action.Move Action.FirstChild) (rs, rf) ;
      calculateActiveButtons  (rs, rf) ;
      true
    in
    let onClickMoveP evt =
      Controller.update (Action.Move Action.Parent) (rs, rf) ;
      calculateActiveButtons (rs, rf) ;
      true
    in
    let onClickMoveNS evt =
      Controller.update (Action.Move Action.NextSib) (rs, rf) ;
      calculateActiveButtons  (rs, rf) ;
      true
    in
    let onClickMovePS evt =
      Controller.update (Action.Move Action.PrevSib) (rs, rf) ;
      calculateActiveButtons (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [
        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "moveLeftChildButton"; a_onclick onClickMoveLC] [pcdata "move left child"] 
          ];
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "moveParentButton"; a_onclick onClickMoveP] [pcdata "move parent"] 
          ];
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "moveNextSibButton"; a_onclick onClickMoveNS] [pcdata "move next sib"] 
          ];
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "movePrevSibButton"; a_onclick onClickMovePS] [pcdata "move prev sib"] 
          ]
        ]
      ]
      )

  let addActions (rs, rf) =
    let var_key_handler evt =
      if evt##keyCode = 13 then (
        let tgt = Dom_html.CoerceTo.input(Dom.eventTarget evt) in
        Js.Opt.case tgt
          (fun () -> (alert ("ERROR")) )
          (fun e -> 
             (*     alert (Js.to_string e##value); *)
             Controller.update (Action.Construct (Action.SVar ((Js.to_string e##value)))) (rs, rf) ;
             calculateActiveButtons (rs, rf)
          ) ;
        false
      ) else 
        let mOld = React.S.value rs in
        let performSyn = Action.performSyn Ctx.empty in 
        let tgt = Dom_html.CoerceTo.input(Dom.eventTarget evt) in
        Js.Opt.case tgt
          (fun () -> (alert ("ERROR")) )
          (fun e -> let _ = begin
               let currStr = (Js.to_string e##value)^(Char.escaped(Char.chr (evt##keyCode))) in 
               Js.Unsafe.fun_call (Js.Unsafe.variable "goodValue") [|Js.Unsafe.inject ("var_input_id")|];
               try (performSyn 
                      (Action.Construct 
                         (Action.SVar currStr)) 
                      mOld) with
               | Action.InvalidAction -> 
                 begin
                   (* alert (currStr); *)
                   Js.Unsafe.fun_call (Js.Unsafe.variable "badValue") [|Js.Unsafe.inject ("var_input_id")|]
                 end
             end in ()
          ) ; 
        true
    in
    let varInput = Html5.(input ~a:[
        a_class ["form-control"];
        a_id "var_input_id" ;
        a_input_type `Text ;
        a_value "" ;
        a_onkeypress var_key_handler ; 
        (* a_onkeydown var_key_handler ;  *)
      ] ()) in 
    let num_key_handler evt =
      if evt##keyCode = 13 then (
        let tgt = Dom_html.CoerceTo.input(Dom.eventTarget evt) in
        Js.Opt.case tgt
          (fun () -> ())
          (fun e -> 
             (*     alert (Js.to_string e##value); *)
             Controller.update (Action.Construct (Action.SNumLit (int_of_string (Js.to_string e##value)))) (rs, rf) ;
             calculateActiveButtons (rs, rf) ;
          ) ;
        false
      ) else true
    in
    let numInput = Html5.(input ~a:[
        a_class ["form-control"];
        a_id "num_input_id" ;
        a_input_type `Number ;
        a_value "" ;
        a_onkeypress num_key_handler ;
        a_onkeydown num_key_handler ;
      ] ()) in 
    let lam_key_handler evt =
      if evt##keyCode = 13 then (
        let tgt = Dom_html.CoerceTo.input(Dom.eventTarget evt) in
        Js.Opt.case tgt
          (fun () -> ())
          (fun e -> 
             (*     alert (Js.to_string e##value); *)
             Controller.update (Action.Construct (Action.SLam ((Js.to_string e##value)))) (rs, rf) ;
             calculateActiveButtons (rs, rf) ;
          ) ;
        false
      ) else true
    in
    let lamInput = Html5.(input ~a:[
        a_class ["form-control"]; 
        a_id "lam_input_id" ;
        a_input_type `Text ;
        a_value "" ;
        a_onkeypress lam_key_handler ;
        a_onkeydown lam_key_handler ;
      ] ()) in 
    let onClickAddPlus evt =
      Controller.update (Action.Construct Action.SPlus) (rs, rf) ;
      calculateActiveButtons   (rs, rf) ;
      true
    in
    let onClickAddAsc evt =
      Controller.update (Action.Construct (Action.SAsc)) (rs, rf) ;
      calculateActiveButtons  (rs, rf) ;
      true
    in
    let onClickAddApp evt =
      Controller.update (Action.Construct (Action.SAp)) (rs, rf) ;
      calculateActiveButtons  (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [

        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "addPlusButton"; a_onclick onClickAddPlus] [pcdata "Construct Plus"] ; 
          ];
          li [
            div ~a:[a_id "numLabel"] [ pcdata "Construct Number:" ] ;  numInput
          ];
          li [
            div ~a:[a_id "lamLabel"] [ pcdata "Construct Lambda:" ] ;lamInput;
          ];
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "addAscButton"; a_onclick onClickAddAsc] [pcdata "Construct Ascription"] 
          ];
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "addAppButton"; a_onclick onClickAddApp] [pcdata "Construct Application"] 
          ];      
          li [
            div ~a:[a_id "lamLabel"] [ pcdata "Construct Var:" ] ; varInput;
          ];
        ]
      ]
      )



  let addTypes (rs, rf) =
    let onClickAddNum evt =
      Controller.update (Action.Construct Action.SNum) (rs, rf) ;
      calculateActiveButtons (rs, rf) ;
      true
    in
    let onClickAddArrow evt =
      Controller.update (Action.Construct Action.SArrow) (rs, rf) ;
      calculateActiveButtons (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [
        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "addNumButton"; a_onclick onClickAddNum] [pcdata "Construct Num"] 
          ];
          li [
            button ~a:[a_class ["btn btn-primary"]; a_id "addArrowButton"; a_onclick onClickAddArrow] [pcdata "Construct Arrow"] 
          ];
        ]
      ]
      )



  let view (rs, rf) =
    let model = viewModel (rs, rf) in 
    let actions = viewActions (rs, rf) in 
    let mActions = moveActions (rs, rf) in
    let aActions = addActions (rs, rf) in
    let aTypes = addTypes (rs, rf) in
    calculateActiveButtons (rs, rf);
    Html5.(
      div [
        div ~a:[a_class ["comments"]] [
          (* p [
             pcdata "HZ model"
             ] ; *)
        ] ;
        div ~a:[a_class ["container"]] [
          div ~a:[a_class ["Model"]]  [ model ] ;
        ];

        div ~a:[a_class ["Actions"]]  [ actions ];
        div ~a:[a_class ["ActionsLeftPlus"]]  [ mActions ];
        div ~a:[a_class ["ActionsAdd"]]  [ aActions ];
        div ~a:[a_class ["ActionsTypes"]]  [ aTypes ];
      ]
    ) 
end 

open Lwt.Infix


let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "container"))
      (fun () -> assert false)
  in
  let m = Model.empty in
  let rp = React.S.create m in
  (* let input = Dom_html.createInput ~name: (Js.string "inputTextBox") ~_type:(Js.string "text") doc in *)
  (*   let textbox = Dom_html.createTextarea Dom_html.document in
       Dom.appendChild parent textbox; *)
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view rp)) ;
  (* Dom.appendChild parent input;*)
  View.calculateActiveButtons (rp);
  Lwt.return ()


let _ = Lwt_js_events.onload () >>= main
