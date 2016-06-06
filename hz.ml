open Hz_controller
open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model

module View = struct

  let rec stringFromHType (htype : HType.t ) : string = match htype with
    | HType.Num -> "num"
    | HType.Arrow (fst,snd) -> "(" ^ stringFromHType (fst) ^ "->" ^ stringFromHType (snd) ^ ")"
    | HType.Hole -> "H" 

  let rec stringFromHExp (hexp : HExp.t ) : string = match hexp with
    | HExp.Asc (hexp,htype) -> (stringFromHExp hexp) ^ ":" ^ (stringFromHType htype)
    | HExp.Var str -> str
    | HExp.Lam (var,exp) -> "λ" ^  var ^ "." ^ (stringFromHExp exp)
    | HExp.Ap (e1, e2) -> (stringFromHExp e1) ^ "(" ^ (stringFromHExp e2) ^ ")"
    | HExp.NumLit num -> string_of_int num
    | HExp.Plus (n1,n2) -> (stringFromHExp n1) ^"+"^ (stringFromHExp n2)
    | HExp.EmptyHole ->  "{}" 
    | HExp.NonEmptyHole hc -> "{" ^ (stringFromHExp hc) ^ "}"

  let rec stringFromZType (ztype : ZType.t ) : string = match ztype with
    | ZType.FocusedT htype -> ">" ^ stringFromHType htype ^ "<"
    | ZType.LeftArrow  (ztype, htype) -> stringFromZType ztype  ^ "->" ^ stringFromHType htype
    | ZType.RightArrow (htype, ztype) -> stringFromHType htype ^ "->" ^ stringFromZType ztype

  let rec stringFromZExp (zexp : ZExp.t ) : string = match zexp with
    | ZExp.FocusedE hexp -> ">" ^ stringFromHExp hexp ^ "<"
    | ZExp.LeftAsc (e, asc) -> (* "LA" ^ *)  stringFromZExp e ^ ":" ^ stringFromHType asc 
    | ZExp.RightAsc (e, asc) -> stringFromHExp e ^ ":" ^ stringFromZType asc
    | ZExp.LamZ (var,exp) -> "λ" ^  var ^ "." ^ (stringFromZExp exp)
    | ZExp.LeftAp (e1,e2) -> stringFromZExp e1 ^ stringFromHExp e2
    | ZExp.RightAp (e1,e2) -> stringFromHExp e1 ^ stringFromZExp e2
    | ZExp.LeftPlus (num1,num2) -> stringFromZExp num1 ^ "+" ^ stringFromHExp num2
    | ZExp.RightPlus (num1,num2) -> stringFromHExp num1  ^ "+" ^ stringFromZExp num2
    | ZExp.NonEmptyHoleZ e -> "{" ^ stringFromZExp e ^ "}"

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
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addNumberButton"|]
          end
    in let _ = try (performSyn (Action.Construct (Action.SLam "lamStr")) mOld) with
        | Action.InvalidAction -> begin
            Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addLambdaButton"|]
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
    Html5.(button ~a:[a_id "delButton"; a_onclick onClickDel] [pcdata "del"] )

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
            button ~a:[a_id "moveLeftChildButton"; a_onclick onClickMoveLC] [pcdata "move left child"] 
          ];
          li [
            button ~a:[a_id "moveParentButton"; a_onclick onClickMoveP] [pcdata "move parent"] 
          ];
          li [
            button ~a:[a_id "moveNextSibButton"; a_onclick onClickMoveNS] [pcdata "move next sib"] 
          ];
          li [
            button ~a:[a_id "movePrevSibButton"; a_onclick onClickMovePS] [pcdata "move prev sib"] 
          ]
        ]
      ]
      )

  let addActions (rs, rf) =
    let addNumberKeyHandler evt =
      (*  begin Js.Unsafe.fun_call (Js.Unsafe.variable "enable") [|Js.Unsafe.inject "delButton"|] end;
          let numValue =  Dom_html.document##getElementById(Js.string "numInput") in 
          let numStr = Js.to_string (numValue##value) in 
          calculateActiveButtons (rs, rf) ;
          let mOld = React.S.value rs in
          let performSyn = Action.performSyn Ctx.empty in 
          let _ = Js.Unsafe.fun_call (Js.Unsafe.variable "enableAll") [|Js.Unsafe.inject "test"|] in 
          let _ = try (performSyn (Action.Construct (Action.SNumLit numStr)) mOld) with
          | Action.InvalidAction -> begin
             Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "delButton"|]
           end in 
          (* Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "addAppButton"|]; *)
          false *)
      (*  let numValue = Tyxml_js.To_dom.of_input (get_el "numInput") in
          numValue##value <- "test" *)

      (* let input = Js.coerce_opt (Dom_html.document##getElementById "numInput")
          Dom_html.CoerceTo.div (fun _ -> assert false) in 

         input##value <-  Js.string "test" *)
      (* let doc = Dom_html.document in
         Js.Opt.case (doc##getElementById(Js.string "numInput"))
         (fun _ -> assert false)
         (fun e ->
           (* e##title <- Js.string "test" *)
           e##title <- Js.to_string  (e##getAttribute "value")
           (* e##title <- Js.some (Js.string "test") *)
           (* e##setAttribute "value" "test";  *)
         ); *)
      (* 
      let inputNumId = get_element_by_id "numInput" in
      let numValue = Tyxml_js.Of_dom.of_input inputNumId in 
      numValue##value <- "test"; *)
      (* let numStr = Js.to_string (numValue##value) in 
         Tyxml_js.To_dom.of_input
         b##onclick <- Dom_html.handler
         (fun _ -> dp_custom##setExtraWeekAtEnd(i##checked); Js._true);
         (*  (fun _ -> 
          callback (e##title <- Js.some (Js.string "test")); _true);   *) *)


      (*  let click = (Dom.document#getElementById "numInput" : Dom.button) in
          click##value <- "test"; *)
      (*       let input = (Dom.document#createElement "input" : Dom.text) in
               let set_input () =
               let i = float_of_string input#_get_value in
               v#set i
               in
               let set_value i =
               input#_set_value (string_of_float i) in (); *)
      (*       let numValue = Tyxml_js.To_dom.of_input (get_el "numInput") in  *)
      let numValue = get_el "numInput" in 
      let numStr = 
        Js.to_string
          (Js.Opt.get  
             (numValue##getAttribute (Js.string "value"))
             (fun () -> Js.string "AAA"))
      in
      Js.Unsafe.fun_call (Js.Unsafe.variable "enable") [|Js.Unsafe.inject numStr|] ;
      (* in  *)

      (*       (Js.Opt.case (e##getAttribute (Js.string "rel"))
                   (fun () -> "") *)
      (*       Controller.update (Action.Construct (Action.SNumLit (int_of_string numStr))) (rs, rf) ;
               calculateActiveButtons (rs, rf) ; *)
      true
    in
    let inputLam  = Html5.(input ~a:[a_class ["c1"]; a_id "lamInput"] ()) in
    let inputVar  = Html5.(input ~a:[a_class ["c1"]; a_id "varInput"] ()) in
    let inputNum  = Html5.(input ~a:[a_class ["c1"]; a_id "numInput"; a_onkeypress addNumberKeyHandler; a_onkeydown addNumberKeyHandler] ()) in
    let onClickAddPlus evt =
      Controller.update (Action.Construct Action.SPlus) (rs, rf) ;
      calculateActiveButtons   (rs, rf) ;
      true
    in
    let onClickAddNumber evt =
      let numValue = Tyxml_js.To_dom.of_input inputNum in 
      let numStr = Js.to_string (numValue##value) in 
      Controller.update (Action.Construct (Action.SNumLit (int_of_string numStr))) (rs, rf) ;
      calculateActiveButtons (rs, rf) ;
      true
    in
    let onClickAddLam evt =
      let lamValue = Tyxml_js.To_dom.of_input inputLam in 
      let lamStr = Js.to_string (lamValue##value) in 
      Controller.update (Action.Construct (Action.SLam (lamStr))) (rs, rf) ;
      calculateActiveButtons  (rs, rf) ;
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
    let onClickAddVar evt =
      let varValue = Tyxml_js.To_dom.of_input inputVar in 
      let varStr = Js.to_string (varValue##value) in 
      Controller.update (Action.Construct (Action.SVar varStr)) (rs, rf) ;
      calculateActiveButtons (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [

        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_id "addPlusButton"; a_onclick onClickAddPlus] [pcdata "Add Plus"] 
          ];
          li [
            button ~a:[a_id "addNumberButton"; a_onclick onClickAddNumber] [pcdata "Add Number"];inputNum 
          ];
          li [
            button ~a:[a_id "addLambdaButton"; a_onclick onClickAddLam] [pcdata "Add Lambda"];inputLam;
          ];
          li [
            button ~a:[a_id "addAscButton"; a_onclick onClickAddAsc] [pcdata "Add Ascription"] 
          ];
          li [
            button ~a:[a_id "addAppButton"; a_onclick onClickAddApp] [pcdata "Add Appliction"] 
          ];      
          li [
            button ~a:[a_id "addVarButton"; a_onclick onClickAddVar] [pcdata "Add Var x"];inputVar;
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
            button ~a:[a_id "addNumButton"; a_onclick onClickAddNum] [pcdata "Add Num"] 
          ];
          li [
            button ~a:[a_id "addArrowButton"; a_onclick onClickAddArrow] [pcdata "Add Arrow"] 
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
          p [
            pcdata "HZ model"
          ] ;
        ] ;
        div ~a:[a_class ["Model"]]  [ model ] ;
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
  (* Dom.appendChild parent input; *)
  Lwt.return ()


let _ = Lwt_js_events.onload () >>= main
