open Hz_model
open Hz_controller
open Tyxml_js
open Hz_model.Action
open Hz_model.Model
open Hz_model.Model.ZExp
open Hz_model.Model.ZType
open Hz_model.Model.HExp
open Hz_model.Model.HType

module View = struct

  let rec stringFromHType (htype : Hz_model.Model.HType.t ) : string = match htype with
    | Num -> "num"
    | Arrow (fst,snd) -> "(" ^ stringFromHType (fst) ^ "->" ^ stringFromHType (snd) ^ ")"
    | Hole -> "H" 

  let rec stringFromHExp (hexp : Model.HExp.t ) : string = match hexp with
    | Asc (hexp,htype) -> (stringFromHExp hexp) ^ ":" ^ (stringFromHType htype)
    | Var str -> str
    | Lam (var,exp) -> "λ" ^  var ^ "." ^ (stringFromHExp exp)
    | Ap (e1, e2) -> (stringFromHExp e1) ^ "(" ^ (stringFromHExp e2) ^ ")"
    | NumLit num -> string_of_int num
    | Plus (n1,n2) -> (stringFromHExp n1) ^"+"^ (stringFromHExp n2)
    | EmptyHole ->  "{}" 
    | NonEmptyHole hc -> "{" ^ (stringFromHExp hc) ^ "}"

  let rec stringFromZType (ztype : Model.ZType.t ) : string = match ztype with
    | FocusedT htype -> ">" ^ stringFromHType htype ^ "<"
    | FirstArrow  (ztype, htype) -> stringFromZType ztype  ^ "->" ^ stringFromHType htype
    | SecondArrow (htype, ztype) -> stringFromHType htype ^ "->" ^ stringFromZType ztype

  let rec stringFromZExp (zexp : Model.ZExp.t ) : string = match zexp with
    | FocusedE hexp -> ">" ^ stringFromHExp hexp ^ "<"
    | LeftAsc (e, asc) -> (* "LA" ^ *)  stringFromZExp e ^ ":" ^ stringFromHType asc 
    | RightAsc (e, asc) -> stringFromHExp e ^ ":" ^ stringFromZType asc
    | LamZ (var,exp) -> "λ" ^  var ^ "." ^ (stringFromZExp exp)
    | LeftAp (e1,e2) -> stringFromZExp e1 ^ stringFromHExp e2
    | RightAp (e1,e2) -> stringFromHExp e1 ^ stringFromZExp e2
    | LeftPlus (num1,num2) -> stringFromZExp num1 ^ "+" ^ stringFromHExp num2
    | RightPlus (num1,num2) -> stringFromHExp num1  ^ "+" ^ stringFromZExp num2
    | NonEmptyHoleZ e -> "{" ^ stringFromZExp e ^ "}"

  let viewSignal (rs, rf) = (React.S.map (fun ((zexp,htype) :model) -> stringFromZExp zexp) rs)

  let calculateActiveButtons a (rs, rf) =
    let mOld = React.S.value rs in
    (* let m = (Action.performSyn mOld a) in *)
    (* begin
       (*       let elements =
               Dom_html.window##document##getElementsByTagName(Js.string "inputTextBox")
               in
               Js.Opt.get elements##item(0)
               (fun () -> failwith ("find_tag("^name^")")); *)
       begin
        let tbox =
          Js.Opt.get (Dom_html.document##getElementById(Js.string "inputTextBox"))
            (fun () -> assert false) in
        tbox##value := Js.string "test";

       end
    *)
    begin
      Js.Unsafe.fun_call (Js.Unsafe.variable "enableAll") [|Js.Unsafe.inject "test"|]
    end;
    begin
      try (Action.performSyn mOld (Action.Del)) with
      | NotImplemented -> begin
          Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "delButton"|]
        end
    end;
    begin
      try (Action.performSyn mOld (Action.Move FirstChild)) with
      | NotImplemented -> begin
          Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "moveLeftChildButton"|]
        end
    end;
    begin
      try (Action.performSyn mOld (Action.Move Parent)) with
      | NotImplemented -> begin
          Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "moveParentButton"|]
        end
    end


  let decodeURI () =
    let varname = 
      begin Js.Unsafe.fun_call (Js.Unsafe.variable "disable") [|Js.Unsafe.inject "moveParentButton"|]  
      end

    in

    Js.string varname

  let viewModel (rs, rf) =
    R.Html5.(pcdata (viewSignal (rs,rf))) 

  let viewActions (rs, rf) =
    let onClickDel evt =
      Controller.update (Action.Del) (rs, rf); 
      calculateActiveButtons (Action.Del) (rs, rf);
      true
    in
    Html5.(button ~a:[a_id "delButton"; a_onclick onClickDel] [pcdata "del"] )

  let moveActions (rs, rf) =
    let onClickMoveLC evt =
      Controller.update (Action.Move FirstChild) (rs, rf) ;
      calculateActiveButtons (Action.Move FirstChild) (rs, rf) ;
      true
    in
    let onClickMoveP evt =
      Controller.update (Action.Move Parent) (rs, rf) ;
      begin
        Js.Unsafe.fun_call (Js.Unsafe.variable "testFun") [|Js.Unsafe.inject "test"|]
      end
        true
    in
    let onClickMoveNS evt =
      Controller.update (Action.Move NextSib) (rs, rf) ;
      true
    in
    let onClickMovePS evt =
      Controller.update (Action.Move PrevSib) (rs, rf) ;
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
    (* let doc = Dom_html.document in
       let con =
       Js.Opt.get (doc##getElementById(Js.string "containerDiv"))
        (fun () -> assert false)
       in
       let input = Dom_html.createInput ~name: (Js.string "inputTextBox") ~_type:(Js.string "text") doc in
       Dom.appendChild con input;  *)
    let inputVar  = Html5.(input ~a:[a_class ["c1"]; a_id "id-of-input"]) () in
    inputVar##value <- (Js.string "inputTextBox") ;

    let onClickAddPlus evt =
      Controller.update (Action.Construct SPlus) (rs, rf) ;
      true
    in
    let onClickAddNumber evt =
      Controller.update (Action.Construct (SNumlit 1)) (rs, rf) ;
      true
    in
    let onClickAddLam evt =
      (*       let tbox =
               Js.Opt.get (Dom_html.document##getElementById(Js.string "inputTextBox"))
                (fun () -> assert false) in *)
      (* let text = Js.to_string (input##value) in  *)
      (* let text = Js.to_string (textbox##value) in *)
      (*       let question =
               Js.string "Do you find this tutorial useful ?" in
               let result = Dom_html.window##(prompt question) in
      *)
      (* 
      let res = Dom_html.window##alert(Js.string "title", Js.string "default") in *)
      (*       let question =
               Js.string "Do you find this tutorial useful ?" in
               let result = Dom_html.window##(prompt question) in *)

      (* let varName = decodeURI in *) 
      (*      let varname = 
              begin 
               (* Js.Unsafe.fun_call (Js.Unsafe.variable "lambdaName") [|Js.Unsafe.inject "moveParentButton"|]   *)
               Js.Unsafe.call (Js.Unsafe.variable "lambdaName") [|Js.Unsafe.inject "moveParentButton"|]
              end
              in *)
      (*       let tbox =
               Js.Opt.get (Dom_html.document##getElementById(Js.string "textId"))
                (fun () -> assert false) in
               let text = Js.to_string (tbox##innerHTML) in *)
      (* let varName = Dom_html.document##getElementById(Js.string "textId") in
         let value =  Js.Opt.get varName##value (fun () -> assert false) in *)

      (*       let varValue = Tyxml_js.To_dom.of_input inputVar in 
               let varStr =  Js.to_string (varValue##value()) in  *)
      Controller.update (Action.Construct (SLam ("varStr"))) (rs, rf) ;
      true
    in
    let onClickAddAsc evt =
      Controller.update (Action.Construct (SAsc)) (rs, rf) ;
      true
    in
    let onClickAddApp evt =
      Controller.update (Action.Construct (SAp)) (rs, rf) ;
      true
    in
    let onClickAddVar evt =
      Controller.update (Action.Construct (SVar "x")) (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [
        inputVar;
        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_id "addPlusButton"; a_onclick onClickAddPlus] [pcdata "Add Plus"] 
          ];
          li [
            button ~a:[a_id "addNumberButton"; a_onclick onClickAddNumber] [pcdata "Add Number"] 
          ];
          li [
            button ~a:[a_id "addLambdaButton"; a_onclick onClickAddLam] [pcdata "Add Lambda"];
          ];
          li [
            button ~a:[a_id "addAscButton"; a_onclick onClickAddAsc] [pcdata "Add Ascription"] 
          ];
          li [
            button ~a:[a_id "addAppButton"; a_onclick onClickAddApp] [pcdata "Add Appliction"] 
          ];      
          li [
            button ~a:[a_id "addVarButton"; a_onclick onClickAddVar] [pcdata "Add Var x"] 
          ];
        ]
      ]
      )


  let addTypes (rs, rf) =
    let onClickAddNum evt =
      Controller.update (Action.Construct SNum) (rs, rf) ;
      true
    in
    let onClickAddArrow evt =
      Controller.update (Action.Construct SArrow) (rs, rf) ;
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
  let input = Dom_html.createInput ~name: (Js.string "inputTextBox") ~_type:(Js.string "text") doc in
  (*   let textbox = Dom_html.createTextarea Dom_html.document in
       Dom.appendChild parent textbox; *)
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view rp)) ;
  Dom.appendChild parent input;
  Lwt.return ()


let _ = Lwt_js_events.onload () >>= main
