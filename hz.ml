open Lwt.Infix
exception NotImplemented
exception InProgress

module Model = struct

  module HType = struct 
    type t = 
        Num
      | Arrow of t * t 
      | Hole 
  end

  module HExp = struct
    type t = 
      | Asc of t * HType.t 
      | Var of string
      | Lam of string * t
      | Ap of t * t 
      | NumLit of int
      | Plus of t * t
      | EmptyHole 
      | NonEmptyHole of t

    (* let rec performSyn hexp : HType.t = 
       raise NotImplemented

       and performAna hexp htype: boolean =
       raise NotImplemented  *)
    (* Raise Expection if not well typed *)
  end

  module ZType = struct
    type t = 
      | FocusedT of HType.t
      | FirstArrow of t * HType.t
      | SecondArrow of HType.t * t 
  end

  module ZExp = struct
    type t = 
      | FocusedE of HExp.t
      | LeftAsc of t * HType.t
      | RightAsc of HExp.t * ZType.t
      | LamZ of string * t
      | LeftAp of t * HExp.t
      | RightAp of HExp.t * t 
      | LeftPlus of t * HExp.t
      | RightPlus of HExp.t * t
      | NonEmptyHoleZ of t
  end


  open HExp
  open ZExp
  open HType
  (* let empty = (HType.Arrow ((HType.Hole),(HType.Arrow ((HType.Num 1),(HType.Num 2)))))    *)
  (* let empty = Lam ((Var "x"),InProgressHole (Plus (NumLit 1, NumLit 3))) *)
  (* let empty = (FocusedE (Plus (NumLit 1, NumLit 3))),(Num) *)
  (* let empty = (ZExp.LeftPlus ((ZExp.FocusedE (NumLit 8)), (NumLit 7))), HType.Num *)
  let empty = (FocusedE EmptyHole),(Num)
end

type model = Model.ZExp.t * Model.HType.t
type rs = model React.signal
type rf = ?step:React.step -> model -> unit
type rp = rs * rf

module Action = struct
  open Model
  type direction =  
      FirstChild 
    | Parent 
    | NextSib 
    | PrevSib

  type shape = 
    | SArrow
    | SNum
    | SAsc
    | SVar of string
    | SLam of string
    | SAp 
    | SArg
    | SNumlit of int
    | SPlus

  type t =
      Move of direction
    | Del 
    | Construct of shape
    | Finish


  let rec performTyp (ztype,a) : Model.ZType.t =
    let m = match a with 
      | Del -> begin
          match ztype with
          | ZType.FocusedT _ -> ZType.FocusedT Hole
          | _ -> (performTyp (ztype,a))
        end
      | Move dir -> begin
          match dir with 
          | FirstChild -> begin
              match ztype with 
              | ZType.FocusedT t1 -> begin
                  match t1 with
                  | HType.Arrow (t1,t2) ->  ZType.FirstArrow (( ZType.FocusedT t1),t2)
                end
              | ZType.FirstArrow (t1,t2) -> ZType.FocusedT (Hole)
              | ZType.SecondArrow _ -> ZType.FocusedT (Hole)
            end
          | NextSib -> begin
              match ztype with 
              | ZType.FirstArrow (t1,t2) -> begin
                  match t1 with 
                  | ZType.FocusedT f1 -> ZType.SecondArrow (f1,(ZType.FocusedT(t2)))
                  | _ -> ZType.FirstArrow ((performTyp (t1,a)),t2)
                end
            end
          | Parent -> begin
              match ztype with 
              | ZType.FirstArrow (z1,t1) -> begin
                  match z1 with 
                  | ZType.FocusedT f1 -> ZType.FocusedT (HType.Arrow (f1,t1))
                  | _  -> ZType.FirstArrow ((performTyp (z1,a)),t1)
                end
              | ZType.SecondArrow (t1,z1) -> begin
                  match z1 with 
                  | ZType.FocusedT f1 -> ZType.FocusedT (HType.Arrow (t1,f1))
                  | _  -> ZType.SecondArrow (t1,(performTyp (z1,a)))
                end
            end 
        end
      | Construct shape -> begin
          match ztype with
          | ZType.FocusedT t1 -> begin
              match t1 with
              | HType.Hole -> begin
                  match shape with 
                  | SNum -> ZType.FocusedT (Num)
                end
              | HType.Arrow (t1,t2) -> ZType.FirstArrow (( ZType.FocusedT t1),t2)
              | _ -> raise InProgress
              (* (fst (performTyp ((ZType.FocusedT t1),a))) ,t2) *)
            end
          | ZType.FirstArrow (z1,t1) -> ZType.FirstArrow ((performTyp (z1,a)),t1)
          | ZType.SecondArrow (t1,z1) -> ZType.SecondArrow (t1,(performTyp (z1,a)))
        end        
      | _ -> raise NotImplemented
    in m

  let rec performSyn ((zexp,htype): model) a : ZExp.t * HType.t =
    let m = match a with 
      | Del -> begin
          match zexp with
          | ZExp.FocusedE _ -> ZExp.FocusedE Model.HExp.EmptyHole
          | ZExp.LeftPlus (z1,h1) -> ZExp.LeftPlus (fst (performSyn (z1,htype) a),h1)
          | ZExp.RightPlus (h1,z1) -> ZExp.RightPlus (h1,fst (performSyn (z1,htype) a))
          | ZExp.LeftAsc (z1,a1) -> ZExp.LeftAsc (fst (performSyn (z1,htype) a),a1)
          | ZExp.RightAsc (a1,z1) -> ZExp.RightAsc (a1, (performTyp (z1,a)) )  (* fst (performTyp (z1,htype) a) *)
          | ZExp.LamZ (var,z1) -> ZExp.LamZ (var,fst (performSyn (z1,htype) a))
          | _ -> raise NotImplemented
        end
      | Move dir -> begin
          match dir with 
          | FirstChild -> begin
              match zexp with 
              | ZExp.FocusedE hexp -> begin
                  match hexp with
                  | HExp.Plus (h1,h2) -> ZExp.LeftPlus ((ZExp.FocusedE h1),h2)
                  | HExp.Lam (var,body) -> ZExp.LamZ (var,(ZExp.FocusedE body))
                  | HExp.Asc (a1,a2) -> ZExp.LeftAsc ((ZExp.FocusedE a1),a2)
                end
              | ZExp.LeftPlus (z1,h1) -> ZExp.LeftPlus (fst (performSyn (z1,htype) a),h1)
              | ZExp.RightPlus (h1,z1) -> ZExp.RightPlus (h1,fst (performSyn (z1,htype) a))
              | ZExp.LeftAsc (z1,a1) -> ZExp.LeftAsc (fst (performSyn (z1,htype) a),a1)
              | ZExp.RightAsc (a1,z1) -> ZExp.RightAsc (a1, (performTyp (z1,a)) )  (* fst (performTyp (z1,htype) a) *)
              | ZExp.LamZ (var,z1) -> ZExp.LamZ (var,fst (performSyn (z1,htype) a))
            end
          | Parent -> begin
              match zexp with 
              | ZExp.LeftPlus (z1,h1) -> begin
                  match z1 with 
                  | ZExp.FocusedE hexp -> ZExp.FocusedE (Plus (hexp,h1))
                  | _ -> ZExp.LeftPlus((fst (performSyn (z1,htype) a)),h1)
                end
              | ZExp.RightPlus (h1,z1) -> begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.FocusedE (Plus (h1,hexp))
                  | _ -> ZExp.RightPlus(h1,(fst (performSyn (z1,htype) a)))
                end
              | ZExp.LeftAsc (z1,a1) -> begin
                  match z1 with 
                  | ZExp.FocusedE hexp -> ZExp.FocusedE (Asc (hexp,a1))
                  | _ -> ZExp.LeftAsc((fst (performSyn (z1,htype) a)),a1)
                end
              | ZExp.RightAsc (a1,z1) -> begin
                  match z1 with
                  | ZType.FocusedT htype -> ZExp.FocusedE (Asc (a1,htype))
                  | _ -> ZExp.RightAsc(a1,(performTyp (z1,a)))
                end
              | ZExp.LamZ (var,z1) -> begin
                  match z1 with
                  | ZExp.FocusedE htype -> ZExp.FocusedE (Lam (var,htype))
                  | _ -> ZExp.LamZ (var,(fst (performSyn (z1,htype) a)))
                end 
              | _ -> raise NotImplemented
            end
          | NextSib -> begin
              match zexp with
              | ZExp.LeftPlus (z1,h1) -> begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.RightPlus (hexp, (ZExp.FocusedE h1))
                  | _ -> ZExp.LeftPlus((fst (performSyn (z1,htype) a)),h1)
                end
              | ZExp.RightPlus (h1,z1) -> begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.LeftPlus ((ZExp.FocusedE h1),hexp)
                  | _ -> ZExp.RightPlus(h1,(fst (performSyn (z1,htype) a)))
                end
              | ZExp.LeftAsc (z1,t1) ->   begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.RightAsc (hexp, (ZType.FocusedT t1))
                  | _ -> ZExp.LeftAsc((fst (performSyn (z1,htype) a)),t1)
                end 
              | ZExp.RightAsc (a1,z1) ->   begin
                  match z1 with
                  (* | ZExp.FocusedE hexp -> ZExp.RightAsc (hexp, (ZType.FocusedT t1)) *)
                  | _ -> ZExp.RightAsc(a1,(performTyp (z1,a)))
                end  
            end
          | PrevSib -> begin
              match zexp with 
              | ZExp.RightAsc (a1,z1) -> begin
                  match z1 with
                  | ZType.FocusedT f1 -> ZExp.LeftAsc (ZExp.FocusedE (a1),f1)
                end
            end 
        end
      | Construct shape -> begin
          match zexp with
          | ZExp.FocusedE hexp -> begin
              match shape with 
              | SPlus -> (ZExp.FocusedE (HExp.Plus (HExp.EmptyHole,(HExp.EmptyHole)))) 
              | SNumlit i -> (ZExp.FocusedE (HExp.NumLit i))
              | SLam var -> (ZExp.FocusedE  (HExp.Asc ((HExp.Lam ("x",HExp.EmptyHole)),HType.Arrow (HType.Hole,HType.Hole)))) (* (HExp.Asc (HExp.Lam ("x",HExp.EmptyHole)), HType.Arrow (HType.Hole, HType.Hole))) *)
              | SVar v -> (ZExp.FocusedE (HExp.Var v))
              | SAsc -> (ZExp.LeftAsc ((ZExp.FocusedE HExp.EmptyHole),(HType.Hole))) 
              | _ -> raise NotImplemented 
            end
          | ZExp.LeftPlus (z1,h1) -> ZExp.LeftPlus (fst (performSyn (z1,htype) a),h1)
          | ZExp.RightPlus (h1,z1) -> ZExp.RightPlus (h1,fst (performSyn (z1,htype) a)) 
          | ZExp.LamZ (var,z1) -> ZExp.LamZ (var,fst (performSyn (z1,htype) a))       (*  (ZExp.LeftPlus ((fst (performSyn (z1,htype) a)),h1)),htype *) (*  of t * HExp.t *)
          | ZExp.LeftAsc (z1,a1) -> ZExp.LeftAsc (fst (performSyn (z1,htype) a),a1)
          | ZExp.RightAsc (a1,z1) -> ZExp.RightAsc (a1, (performTyp (z1,a)) )(*   ZExp.RightAsc (a1, (performSyn (z1,a)))  *)
          | _ -> raise NotImplemented  
        end
      | _ -> raise NotImplemented
    in m,htype


  and performAna zexp htype a : ZExp.t =
    raise NotImplemented 



end

module Controller = struct

  open Action
  open Model.ZExp
  open Model.HExp
  open Model

  exception Exception
  let update a ((rs, rf) : rp) =
    let mOld = React.S.value rs in
    let m = (performSyn mOld a) in
    rf m

end

module View = struct

  open Action
  open Tyxml_js
  open Model.HType
  open Model.HExp
  open Model.ZExp
  open Model.ZType

  let rec stringFromHType (htype : Model.HType.t ) : string = match htype with
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

  let viewModel (rs, rf) =
    (*  let num = React.S.value rs in *)
    R.Html5.(pcdata (viewSignal (rs,rf))) 

  let viewActions (rs, rf) =
    let onClickDel evt =
      Controller.update (Action.Del) (rs, rf) ;
      true
    in
    (* Html5.(p [pcdata (stringFromZExp num)])  *)
    Html5.(button ~a:[a_onclick onClickDel] [pcdata "del"] )

  (*   let moveActions (rs, rf) =
       let onClickMove evt =
       Controller.update (Action.Move FirstChild) (rs, rf) ;
       true
       in
       (* Html5.(p [pcdata (stringFromZExp num)])  *)
       Html5.(button ~a:[a_onclick onClickMove] [pcdata "move"] ) *)
  let moveActions (rs, rf) =
    let onClickMoveLC evt =
      Controller.update (Action.Move FirstChild) (rs, rf) ;
      true
    in
    let onClickMoveP evt =
      Controller.update (Action.Move Parent) (rs, rf) ;
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
            button ~a:[a_onclick onClickMoveLC] [pcdata "move left child"] 
          ];
          li [
            button ~a:[a_onclick onClickMoveP] [pcdata "move parent"] 
          ];
          li [
            button ~a:[a_onclick onClickMoveNS] [pcdata "move next sib"] 
          ];
          li [
            button ~a:[a_onclick onClickMovePS] [pcdata "move prev sib"] 
          ]
        ]
      ]
      )

  (*  | SPlus -> (ZExp.FocusedE (HExp.Plus (HExp.EmptyHole,(HExp.EmptyHole)))) 
      (* | _ -> raise NotImplemented  *)
      | SArrow -> raise NotImplemented 
      | SNum -> raise NotImplemented 
      | SAsc -> raise NotImplemented 
      | SVar _ -> raise NotImplemented 
      | SLam _ -> raise NotImplemented 
      | SAp -> raise NotImplemented 
      | SArg -> raise NotImplemented 
      | SNumlit num -> ZExp.FocusedE (HExp.NumLit num)


  *)

  let addActions (rs, rf) =
    let onClickAddPlus evt =
      Controller.update (Action.Construct SPlus) (rs, rf) ;
      true
    in
    let onClickAddNumber evt =
      Controller.update (Action.Construct (SNumlit 1)) (rs, rf) ;
      true
    in
    let onClickAddLam evt =
      Controller.update (Action.Construct (SLam "lam")) (rs, rf) ;
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
    let onClickAddNum evt =
      Controller.update (Action.Construct (SNum)) (rs, rf) ;
      true
    in
    let onClickAddVar evt =
      Controller.update (Action.Construct (SVar "x")) (rs, rf) ;
      true
    in
    Html5.(div ~a:[a_class ["several"; "css"; "class"]; a_id "id-of-div"] [
        ul ~a:[a_class ["one-css-class"]; a_id "id-of-ul"] [
          li [
            button ~a:[a_onclick onClickAddPlus] [pcdata "Add Plus"] 
          ];
          li [
            button ~a:[a_onclick onClickAddNumber] [pcdata "Add Number"] 
          ];
          li [
            button ~a:[a_onclick onClickAddLam] [pcdata "Add Lambda"] 
          ];
          li [
            button ~a:[a_onclick onClickAddAsc] [pcdata "Add Ascription"] 
          ];
          li [
            button ~a:[a_onclick onClickAddApp] [pcdata "Add Appliction"] 
          ];
          li [
            button ~a:[a_onclick onClickAddNum] [pcdata "Add Num"] 
          ];
          li [
            button ~a:[a_onclick onClickAddVar] [pcdata "Add Var x"] 
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
            button ~a:[a_onclick onClickAddNum] [pcdata "Add Num"] 
          ];
          li [
            button ~a:[a_onclick onClickAddArrow] [pcdata "Add Arrow"] 
          ];
        ]
      ]
      )

  (* let addTextInput (rs, rf) =
     Html5.(textarea) *)
  (* ~name:textname *) (* 1 10 () *) 
  (* textarea [pcdata "del"]~a:[a_onclick onClickDel] (* [pcdata "del"] *) *) 

  let view (rs, rf) =
    let model = viewModel (rs, rf) in 
    let actions = viewActions (rs, rf) in 
    let mActions = moveActions (rs, rf) in
    let aActions = addActions (rs, rf) in
    let aTypes = addTypes (rs, rf) in
    (* let textInputs = addTextInput (rs, rf) in *)
    (* let actionsRP = moveActionsRP (rs, rf) in  *)
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
        (* div ~a:[a_class ["textArea"]]  [ addTextInput ] *)
        (* (* (* div ~a:[a_class ["ActionsRightPlus"]]  [ actionsRP ] *) *) *)
      ]
    ) 

end

let main _ =
  let doc = Dom_html.document in
  let parent =
    Js.Opt.get (doc##getElementById(Js.string "container"))
      (fun () -> assert false)
  in
  let m = Model.empty in
  let rp = React.S.create m in
  let input = Dom_html.createInput ~_type:(Js.string "text") doc in
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view rp)) ;
  Dom.appendChild parent input;
  Lwt.return ()

let _ = Lwt_js_events.onload () >>= main
