open Lwt.Infix

module Model = struct

  module HType = struct 
    type t = 
        Num of int
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
  end

  module ZType = struct
    type t = 
      | FocusedT of HType.t
      | FirstArrow of t * HType.t
      | SecondArrow of t * HType.t
  end

  module ZExp = struct
    type t = 
      | FocusedE of HExp.t
      | LeftAsc of t * HType.t
      | RightAsc of HType.t * t 
      | LamZ of string * t
      | LeftAp of t * HExp.t
      | RightAp of HExp.t * t 
      | LeftPlus of t * HExp.t
      | RightPlus of HExp.t * t
      | NonEmptyHoleZ of t
  end


  open HExp
  (* let empty = (HType.Arrow ((HType.Hole),(HType.Arrow ((HType.Num 1),(HType.Num 2)))))    *)
  let empty = Lam ((Var "x"),InProgressHole (Plus (NumLit 1, NumLit 3)))
end

type rs = Model.ZExp.t React.signal
type rf = ?step:React.step -> Model.HType.t -> unit
type rp = rs * rf

module Action = struct
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

end

module Controller = struct

  open Action

  let update a ((rs, rf) : rp) =
    let m = React.S.value r in
    let m = 
      match a with Action.Del ->
      match m with 
      | EmptyHole -> 
        (* (Model.HType.Num 2) *)
    in
    rf m

end

module View = struct

  open Action
  open Tyxml_js
  open Model.HType
  open Model.HExp

  let rec stringFromHType (htype : Model.HType.t ) : string = match htype with
    | Num n -> string_of_int n
    | Arrow (fst,snd) -> "(" ^ stringFromHType (fst) ^ "->" ^ stringFromHType (snd) ^ ")"
    | Hole -> "H" 

  let rec stringFromHExp (hexp : Model.HExp.t ) : string = match hexp with
    | Asc (hexp,htype) -> (stringFromHExp hexp) ^ ":" ^ (stringFromHType htype)
    | Var str -> str
    | Lam (var,exp) -> "λ" ^ (stringFromHExp var) ^ "." ^ (stringFromHExp exp)
    | Ap (e1, e2) -> (stringFromHExp e1) ^ "(" ^ (stringFromHExp e2) ^ ")"
    | NumLit num -> string_of_int num
    | Plus (n1,n2) -> (stringFromHExp n1) ^"+"^ (stringFromHExp n2)
    | EmptyHole ->  "{}" 
    | InProgressHole hc -> "{" ^ (stringFromHExp hc) ^ "}"

  let viewNum (rs, rf) =
    let num = React.S.value rs in
    Html5.(p [pcdata (stringFromHExp num)]) 

  let view (rs, rf) =
    let num = viewNum (rs, rf) in 
    Html5.(
      div [
        div ~a:[a_class ["comments"]] [
          p [
            pcdata "HZ model"
          ] ;
        ] ;
        div ~a:[a_class ["Model"]]  [ num ]
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
  Dom.appendChild parent (Tyxml_js.To_dom.of_div (View.view rp)) ;
  Lwt.return ()

let _ = Lwt_js_events.onload () >>= main
