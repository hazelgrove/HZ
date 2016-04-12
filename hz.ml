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
      | Lam of t * t
      | Ap of t * t 
      | NumLit of int
      | Plus of t * t
      | EmptyHole 
      | InProgressHole of t
  end

  module ZType = struct
    type t = 
      | Selected of HType.t
      | First of HType.t
      | Secont of HType.t
  end

  module ZExp = struct
    type t = 
      | Selected of HType.t
      | First of HType.t
      | Second of HType.t
  end


  open HExp
  (* let empty = (HType.Arrow ((HType.Hole),(HType.Arrow ((HType.Num 1),(HType.Num 2)))))    *)
  let empty = Lam ((Var "x"),InProgressHole (Plus (NumLit 1, NumLit 3)))
end

type rs = Model.HType.t React.signal
type rf = ?step:React.step -> Model.HType.t -> unit
type rp = rs * rf

module Action = struct
  type direction =  
      FirstChile 
    | Parent 
    | NextSib 
    | PrevSib

  type t =
      Move of direction
    | Del 
    | Construct of Model.HExp.t
    | Finish

end

module Controller = struct

  open Action

  let update a ((rs, rf) : rp) =
    let m =
      (Model.HType.Num 2)
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
