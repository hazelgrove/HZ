open Lwt.Infix

module Model = struct

  module HType = struct 
    type t = 
        Num of int
      | Arrow of t * t

  end

  let empty = (HType.Num 5)
end

type rs = Model.HType.t React.signal
type rf = ?step:React.step -> Model.HType.t -> unit
type rp = rs * rf

module Action = struct
end

module Controller = struct

  open Action

  let update a ((rs, rf) : rp) =
    (*     let a = React.S.value rs in *)
    let m =
      (Model.HType.Num 2)
    in
    rf m

end

module View = struct

  open Action
  open Tyxml_js
  open Model.HType

  let rec intFromView (htype : Model.HType.t ) : string = match htype with
    | Num n -> string_of_int n
    | Arrow (fst,snd) -> intFromView (fst) ^ "->" ^ intFromView (fst)

  let viewNum (rs, rf) =
    let num = React.S.value rs in
    Html5.(p [pcdata (intFromView num)]) 

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
