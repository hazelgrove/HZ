open Hz_controller
open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model

open React;;
open Lwt.Infix;; 

module StringView = struct
  let rec of_htype (htype : HType.t ) : string = match htype with
    | HType.Num -> "num"
    | HType.Arrow (fst,snd) -> "(" ^ of_htype (fst) ^ "->" ^ of_htype (snd) ^ ")"
    | HType.Hole -> "(||)"  

  let rec of_hexp (hexp : HExp.t ) : string = match hexp with
    | HExp.Asc (hexp,htype) -> (of_hexp hexp) ^ " : " ^ (of_htype htype)
    | HExp.Var str -> str
    | HExp.Lam (var,exp) -> "λ" ^  var ^ "." ^ (of_hexp exp)
    | HExp.Ap (e1, e2) -> (of_hexp e1) ^ "(" ^ (of_hexp e2) ^ ")"
    | HExp.NumLit num -> string_of_int num
    | HExp.Plus (n1,n2) -> (of_hexp n1) ^ " + " ^ (of_hexp n2)
    | HExp.EmptyHole ->  "(||)" 
    | HExp.NonEmptyHole hc -> "(|" ^ (of_hexp hc) ^ "|)"

  let rec of_ztype (ztype : ZType.t ) : string = match ztype with
    | ZType.FocusedT htype -> "⊳" ^ of_htype htype ^ "⊲"
    | ZType.LeftArrow  (ztype, htype) -> "(" ^ of_ztype ztype  ^ " -> " ^ of_htype htype ^ ")"
    | ZType.RightArrow (htype, ztype) -> "(" ^ of_htype htype ^ " -> " ^ of_ztype ztype ^ ")"

  let rec of_zexp (zexp : ZExp.t ) : string = match zexp with
    | ZExp.FocusedE hexp -> "⊳" ^ of_hexp hexp ^ "⊲"
    | ZExp.LeftAsc (e, asc) -> (* "LA" ^ *)  of_zexp e ^ " : " ^ of_htype asc 
    | ZExp.RightAsc (e, asc) -> of_hexp e ^ " : " ^ of_ztype asc
    | ZExp.LamZ (var,exp) -> "λ" ^  var ^ "." ^ (of_zexp exp)
    | ZExp.LeftAp (e1,e2) -> of_zexp e1 ^ "(" ^ of_hexp e2 ^ ")"
    | ZExp.RightAp (e1,e2) -> of_hexp e1 ^ "(" ^ of_zexp e2 ^ ")"
    | ZExp.LeftPlus (num1,num2) -> of_zexp num1 ^ " + " ^ of_hexp num2
    | ZExp.RightPlus (num1,num2) -> of_hexp num1  ^ " + " ^ of_zexp num2
    | ZExp.NonEmptyHoleZ e -> "(|" ^ of_zexp e ^ "|)"
end   

(* TODO: put common utils somewhere sensible *)
exception No_value 
let opt_get opt = match opt with Some x -> x | _ -> raise No_value

module Ev = Lwt_js_events
let bind_event ev elem handler = 
  let handler evt _ = handler evt in 
  Ev.(async @@ (fun () -> ev elem handler))

(* create an input and a reactive signal tracking its 
 * string value *)
let r_input attrs = 
  let rs, rf = S.create "" in 
  let i_elt = Html5.input ~a:attrs () in 
  let i_dom = To_dom.of_input i_elt in 
  let _ = bind_event Ev.inputs i_dom (fun _ -> 
      Lwt.return @@ (rf (Js.to_string i_dom##value))) in 
  (rs, i_elt, i_dom)

module View = struct
  let view ((rs, rf) : Model.rp) = 
    (* zexp view *)
    let zexp_view_rs = React.S.map (fun (zexp, _) -> 
        StringView.of_zexp zexp) rs in 
    let zexp_view = Html5.(div [R.Html5.pcdata zexp_view_rs]) in 

    (* helper function for constructing simple action buttons *)
    let action_button action btn_label = 
      Html5.(button ~a:[
          a_onclick (fun _ -> 
              rf (
                Action.performSyn Ctx.empty action (React.S.value rs));
              true); 
          R.filter_attrib
            (a_disabled ())
            (S.map (fun m -> 
                 try 
                   let _ = Action.performSyn Ctx.empty action m in false 
                 with Action.InvalidAction -> true) rs)
        ] [pcdata btn_label]) in 

    (* actions that take an input. the conversion function
     * goes from a string to an arg option where arg is 
     * the action argument. *)
    let action_input_button action conv btn_label = 
      let i_rs, i_elt, _ = r_input [] in 
      Html5.(div [ 
          i_elt;
          button ~a:[
            a_onclick (fun _ -> 
                let arg = opt_get (conv (React.S.value i_rs)) in 
                rf (
                  Action.performSyn 
                    Ctx.empty
                    (action arg)
                    (React.S.value rs)); 
                true
              ); 
            R.filter_attrib
              (a_disabled ())
              (S.l2 (fun s m -> 
                   match conv s with 
                     Some arg -> 
                     begin try 
                         let _ = Action.performSyn Ctx.empty (action arg) m in false 
                       with Action.InvalidAction -> true end 
                   | _ -> true) i_rs rs)
          ] [pcdata btn_label]
        ]) in 

    Html5.(div [
        div ~a:[a_class ["Model"]] [zexp_view];
        div ~a:[a_class ["Actions"]] [
          (action_button (Action.Del) "del");
          (action_button (Action.Move (Action.Parent)) "move parent");
          (action_button (Action.Move (Action.FirstChild)) "move FirstChild");
          (action_button (Action.Move (Action.NextSib)) "move NextSib");
          (action_button (Action.Move (Action.PrevSib)) "move PrevSib");
          (action_button (Action.Construct Action.SNum) "construct SNum");
          (action_button (Action.Construct Action.SArrow) "construct SArrow");
          (action_button (Action.Construct Action.SAsc) "construct SAsc");
          (action_button (Action.Construct Action.SAp) "construct SAp");
          (action_button (Action.Construct Action.SArg) "construct SArg");
          (action_button (Action.Construct Action.SPlus) "construct SPlus");
          (action_input_button 
             (fun n -> Action.Construct (Action.SNumLit n)) 
             (fun s -> try Some (int_of_string s) with Failure "int_of_string" -> None) 
             "construct SNumLit");
          (action_input_button 
             (fun v -> Action.Construct (Action.SVar v)) 
             (fun s -> try Some (s) with Failure "int_of_string" -> None) 
             "construct SVar");
          (action_input_button 
             (fun v -> Action.Construct (Action.SLam v)) 
             (fun s -> try Some (s) with Failure "int_of_string" -> None) 
             "construct SLam");
          (action_button (Action.Finish) "Finish")
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
