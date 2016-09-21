open Hz_controller
open Tyxml_js
open Hz_semantics
open Hz_model
open Hz_model.Model

open React;;
open Lwt.Infix;;

exception NotImplemented

(* module StringView = struct
   let rec of_htype (htype : HTyp.t ) : string = match htype with
    | HTyp.Num -> "num"
    | HTyp.Arrow (fst,snd) -> "(" ^ of_htype (fst) ^ "->" ^ of_htype (snd) ^ ")"
    | HTyp.Hole -> "(||)"

   let rec of_hexp (hexp : HExp.t ) : string = match hexp with
    | HExp.Asc (hexp,htype) -> (of_hexp hexp) ^ " : " ^ (of_htype htype)
    | HExp.Var str -> str
    | HExp.Lam (var,exp) -> "λ" ^  var ^ "." ^ (of_hexp exp)
    | HExp.Ap (e1, e2) -> (of_hexp e1) ^ "(" ^ (of_hexp e2) ^ ")"
    | HExp.NumLit num -> string_of_int num
    | HExp.Plus (n1,n2) -> (of_hexp n1) ^ " + " ^ (of_hexp n2)
    | HExp.EmptyHole ->  "(||)"
    | HExp.NonEmptyHole hc -> "(|" ^ (of_hexp hc) ^ "|)"

   let rec of_ztype (ztype : ZTyp.t ) : string = match ztype with
    | ZTyp.CursorT htype -> "⊳" ^ of_htype htype ^ "⊲"
    | ZTyp.LeftArrow  (ztype, htype) -> "(" ^ of_ztype ztype  ^ " -> " ^ of_htype htype ^ ")"
    | ZTyp.RightArrow (htype, ztype) -> "(" ^ of_htype htype ^ " -> " ^ of_ztype ztype ^ ")"

   let rec of_zexp (zexp : ZExp.t ) : string = match zexp with
    | ZExp.CursorE hexp -> "⊳" ^ of_hexp hexp ^ "⊲"
    | ZExp.LeftAsc (e, asc) -> (* "LA" ^ *)  of_zexp e ^ " : " ^ of_htype asc
    | ZExp.RightAsc (e, asc) -> of_hexp e ^ " : " ^ of_ztype asc
    | ZExp.LamZ (var,exp) -> "λ" ^  var ^ "." ^ (of_zexp exp)
    | ZExp.LeftAp (e1,e2) -> of_zexp e1 ^ "(" ^ of_hexp e2 ^ ")"
    | ZExp.RightAp (e1,e2) -> of_hexp e1 ^ "(" ^ of_zexp e2 ^ ")"
    | ZExp.LeftPlus (num1,num2) -> of_zexp num1 ^ " + " ^ of_hexp num2
    | ZExp.RightPlus (num1,num2) -> of_hexp num1  ^ " + " ^ of_zexp num2
    | ZExp.NonEmptyHoleZ e -> "(|" ^ of_zexp e ^ "|)"
   end *)


module HTMLView = struct
  open Html
  (* let larrow = img ~alt:("left arrow") ~src:("imgs/l-triangle.svg") () *)
  (* let larrow = div ~a:[a_class ["HZElem";"lTri"]] []
     let rarrow = img ~alt:("right arrow") ~src:("imgs/r-triangle.svg") () *)


  let hzdiv str contents =  Html.(div ~a:[a_class ["HZElem";str]] contents)

  let hotdog = hzdiv  "hotdog" [pcdata "(||)"]
  let ascChar = hzdiv "asc" [pcdata ":"]
  (* let plusChar = (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]) *)
  (* let lParensChar = hzdiv "lParens" [pcdata "("] *)
  (* let rParensChar = hzdiv "rParens" [pcdata ")"] *)
  (* let dotChar =   hzdiv "dot" [pcdata "."] *)
  (* let lambdaChar = hzdiv "lambda" [pcdata "λ"] *)
  (* let lambdaChar = Html.(div ~a:[a_class ["HZElem";"lambda"]] [pcdata "λ"]) *)
  let lAscChar =  hzdiv "lAsc" [pcdata "⊳"]
  let rAscChar =  hzdiv "rAsc" [pcdata "⊲"]
  (* let arrowChar = (hzdiv "arrow" [pcdata "->"]) *)


  let rec of_htype (htype : HTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match htype with
    | HTyp.Num -> hzdiv "num" [pcdata "num"]
    | HTyp.Arrow (fst,snd) -> hzdiv "arrowType" [of_htype (fst); hzdiv "arrow" [pcdata "->"]; of_htype (snd)]
    | HTyp.Hole ->  hzdiv  "hotdog" [pcdata "(||)"]

  let rec of_hexp (hexp : HExp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match hexp with
    | HExp.Lam (var,exp) -> hzdiv "lambdaExp" [(div ~a:[a_class ["HZElem";"lambda"]] [pcdata "λ"]) ; hzdiv "hexp" [pcdata var]; (div ~a:[a_class ["HZElem";"dot"]] [pcdata "."]); hzdiv "hexp" [of_hexp exp]]
    | HExp.Asc (hexp,htype) -> hzdiv "Asc" [hzdiv "hexp" [of_hexp hexp]; ascChar; hzdiv "hexp" [of_htype htype]]
    | HExp.Var str -> hzdiv "var" [pcdata str]
    | HExp.Ap (e1, e2) -> hzdiv "Ap" [of_hexp e1; (div ~a:[a_class ["HZElem";"lparens"]] [pcdata "("]); of_hexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [pcdata ")"])]
    | HExp.NumLit num -> hzdiv "numLit" [pcdata (string_of_int num)]
    | HExp.Plus (n1,n2) -> hzdiv "plus" [(of_hexp n1); (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]); (of_hexp n2)]
    | HExp.EmptyHole ->  hzdiv  "hotdog" [pcdata "(||)"]
    | HExp.NonEmptyHole hc -> Html.(div [pcdata "NonEmptyHole Not Implemented"])

  let rec of_ztype (ztype : ZTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match ztype with
    | ZTyp.CursorT htype ->  hzdiv "CursorT" [lAscChar ; (of_htype htype) ;rAscChar]
    | ZTyp.LeftArrow  (ztype, htype) ->  hzdiv "leftArrow" [(of_ztype ztype); hzdiv "arrow" [pcdata "->"]; (of_htype htype)]
    | ZTyp.RightArrow (htype, ztype) -> hzdiv "rightArrow" [(of_htype htype); hzdiv "arrow" [pcdata "->"]; (of_ztype ztype)]

  let rec of_zexp (zexp : ZExp.t ) :  [> Html_types.div ] Tyxml_js.Html.elt  =
    match zexp with
    | ZExp.RightAsc (e, asc) ->  hzdiv "rAsc" [(of_hexp e) ; ascChar; (of_ztype asc)]
    | ZExp.LeftAsc (e, asc) ->   hzdiv "lAsc" [(of_zexp e) ; ascChar; (of_htype asc)]
    | ZExp.CursorE hexp -> hzdiv "CursorE" [lAscChar; (of_hexp hexp); rAscChar]
    | ZExp.LamZ (var,exp) -> hzdiv "lambdaExp" [(div ~a:[a_class ["HZElem";"lambda"]] [pcdata "λ"]) ;hzdiv "var" [pcdata var];(div ~a:[a_class ["HZElem";"dot"]] [pcdata "."]); hzdiv "hexp" [of_zexp exp]]
    | ZExp.LeftAp (e1,e2) -> hzdiv "lAp" [of_zexp e1; (div ~a:[a_class ["HZElem";"lparens"]] [pcdata "("]); of_hexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [pcdata ")"])]
    | ZExp.RightAp (e1,e2) ->  hzdiv "rAp" [of_hexp e1; (div ~a:[a_class ["HZElem";"lparens"]] [pcdata "("]); of_zexp e2; (div ~a:[a_class ["HZElem";"rParens"]] [pcdata ")"])]
    | ZExp.LeftPlus (num1,num2) -> hzdiv "lPlus" [(of_zexp num1); (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]); (of_hexp num2)]
    | ZExp.RightPlus (num1,num2) -> hzdiv "rPlus" [(of_hexp num1); (div ~a:[a_class ["HZElem";"plus"]] [pcdata "+"]); (of_zexp num2)]
    | ZExp.NonEmptyHoleZ e -> Html.(div [pcdata "NonEmptyHoleZ Not Implemented"])
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
      Lwt.return @@
      (rf (Js.to_string (i_dom##.value)))) in
  (rs, i_elt, i_dom)

module View = struct

  let keyActions (event) =
    match  char_of_int event##.keyCode with
    | 'w' -> Action.performSyn Ctx.empty (Action.Move (Action.Parent))
    | 'a' -> Action.performSyn Ctx.empty (Action.Move (Action.FirstChild))
    | 'd' -> Action.performSyn Ctx.empty (Action.Move (Action.NextSib))
    | 's' -> Action.performSyn Ctx.empty (Action.Del)
    | 'j' -> Action.performSyn Ctx.empty (Action.Construct Action.SArrow)
    | 'k' -> Action.performSyn Ctx.empty (Action.Construct Action.SNum)
    |  'l' -> Action.performSyn Ctx.empty (Action.Construct Action.SAsc)
    |  'm' -> Action.performSyn Ctx.empty  (Action.Construct Action.SAp)
    | ',' -> Action.performSyn Ctx.empty (Action.Construct Action.SArg)
    |  ';' -> Action.performSyn Ctx.empty (Action.Construct Action.SPlus)
    |  '.' -> Action.performSyn Ctx.empty (Action.Finish)
    | _ -> raise NotImplemented
  (* |  -> Action.performSyn Ctx.empty *)

  let view ((rs, rf) : Model.rp) =
    (* zexp view *)
    let zexp_view_rs = React.S.map (fun (zexp, _) ->
        [HTMLView.of_zexp zexp]) rs in
    let zexp_view = Html5.(R.Html5.div (ReactiveData.RList.from_signal zexp_view_rs)) in

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
                 with Action.InvalidAction -> true
                    | HExp.IllTyped -> true ) rs)
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
                       with Action.InvalidAction -> true
                          | HExp.IllTyped -> true   end
                   | _ -> true) i_rs rs)
          ] [pcdata btn_label]
        ]) in

    Html5.(
      div [ div  ~a:[a_class ["jumbotron"]]
              [ div  ~a:[a_class ["display-3"]] [pcdata "HZ"];
                div  ~a:[a_class ["subtext"]] [pcdata "(a reference implementation of Hazelnut)"];
                div ~a:[a_class ["Model"]] [zexp_view];];
            div ~a:[a_class ["row";"marketing"]] [
              div ~a:[a_class ["col-lg-3"]] [
                (action_button (Action.Move (Action.FirstChild)) "move firstChild (a)");
                br ();
                (action_button (Action.Move (Action.Parent)) "move parent (w)");
                br ();
                (action_button (Action.Move (Action.NextSib)) "move nextSib (d)");
                br ();
                (action_button (Action.Del) "del (s)");
              ];
              div ~a:[a_class ["col-lg-3"]] [
                (action_button (Action.Construct Action.SArrow) "construct arrow (j)");
                br ();
                (action_button (Action.Construct Action.SNum) "construct num (k)");
                br ();
              ];
              div ~a:[a_class ["col-lg-6"]] [
                (action_button (Action.Construct Action.SAsc) "construct asc (l)");
                br ();
                (action_input_button
                   (fun v -> Action.Construct (Action.SVar v))
                   (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                   "construct var");
                (action_input_button
                   (fun v -> Action.Construct (Action.SLam v))
                   (fun s -> match String.compare s "" with 0 -> None | _ -> Some s)
                   "construct lam");
                (action_button (Action.Construct Action.SAp) "construct ap (m)");
                br ();
                (action_button (Action.Construct Action.SArg) "construct arg (,)");
                br ();
                (action_input_button
                   (fun n -> Action.Construct (Action.SLit n))
                   (fun s -> try Some (int_of_string s) with Failure _ -> None)
                   "construct lit");
                (action_button (Action.Construct Action.SPlus) "construct plus (;)");
                br ();
                br ();
                (action_button (Action.Finish) "finish (.)")
              ]
            ];
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
  bind_event Ev.keypresses doc (fun evt ->
      Lwt.return @@ rf (View.keyActions evt (React.S.value rs) ) );
  (* Lwt.return @@ rf (Action.performSyn Ctx.empty (Action.Move (Action.Parent)) (React.S.value rs)) *)
  Lwt.return ()
let _ = Lwt_js_events.onload () >>= main
