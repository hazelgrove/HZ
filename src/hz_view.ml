(* generates divs from terms *)
module HTMLView = struct
  module Html = Tyxml_js.Html
  open Html
  open Hz_semantics

  let hzdiv str contents =  Html.(div ~a:[a_class ["HZElem";str]] contents)

  let rec of_htype (htype : HTyp.t ) : [> Html_types.div ] Html.elt  =
    match htype with
    | HTyp.Num -> hzdiv "Num" []
    | HTyp.Arrow (fst,snd) -> hzdiv "Arrow" [hzdiv "leftParens" []; of_htype (fst); hzdiv "arrow" []; of_htype (snd); hzdiv "rightParens" []]
    | HTyp.Hole ->  hzdiv "Hole" []
    | HTyp.Sum (fst,snd) -> hzdiv "Sum" [hzdiv "leftParens" []; of_htype (fst); hzdiv "plusSign" []; of_htype (snd); hzdiv "rightParens" []]

  let rec of_hexp (hexp : HExp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match hexp with
    | HExp.Lam (var,exp) -> hzdiv "Lam" [(div ~a:[a_class ["HZElem";"lambda"]] []); hzdiv "hexp" [pcdata var]; (div ~a:[a_class ["HZElem";"dot"]] []); hzdiv "hexp" [of_hexp exp]]
    | HExp.Asc (hexp,htype) -> hzdiv "Asc" [hzdiv "hexp" [of_hexp hexp]; hzdiv "asc" []; hzdiv "hexp" [of_htype htype]]
    | HExp.Var str -> hzdiv "Var" [pcdata str]
    | HExp.Ap (e1, e2) -> hzdiv "Ap" [of_hexp e1; hzdiv "leftParens" []; of_hexp e2; hzdiv "rightParens" []]
    | HExp.NumLit num -> hzdiv "NumLit" [pcdata (string_of_int num)]
    | HExp.Plus (n1,n2) -> hzdiv "Plus" [hzdiv "leftParens" [];(of_hexp n1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_hexp n2); hzdiv "rightParens" []]
    | HExp.EmptyHole ->  hzdiv "EmptyHole" []
    | HExp.NonEmptyHole hc -> hzdiv "NonEmptyHole" [hzdiv "lNE" []; of_hexp hc; hzdiv "rNE" []]
    | HExp.Inj (side,exp) -> hzdiv "Inj" [hzdiv "inj" []; hzdiv "leftParens" []; of_hexp exp; hzdiv "rightParens" []]
    | HExp.Case (e,(var1,exp1),(var2,exp2)) ->
      hzdiv "Case" [
        hzdiv "case" [];
        hzdiv "leftParens" [];
        hzdiv "exp" [of_hexp e];
        hzdiv "comma" [];
        hzdiv "var1" [pcdata var1];
        hzdiv "dot" [];
        hzdiv "exp1" [of_hexp exp1];
        hzdiv "comma" [];
        hzdiv "var2" [pcdata var2];
        hzdiv "dot" [];
        hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" []
      ]

  let lAscChar =  hzdiv "lAsc" []
  let rAscChar =  hzdiv "rAsc" []

  let rec of_ztype (ztype : ZTyp.t ) : [> Html_types.div ] Tyxml_js.Html.elt  =
    match ztype with
    | ZTyp.CursorT htype -> hzdiv "CursorT" [lAscChar; (of_htype htype) ;rAscChar]
    | ZTyp.LeftArrow  (ztype, htype) -> hzdiv "LeftArrow"  [hzdiv "leftParens" [];(of_ztype ztype); hzdiv "arrow" []; (of_htype htype); hzdiv "rightParens" []]
    | ZTyp.RightArrow (htype, ztype) -> hzdiv "RightArrow" [hzdiv "leftParens" [];(of_htype htype); hzdiv "arrow" []; (of_ztype ztype); hzdiv "rightParens" []]
    | ZTyp.LeftSum (ztype, htype) -> hzdiv "LeftSum"  [hzdiv "leftParens" [];(of_ztype ztype); hzdiv "plusSign" []; (of_htype htype); hzdiv "rightParens" []]
    | ZTyp.RightSum (htype,ztype) -> hzdiv "RightSum" [hzdiv "leftParens" [];(of_htype htype); hzdiv "plusSign" []; (of_ztype ztype); hzdiv "rightParens" []]

  let rec of_zexp (zexp : ZExp.t ) :  [> Html_types.div ] Tyxml_js.Html.elt  =
    match zexp with
    | ZExp.RightAsc (e, asc) -> hzdiv "RightAsc" [(of_hexp e); hzdiv "asc" []; (of_ztype asc)]
    | ZExp.LeftAsc (e, asc) -> hzdiv "LeftAsc" [(of_zexp e); hzdiv "asc" []; (of_htype asc)]
    | ZExp.CursorE hexp -> hzdiv "CursorE" [lAscChar; (of_hexp hexp); rAscChar]
    | ZExp.LamZ (var,exp) -> hzdiv "LamZ" [(div ~a:[a_class ["HZElem";"lambda"]] []); hzdiv "var" [pcdata var];(div ~a:[a_class ["HZElem";"dot"]] []); hzdiv "hexp" [of_zexp exp]]
    | ZExp.LeftAp (e1,e2) -> hzdiv "LeftAp" [of_zexp e1; (div ~a:[a_class ["HZElem";"leftParens"]] []); of_hexp e2; (div ~a:[a_class ["HZElem";"rightParens"]] [])]
    | ZExp.RightAp (e1,e2) ->  hzdiv "RightAp" [of_hexp e1; (div ~a:[a_class ["HZElem";"leftParens"]] []); of_zexp e2; (div ~a:[a_class ["HZElem";"rightParens"]] [])]
    | ZExp.LeftPlus (num1,num2) -> hzdiv "LeftPlus" [hzdiv "leftParens" [];(of_zexp num1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_hexp num2); hzdiv "rightParens" []]
    | ZExp.RightPlus (num1,num2) -> hzdiv "RightPlus" [hzdiv "leftParens" [];(of_hexp num1); (div ~a:[a_class ["HZElem";"plusSign"]] []); (of_zexp num2); hzdiv "rightParens" []]
    | ZExp.NonEmptyHoleZ e ->  hzdiv "NonEmptyHoleZ" [hzdiv "lNZ" []; of_zexp e; hzdiv "rNZ" []]
    | ZExp.InjZ (side,exp) -> hzdiv "Inj" [hzdiv "inj" []; hzdiv "leftParens" []; of_zexp exp; hzdiv "rightParens" []]
    | ZExp.CaseZ1 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv "Case" [
        hzdiv "case" []; hzdiv "leftParens" []; hzdiv "exp2" [of_zexp e];
        hzdiv "comma" []; hzdiv "var1" [pcdata var1];
        hzdiv "dot" []; hzdiv "exp1" [of_hexp exp1];
        hzdiv "comma" []; hzdiv "var2" [pcdata var2];
        hzdiv "dot" []; hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" []
      ]
    | ZExp.CaseZ2 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv "Case" [
        hzdiv "case" []; hzdiv "leftParens" []; hzdiv "exp2" [of_hexp e];
        hzdiv "comma" []; hzdiv "var1" [pcdata var1];
        hzdiv "dot" []; hzdiv "exp1" [of_zexp exp1];
        hzdiv "comma" []; hzdiv "var2" [pcdata var2];
        hzdiv "dot" []; hzdiv "exp2" [of_hexp exp2];
        hzdiv "rightParens" []
      ]
    | ZExp.CaseZ3 (e,(var1,exp1) ,(var2,exp2)) -> hzdiv "Case" [
        hzdiv "case" []; hzdiv "leftParens" []; hzdiv "exp2" [of_hexp e]; hzdiv "comma" [];
        hzdiv "var1" [pcdata var1]; hzdiv "dot" [];
        hzdiv "exp1" [of_hexp exp1]; hzdiv "comma" [];
        hzdiv "var2" [pcdata var2]; hzdiv "dot" []; hzdiv "exp2" [of_zexp exp2]; hzdiv "rightParens" []
      ]
end

