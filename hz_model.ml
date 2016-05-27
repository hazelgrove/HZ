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
          | PrevSib -> begin
              match ztype with 
              | ZType.SecondArrow (t1,t2) -> begin
                  match t2 with 
                  | ZType.FocusedT f1 -> ZType.FirstArrow ((ZType.FocusedT(t1)),f1)
                  | _ -> ZType.SecondArrow (t1,(performTyp (t2,a)))
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
              | HType.Num -> begin 
                  match shape with
                  | SArrow -> ZType.SecondArrow(t1,ZType.FocusedT HType.Hole)
                  | _ -> raise NotImplemented
                end
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
                  | HExp.Ap (a1,a2) -> ZExp.LeftAp ((ZExp.FocusedE a1),a2)
                end
              | ZExp.LeftPlus (z1,h1) -> ZExp.LeftPlus (fst (performSyn (z1,htype) a),h1)
              | ZExp.RightPlus (h1,z1) -> ZExp.RightPlus (h1,fst (performSyn (z1,htype) a))
              | ZExp.LeftAsc (z1,a1) -> ZExp.LeftAsc (fst (performSyn (z1,htype) a),a1)
              | ZExp.RightAsc (a1,z1) -> ZExp.RightAsc (a1, (performTyp (z1,a)) )  (* fst (performTyp (z1,htype) a) *)
              | ZExp.LeftAp (z1,a1) -> ZExp.LeftAp (fst (performSyn (z1,htype) a),a1)
              | ZExp.RightAp (a1,z1) -> ZExp.RightAp (a1,(fst (performSyn (z1,htype) a))) 
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
              | ZExp.LeftAp (z1,a1) -> begin
                  match z1 with 
                  | ZExp.FocusedE hexp -> ZExp.FocusedE (Ap (hexp,a1))
                  | _ -> ZExp.LeftAp((fst (performSyn (z1,htype) a)),a1)
                end
              | ZExp.RightAp (a1,z1) -> begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.FocusedE (Ap (a1,hexp))
                  | _ -> ZExp.RightAp(a1,(fst (performSyn (z1,htype) a)))
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
              | ZExp.LeftAp (z1,t1) ->   begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.RightAp (hexp, (ZExp.FocusedE t1))
                  | _ -> ZExp.LeftAp((fst (performSyn (z1,htype) a)),t1)
                end 
              | ZExp.RightAp (a1,z1) ->   begin
                  match z1 with
                  | _ -> ZExp.RightAp(a1,(fst (performSyn (z1,htype) a)))
                end    
            end
          | PrevSib -> begin
              match zexp with 
              | ZExp.RightAsc (a1,z1) -> begin
                  match z1 with
                  | ZType.FocusedT f1 -> ZExp.LeftAsc (ZExp.FocusedE (a1),f1)
                  | ZType.SecondArrow (t1,zt1) -> begin 
                    match zt1 with 
                    | ZType.FocusedT htype -> ZExp.RightAsc (a1,(ZType.FirstArrow (ZType.FocusedT (HType.Num),HType.Num)))
                  end

                  (* ZType.Arrow (a1,(performTyp (z1,a))) *)
                end
              | ZExp.RightAp (a1,z1) -> begin
                  match z1 with
                  | ZExp.FocusedE hexp -> ZExp.LeftAp (ZExp.FocusedE (a1),hexp)
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
              | SAp -> ZExp.RightAp (hexp,(ZExp.FocusedE HExp.EmptyHole))
              | _ -> raise NotImplemented 
            end
          | ZExp.LeftPlus (z1,h1) -> ZExp.LeftPlus (fst (performSyn (z1,htype) a),h1)
          | ZExp.RightPlus (h1,z1) -> ZExp.RightPlus (h1,fst (performSyn (z1,htype) a)) 
          | ZExp.LamZ (var,z1) -> ZExp.LamZ (var,fst (performSyn (z1,htype) a))       (*  (ZExp.LeftPlus ((fst (performSyn (z1,htype) a)),h1)),htype *) (*  of t * HExp.t *)
          | ZExp.LeftAsc (z1,a1) -> ZExp.LeftAsc (fst (performSyn (z1,htype) a),a1)
          | ZExp.RightAsc (a1,z1) -> ZExp.RightAsc (a1, (performTyp (z1,a)) )(*   ZExp.RightAsc (a1, (performSyn (z1,a)))  *)
          | ZExp.LeftAp (z1,h1) -> ZExp.LeftAp(fst (performSyn (z1,htype) a),h1)
          | ZExp.RightAp (h1,z1) -> ZExp.RightAp(h1,fst (performSyn (z1,htype) a))
          | _ -> raise NotImplemented  
        end
      | _ -> raise NotImplemented
    in m,htype


  and performAna zexp htype a : ZExp.t =
    raise NotImplemented 



end


