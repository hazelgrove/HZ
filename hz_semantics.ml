module HType = struct 
  type t = 
    | Num
    | Arrow of t * t 
    | Hole

  let rec eq ty1 ty2 = match (ty1, ty2) with 
    | (Num, Num) -> true
    | (Arrow (ty1_left, ty1_right), Arrow (ty2_left, ty2_right)) -> 
      (eq ty1_left ty2_left) && (eq ty1_right ty2_right)
    | (Hole, Hole) -> true
    | _ -> false

  let rec compat ty1 ty2 = eq ty1 ty2 || (
      match (ty1, ty2) with 
      | (Hole, _) -> true
      | (_, Hole) -> true
      | (Arrow (ty1_left, ty1_right), Arrow (ty2_left, ty2_right)) -> 
        (compat ty1_left ty1_right) && (compat ty2_left ty2_right)
      | _ -> false
    )

  let incompat ty1 ty2 = not (compat ty1 ty2)

  let rec complete ty = match ty with 
    | Num -> true
    | Arrow (ty1, ty2) -> (complete ty1) && (complete ty2)
    | Hole -> false
end

module Var = struct
  type t = string
end

module Ctx : sig
  type t
  val empty : t
  val extend : t -> Var.t * HType.t -> t
  val lookup : t -> Var.t -> HType.t option
end = struct
  type t = (Var.t * HType.t) list

  let empty = []

  let extend ctx (x, ty) = (x, ty) :: ctx

  let rec lookup ctx x = match ctx with 
    | [] -> None
    | (y, ty) :: ctx' -> 
      begin match String.compare x y with 
        | 0 -> Some ty 
        | _ -> lookup ctx' x 
      end 
end

module HExp = struct
  type t = 
    | Asc of t * HType.t 
    | Var of Var.t
    | Lam of Var.t * t
    | Ap of t * t 
    | NumLit of int
    | Plus of t * t
    | EmptyHole 
    | NonEmptyHole of t

  exception IllTyped

  let rec syn ctx e = match e with 
    | Asc (e', ty) -> let _ = ana ctx e' ty in ty
    | Var x -> 
      begin match (Ctx.lookup ctx x) with 
        | Some ty -> ty
        | None -> raise IllTyped
      end
    | Ap (e1, e2) -> 
      let ty1 = syn ctx e1 in 
      begin match ty1 with 
        | HType.Arrow (ty1_left, ty1_right) -> 
          let _ = ana ctx e2 ty1_left in 
          ty1_right
        | HType.Hole -> 
          let _ = ana ctx e2 HType.Hole in 
          HType.Hole
        | _ -> raise IllTyped
      end
    | NumLit _ -> HType.Num
    | Plus (e1, e2) -> 
      let _ = ana ctx e1 HType.Num in 
      let _ = ana ctx e2 HType.Num in 
      HType.Num
    | EmptyHole -> HType.Hole
    | NonEmptyHole e' -> 
      let _ = syn ctx e' in 
      HType.Hole
    | _ -> raise IllTyped
  and ana ctx e ty = match e with 
    | Lam (x, e') -> 
      begin match ty with 
        | HType.Arrow (ty1, ty2) -> 
          let ctx' = Ctx.extend ctx (x, ty1) in 
          ana ctx' e' ty2
        | _ -> raise IllTyped
      end 
    | _ -> 
      let ty' = syn ctx e in 
      if HType.compat ty ty' then ()
      else raise IllTyped

  let rec complete e = match e with 
    | Asc (e', ty) -> (complete e') && (HType.complete ty)
    | Var _ -> true
    | Lam (_, e') -> complete e'
    | Ap (e1, e2) -> (complete e1) && (complete e2)
    | NumLit _ -> true
    | Plus (e1, e2) -> (complete e1) && (complete e2)
    | EmptyHole -> false
    | NonEmptyHole _ -> false
end

module ZType = struct
  type t = 
    | FocusedT of HType.t
    | LeftArrow of t * HType.t
    | RightArrow of HType.t * t 

  let rec erase zty = match zty with 
    | FocusedT ty -> ty
    | LeftArrow (zty1, ty2) -> HType.Arrow ((erase zty1), ty2)
    | RightArrow (ty1, zty2) -> HType.Arrow (ty1, (erase zty2))
end

module ZExp = struct
  type t = 
    | FocusedE of HExp.t
    | LeftAsc of t * HType.t
    | RightAsc of HExp.t * ZType.t
    | LamZ of Var.t * t
    | LeftAp of t * HExp.t
    | RightAp of HExp.t * t 
    | LeftPlus of t * HExp.t
    | RightPlus of HExp.t * t
    | NonEmptyHoleZ of t

  let rec erase ze = match ze with 
    | FocusedE e -> e
    | LeftAsc (ze', ty) -> HExp.Asc ((erase ze'), ty)
    | RightAsc (e', zty) -> HExp.Asc (e', (ZType.erase zty))
    | LamZ (x, ze') -> HExp.Lam (x, (erase ze'))
    | LeftAp (ze', e) -> HExp.Ap ((erase ze'), e)
    | RightAp (e, ze') -> HExp.Ap (e, (erase ze'))
    | LeftPlus (ze', e) -> HExp.Plus ((erase ze'), e)
    | RightPlus (e, ze') -> HExp.Plus (e, (erase ze'))
    | NonEmptyHoleZ ze' -> HExp.NonEmptyHole (erase ze')
end

module Action = struct
  type direction =  
    | FirstChild 
    | Parent 
    | NextSib 
    | PrevSib

  type shape = 
    | SArrow
    | SNum
    | SAsc
    | SVar of Var.t
    | SLam of Var.t
    | SAp 
    | SArg
    | SNumLit of int
    | SPlus

  type t =
    | Move of direction
    | Del 
    | Construct of shape
    | Finish

  exception InvalidAction

  let rec performTyp a zty = match (a, zty) with 
    | (Move FirstChild, ZType.FocusedT (HType.Arrow (ty1, ty2))) -> 
      ZType.LeftArrow ((ZType.FocusedT ty1), ty2)
    | (Move Parent, ZType.LeftArrow ((ZType.FocusedT ty1), ty2)) -> 
      ZType.FocusedT (HType.Arrow (ty1, ty2))
    | (Move Parent, ZType.RightArrow (ty1, ZType.FocusedT ty2)) -> 
      ZType.FocusedT (HType.Arrow (ty1, ty2))
    | (Move NextSib, ZType.LeftArrow ((ZType.FocusedT ty1), ty2)) -> 
      ZType.RightArrow (ty1, ZType.FocusedT ty2)
    | (Move PrevSib, ZType.RightArrow (ty1, ZType.FocusedT ty2)) -> 
      ZType.LeftArrow (ZType.FocusedT ty1, ty2)
    | (Del, ZType.FocusedT ty) -> 
      ZType.FocusedT (HType.Hole)
    | (Construct SArrow, ZType.FocusedT ty) -> 
      ZType.RightArrow (ty, ZType.FocusedT HType.Hole)
    | (Construct SNum, ZType.FocusedT HType.Hole) -> 
      ZType.FocusedT HType.Num
    | (_, ZType.LeftArrow (zty1, ty2)) -> 
      let zty1' = performTyp a zty1 in 
      ZType.LeftArrow (zty1', ty2)
    | (_, ZType.RightArrow (ty1, zty2)) -> 
      let zty2' = performTyp a zty2 in 
      ZType.RightArrow (ty1, zty2')
    | _ -> 
      raise InvalidAction

  let rec performEMove direction ze = match (direction, ze) with 
    (* Ascription *)
    | (FirstChild, ZExp.FocusedE (HExp.Asc (e, ty))) -> 
      ZExp.LeftAsc ((ZExp.FocusedE e), ty)
    | (Parent, ZExp.LeftAsc ((ZExp.FocusedE e), ty)) -> 
      ZExp.FocusedE (HExp.Asc (e, ty))
    | (Parent, ZExp.RightAsc (e, ZType.FocusedT ty)) -> 
      ZExp.FocusedE (HExp.Asc (e, ty))
    | (NextSib, ZExp.LeftAsc ((ZExp.FocusedE e), ty)) -> 
      ZExp.RightAsc (e, ZType.FocusedT ty)
    | (PrevSib, ZExp.RightAsc (e, ZType.FocusedT ty)) -> 
      ZExp.LeftAsc ((ZExp.FocusedE e), ty)
    (* Lambda *)
    | (FirstChild, ZExp.FocusedE (HExp.Lam (x, e))) -> 
      ZExp.LamZ (x, (ZExp.FocusedE e))
    | (Parent, ZExp.LamZ (x, ZExp.FocusedE e)) -> 
      ZExp.FocusedE (HExp.Lam (x, e))
    (* Application *)
    | (FirstChild, ZExp.FocusedE (HExp.Ap (e1, e2))) -> 
      ZExp.LeftAp ((ZExp.FocusedE e1), e2)
    | (Parent, ZExp.LeftAp (ZExp.FocusedE e1, e2)) -> 
      ZExp.FocusedE (HExp.Ap (e1, e2))
    | (Parent, ZExp.RightAp (e1, ZExp.FocusedE e2)) -> 
      ZExp.FocusedE (HExp.Ap (e1, e2))
    | (NextSib, ZExp.LeftAp (ZExp.FocusedE e1, e2)) -> 
      ZExp.RightAp (e1, ZExp.FocusedE e2)
    | (PrevSib, ZExp.RightAp (e1, ZExp.FocusedE e2)) -> 
      ZExp.LeftAp (ZExp.FocusedE e1, e2)
    (* Plus *)
    | (FirstChild, ZExp.FocusedE (HExp.Plus (e1, e2))) -> 
      ZExp.LeftPlus ((ZExp.FocusedE e1), e2)
    | (Parent, ZExp.LeftPlus (ZExp.FocusedE e1, e2)) -> 
      ZExp.FocusedE (HExp.Plus (e1, e2))
    | (Parent, ZExp.RightPlus (e1, ZExp.FocusedE e2)) -> 
      ZExp.FocusedE (HExp.Plus (e1, e2))
    | (NextSib, ZExp.LeftPlus (ZExp.FocusedE e1, e2)) -> 
      ZExp.RightPlus (e1, ZExp.FocusedE e2)
    | (PrevSib, ZExp.RightPlus (e1, ZExp.FocusedE e2)) -> 
      ZExp.LeftPlus (ZExp.FocusedE e1, e2)
    (* Non-Empty Hole *)
    | (FirstChild, ZExp.FocusedE (HExp.NonEmptyHole e)) -> 
      ZExp.NonEmptyHoleZ (ZExp.FocusedE e)
    | (Parent, ZExp.NonEmptyHoleZ (ZExp.FocusedE e)) -> 
      ZExp.FocusedE (HExp.NonEmptyHole e)
    (* Zipper Cases *)
    | (_, ZExp.LeftAsc (ze, ty)) -> 
      ZExp.LeftAsc ((performEMove direction ze), ty)
    | (_, ZExp.RightAsc (e, zty)) -> 
      ZExp.RightAsc (e, performTyp (Move direction) zty)
    | (_, ZExp.LamZ (x, ze)) -> 
      ZExp.LamZ (x, (performEMove direction ze))
    | (_, ZExp.LeftAp (ze1, e2)) -> 
      ZExp.LeftAp ((performEMove direction ze1), e2)
    | (_, ZExp.RightAp (e1, ze2)) -> 
      ZExp.RightAp (e1, (performEMove direction ze2))
    | (_, ZExp.LeftPlus (ze1, e2)) -> 
      ZExp.LeftPlus ((performEMove direction ze1), e2)
    | (_, ZExp.RightPlus (e1, ze2)) -> 
      ZExp.RightPlus (e1, (performEMove direction ze2))
    | (_, ZExp.NonEmptyHoleZ (ze)) -> 
      ZExp.NonEmptyHoleZ (performEMove direction ze)
    | _ -> raise InvalidAction

  let rec performSyn ctx a (ze, ty) = match (a, (ze, ty)) with 
    (* Movement *)
    | (Move direction, _) -> 
      ((performEMove direction ze), ty)
    (* Deletion *)
    | (Del, (ZExp.FocusedE e, _)) -> 
      ((ZExp.FocusedE HExp.EmptyHole), ty)
    (* Construction *)
    | (Construct SAsc, (ZExp.FocusedE e, _)) -> 
      (ZExp.RightAsc (e, ZType.FocusedT ty), ty)
    | (Construct (SVar x), (ZExp.FocusedE HExp.EmptyHole, HType.Hole)) -> 
      begin match Ctx.lookup ctx x with 
        | Some xty -> (ZExp.FocusedE (HExp.Var x), xty)
        | None -> raise InvalidAction
      end
    | (Construct (SLam x), (ZExp.FocusedE HExp.EmptyHole, HType.Hole)) -> 
      (ZExp.RightAsc (
          HExp.Lam (x, HExp.EmptyHole), 
          ZType.LeftArrow (ZType.FocusedT HType.Hole, HType.Hole)),
       HType.Arrow (HType.Hole, HType.Hole))
    | (Construct SAp, (ZExp.FocusedE e, HType.Arrow (ty1, ty2))) -> 
      (ZExp.RightAp (
          e,
          ZExp.FocusedE HExp.EmptyHole), 
       ty2)
    | (Construct SAp, (ZExp.FocusedE e, HType.Hole)) -> 
      (ZExp.RightAp (
          e, 
          ZExp.FocusedE HExp.EmptyHole), 
       HType.Hole)
    | (Construct SAp, (ZExp.FocusedE e, _)) -> 
      (ZExp.RightAp (
          HExp.NonEmptyHole e,
          ZExp.FocusedE HExp.EmptyHole), 
       HType.Hole)
    | (Construct SArg, (ZExp.FocusedE e, _)) -> 
      (ZExp.LeftAp (
          ZExp.FocusedE HExp.EmptyHole, 
          e), 
       HType.Hole)
    | (Construct (SNumLit n), (ZExp.FocusedE HExp.EmptyHole, HType.Hole)) -> 
      (ZExp.FocusedE (HExp.NumLit n), 
       HType.Num)
    | (Construct SPlus, (ZExp.FocusedE e, _)) -> 
      if HType.compat ty HType.Num then 
        (ZExp.RightPlus (e, ZExp.FocusedE HExp.EmptyHole), 
         HType.Num)
      else 
        (ZExp.RightPlus (HExp.NonEmptyHole e, ZExp.FocusedE HExp.EmptyHole),
         HType.Num)
    (* Finish *)
    | (Finish, (ZExp.FocusedE (HExp.NonEmptyHole e), HType.Hole)) -> 
      let ty' = HExp.syn ctx e in 
      (ZExp.FocusedE e, ty')
    (* Zipper Cases *)
    | (_, (ZExp.LeftAsc (ze, ty), _)) -> 
      (ZExp.LeftAsc ((performAna ctx a ze ty), ty), ty)
    | (_, (ZExp.RightAsc (e, zty), _)) -> 
      let zty' = performTyp a zty in 
      let ty' = ZType.erase zty' in 
      begin try 
          let _ = HExp.ana ctx e ty' in 
          (ZExp.RightAsc (e, zty'), ty')
        with 
        | HExp.IllTyped -> raise InvalidAction
      end
    | (_, (ZExp.LeftAp (ze1, e2), _)) -> 
      let e1 = ZExp.erase ze1 in 
      let ty1 = HExp.syn ctx e1 in 
      let (ze1', ty') = performSyn ctx a (ze1, ty1) in 
      begin match ty' with 
        | HType.Arrow (ty3, ty4) -> 
          let _ = HExp.ana ctx e2 ty3 in 
          (ZExp.LeftAp (ze1', e2), 
           ty4)
        | HType.Hole -> 
          let _ = HExp.ana ctx e2 HType.Hole in 
          (ZExp.LeftAp (ze1', e2), 
           HType.Hole)
        | _ -> raise InvalidAction
      end
    | (_, (ZExp.RightAp (e1, ze2), _)) -> 
      let ty1 = HExp.syn ctx e1 in 
      begin match ty1 with 
        | HType.Arrow (ty11, ty12) -> 
          let ze2' = performAna ctx a ze2 ty11 in 
          (ZExp.RightAp (e1, ze2'), 
           ty12)
        | HType.Hole -> 
          let ze2' = performAna ctx a ze2 HType.Hole in 
          (ZExp.RightAp (e1, ze2'), 
           HType.Hole)
        | _ -> raise InvalidAction
      end 
    | (_, (ZExp.LeftPlus (ze1, e2), _)) -> 
      let ze1' = performAna ctx a ze1 HType.Num in 
      (ZExp.LeftPlus (ze1', e2), 
       HType.Num)
    | (_, (ZExp.RightPlus (e1, ze2), _)) -> 
      let ze2' = performAna ctx a ze2 HType.Num in 
      (ZExp.RightPlus (e1, ze2'), 
       HType.Num)
    | (_, (ZExp.NonEmptyHoleZ ze1, _)) -> 
      let e1 = ZExp.erase ze1 in 
      let ty1 = HExp.syn ctx e1 in 
      let (ze1', _) = performSyn ctx a (ze1, ty1) in 
      begin match ze1' with 
        | ZExp.FocusedE HExp.EmptyHole ->
          (ze1', HType.Hole)
        | _ -> 
          (ZExp.NonEmptyHoleZ ze1', 
           HType.Hole)
      end
    | _ -> raise InvalidAction
  and performAna ctx a ze ty = match (a, ze, ty) with 
    (* Move *)
    | (Move direction, _, _) -> 
      performEMove direction ze
    (* Deletion *)
    | (Del, ZExp.FocusedE e, _) -> 
      ZExp.FocusedE HExp.EmptyHole
    (* Construction *)
    | (Construct SAsc, ZExp.FocusedE e, _) -> 
      ZExp.RightAsc (e, ZType.FocusedT ty)
    | (Construct (SVar x), ZExp.FocusedE HExp.EmptyHole, _) -> 
      begin match Ctx.lookup ctx x with 
        | Some xty -> 
          if HType.compat ty xty then 
            ZExp.FocusedE (HExp.Var x) 
          else
            ZExp.FocusedE (HExp.NonEmptyHole (HExp.Var x))
        | None -> raise InvalidAction 
      end
    | (Construct (SLam x), ZExp.FocusedE HExp.EmptyHole, HType.Arrow (ty1, ty2)) -> 
      ZExp.LamZ (x, ze)
    | (Construct (SLam x), ZExp.FocusedE HExp.EmptyHole, _) -> 
      ZExp.NonEmptyHoleZ (
        ZExp.RightAsc (
          HExp.Lam (x, HExp.EmptyHole),
          ZType.LeftArrow (ZType.FocusedT HType.Hole, HType.Hole)
        ))
    | (Construct (SNumLit n), ZExp.FocusedE HExp.EmptyHole, HType.Num) -> 
      ZExp.FocusedE (HExp.NumLit n)
    | (Construct (SNumLit n), ZExp.FocusedE HExp.EmptyHole, _) -> 
      ZExp.NonEmptyHoleZ (
        ZExp.FocusedE (HExp.NumLit n))
    (* Finishing *)
    | (Finish, ZExp.FocusedE (HExp.NonEmptyHole e), _) -> 
      let _ = HExp.ana ctx e ty in 
      ZExp.FocusedE e
    (* Zipper Cases *)
    | (_, ZExp.LamZ (x, ze'), HType.Arrow (ty1, ty2)) -> 
      let ctx' = Ctx.extend ctx (x, ty1) in 
      let ze'' = performAna ctx' a ze' ty2 in 
      ZExp.LamZ (x, ze'') 
    (* Subsumption *) 
    | _ -> 
      let e = ZExp.erase ze in 
      let ty1 = HExp.syn ctx e in 
      let (ze', ty1') = performSyn ctx a (ze, ty1) in 
      if HType.compat ty ty1' then 
        ze'
      else 
        raise InvalidAction
end

