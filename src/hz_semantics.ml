(* Comments below indicate the corresponding rule numbers from the supplement. *)

module HTyp = struct 
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
      | (Hole, _) (* 1a *) -> true
      | (_, Hole) (* 1b *) -> true
      | (Arrow (ty1_left, ty1_right), Arrow (ty2_left, ty2_right)) (* 1d *) -> 
        (compat ty1_left ty1_right) && (compat ty2_left ty2_right)
      | _ -> eq ty1 ty2 (* 1c *)
    )

  let incompat ty1 ty2 = not (compat ty1 ty2) (* see text *)

  let matched_arrow ty = match ty with 
    | Arrow (ty1, ty2) (* 3a *) -> Some (ty1, ty2)
    | Hole (* 3b *) -> Some (Hole, Hole)
    | _ -> None

  let has_matched_arrow ty = match matched_arrow ty with 
    | Some _ -> true
    | None -> false

  let rec complete ty = match ty with 
    | Num (* 6a *) -> true
    | Arrow (ty1, ty2) (* 6b *) -> (complete ty1) && (complete ty2)
    | Hole -> false
end

module Var = struct
  type t = string
end

module Ctx : sig
  type t
  val empty : t
  val extend : t -> Var.t * HTyp.t -> t
  val lookup : t -> Var.t -> HTyp.t option
end = struct
  type t = (Var.t * HTyp.t) list

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
    | Asc of t * HTyp.t 
    | Var of Var.t
    | Lam of Var.t * t
    | Ap of t * t 
    | NumLit of int
    | Plus of t * t
    | EmptyHole 
    | NonEmptyHole of t

  exception IllTyped

  let rec syn ctx e = match e with 
    | Asc (e', ty) (* 5a *) -> let _ = ana ctx e' ty in ty
    | Var x (* 5b *) -> 
      begin match (Ctx.lookup ctx x) with 
        | Some ty -> ty
        | None -> raise IllTyped
      end
    | Ap (e1, e2) (* 5c *) -> 
      let ty1 = syn ctx e1 in 
      begin match HTyp.matched_arrow ty1 with 
        | Some (ty1_left, ty1_right) -> 
          let _ = ana ctx e2 ty1_left in 
          ty1_right
        | _ -> raise IllTyped
      end
    | NumLit _ (* 5d *) -> HTyp.Num
    | Plus (e1, e2) (* 5e *) -> 
      let _ = ana ctx e1 HTyp.Num in 
      let _ = ana ctx e2 HTyp.Num in 
      HTyp.Num
    | EmptyHole (* 5f *)  -> HTyp.Hole
    | NonEmptyHole e' (* 5g *) -> 
      let _ = syn ctx e' in 
      HTyp.Hole
    | _ -> raise IllTyped
  and ana ctx e ty = match e with 
    | Lam (x, e') (* 4a *) -> 
      begin match HTyp.matched_arrow ty with 
        | Some (ty1, ty2) -> 
          let ctx' = Ctx.extend ctx (x, ty1) in 
          ana ctx' e' ty2
        | _ -> raise IllTyped
      end 
    | _ (* 4b *) -> (* subsumption *) 
      let ty' = syn ctx e in 
      if HTyp.compat ty ty' then ()
      else raise IllTyped

  let rec complete e = match e with 
    | Asc (e', ty) (* 7a *) -> (complete e') && (HTyp.complete ty)
    | Var _ (* 7b *) -> true
    | Lam (_, e') (* 7c *) -> complete e'
    | Ap (e1, e2) (* 7d *) -> (complete e1) && (complete e2)
    | NumLit _ -> (* 7e *) true
    | Plus (e1, e2) (* 7f *) -> (complete e1) && (complete e2)
    | EmptyHole -> false
    | NonEmptyHole _ -> false
end

module ZTyp = struct
  type t = 
    | CursorT of HTyp.t
    | LeftArrow of t * HTyp.t
    | RightArrow of HTyp.t * t 

  let rec erase zty = match zty with 
    | CursorT ty (* 8a *) -> ty
    | LeftArrow (zty1, ty2) (* 8b *) -> HTyp.Arrow ((erase zty1), ty2)
    | RightArrow (ty1, zty2) (* 8c *) -> HTyp.Arrow (ty1, (erase zty2))
end

module ZExp = struct
  type t = 
    | CursorE of HExp.t
    | LeftAsc of t * HTyp.t
    | RightAsc of HExp.t * ZTyp.t
    | LamZ of Var.t * t
    | LeftAp of t * HExp.t
    | RightAp of HExp.t * t 
    | LeftPlus of t * HExp.t
    | RightPlus of HExp.t * t
    | NonEmptyHoleZ of t

  let rec erase ze = match ze with 
    | CursorE e (* 9a *) -> e
    | LeftAsc (ze', ty) (* 9b *) -> HExp.Asc ((erase ze'), ty)
    | RightAsc (e', zty) (* 9c *) -> HExp.Asc (e', (ZTyp.erase zty))
    | LamZ (x, ze') (* 9d *) -> HExp.Lam (x, (erase ze'))
    | LeftAp (ze', e) (* 9e *) -> HExp.Ap ((erase ze'), e)
    | RightAp (e, ze') (* 9f *) -> HExp.Ap (e, (erase ze'))
    | LeftPlus (ze', e) (* 9g *) -> HExp.Plus ((erase ze'), e)
    | RightPlus (e, ze') (* 9h *) -> HExp.Plus (e, (erase ze'))
    | NonEmptyHoleZ ze' (* 9i *) -> HExp.NonEmptyHole (erase ze')
end

module Action = struct
  (* we also have PrevSib for possible future use *)
  type direction =  
    | FirstChild 
    | Parent 
    | NextSib 
    | PrevSib (* for convenience, not in paper *)

  type shape = 
    | SArrow
    | SNum
    | SAsc
    | SVar of Var.t
    | SLam of Var.t
    | SAp 
    | SArg
    | SLit of int
    | SPlus
    | SNEHole 

  type t =
    | Move of direction
    | Del 
    | Construct of shape
    | Finish

  exception InvalidAction
  exception Impossible

  let rec performTyp a zty = match (a, zty) with 
    (* Movement *)
    | (Move FirstChild, ZTyp.CursorT (HTyp.Arrow (ty1, ty2))) (* 10a *) -> 
      ZTyp.LeftArrow ((ZTyp.CursorT ty1), ty2)
    | (Move Parent, ZTyp.LeftArrow ((ZTyp.CursorT ty1), ty2)) (* 10b *) -> 
      ZTyp.CursorT (HTyp.Arrow (ty1, ty2))
    | (Move Parent, ZTyp.RightArrow (ty1, ZTyp.CursorT ty2)) (* 10c *) -> 
      ZTyp.CursorT (HTyp.Arrow (ty1, ty2))
    | (Move NextSib, ZTyp.LeftArrow ((ZTyp.CursorT ty1), ty2)) (* 10d *) -> 
      ZTyp.RightArrow (ty1, ZTyp.CursorT ty2)
    | (Move PrevSib, ZTyp.RightArrow (ty1, ZTyp.CursorT ty2)) (* (prevSib not in paper) *) -> 
      ZTyp.LeftArrow (ZTyp.CursorT ty1, ty2)
    (* Del *)
    | (Del, ZTyp.CursorT ty) (* 10e *) -> 
      ZTyp.CursorT (HTyp.Hole)
    (* Construct *)
    | (Construct SArrow, ZTyp.CursorT ty) (* 10f *) -> 
      ZTyp.RightArrow (ty, ZTyp.CursorT HTyp.Hole)
    | (Construct SNum, ZTyp.CursorT HTyp.Hole) (* 10g *) -> 
      ZTyp.CursorT HTyp.Num
    (* Zipper Rules *)
    | (_, ZTyp.LeftArrow (zty1, ty2)) (* 10h *) -> 
      let zty1' = performTyp a zty1 in 
      ZTyp.LeftArrow (zty1', ty2)
    | (_, ZTyp.RightArrow (ty1, zty2)) (* 10i *) -> 
      let zty2' = performTyp a zty2 in 
      ZTyp.RightArrow (ty1, zty2')
    | _ -> 
      raise InvalidAction

  let rec performEMove action ze = match action with 
    | Move direction -> 
      begin match (direction, ze) with 
        (* Ascription *)
        | (FirstChild, ZExp.CursorE (HExp.Asc (e, ty))) (* 11a *) -> 
          ZExp.LeftAsc ((ZExp.CursorE e), ty)
        | (Parent, ZExp.LeftAsc ((ZExp.CursorE e), ty)) (* 11b *) -> 
          ZExp.CursorE (HExp.Asc (e, ty))
        | (Parent, ZExp.RightAsc (e, ZTyp.CursorT ty)) (* 11c *) -> 
          ZExp.CursorE (HExp.Asc (e, ty))
        | (NextSib, ZExp.LeftAsc ((ZExp.CursorE e), ty)) (* 11d *) -> 
          ZExp.RightAsc (e, ZTyp.CursorT ty)
        | (PrevSib, ZExp.RightAsc (e, ZTyp.CursorT ty)) (* prevSib not in paper *) -> 
          ZExp.LeftAsc ((ZExp.CursorE e), ty)
        (* Lambda *)
        | (FirstChild, ZExp.CursorE (HExp.Lam (x, e))) (* 11e *) -> 
          ZExp.LamZ (x, (ZExp.CursorE e))
        | (Parent, ZExp.LamZ (x, ZExp.CursorE e)) (* 11f *) -> 
          ZExp.CursorE (HExp.Lam (x, e))
        (* Application *)
        | (FirstChild, ZExp.CursorE (HExp.Ap (e1, e2))) (* 11g *) -> 
          ZExp.LeftAp ((ZExp.CursorE e1), e2)
        | (Parent, ZExp.LeftAp (ZExp.CursorE e1, e2)) (* 11h *) -> 
          ZExp.CursorE (HExp.Ap (e1, e2))
        | (Parent, ZExp.RightAp (e1, ZExp.CursorE e2)) (* 11i *) -> 
          ZExp.CursorE (HExp.Ap (e1, e2))
        | (NextSib, ZExp.LeftAp (ZExp.CursorE e1, e2)) (* 11j *) -> 
          ZExp.RightAp (e1, ZExp.CursorE e2)
        | (PrevSib, ZExp.RightAp (e1, ZExp.CursorE e2)) (* prevSib not in paper *) -> 
          ZExp.LeftAp (ZExp.CursorE e1, e2)
        (* Plus *)
        | (FirstChild, ZExp.CursorE (HExp.Plus (e1, e2))) (* 11k *) -> 
          ZExp.LeftPlus ((ZExp.CursorE e1), e2)
        | (Parent, ZExp.LeftPlus (ZExp.CursorE e1, e2)) (* 11l *) -> 
          ZExp.CursorE (HExp.Plus (e1, e2))
        | (Parent, ZExp.RightPlus (e1, ZExp.CursorE e2)) (* 11m *) -> 
          ZExp.CursorE (HExp.Plus (e1, e2))
        | (NextSib, ZExp.LeftPlus (ZExp.CursorE e1, e2)) (* 11n *) -> 
          ZExp.RightPlus (e1, ZExp.CursorE e2)
        | (PrevSib, ZExp.RightPlus (e1, ZExp.CursorE e2)) (* prevSib not in paper *) -> 
          ZExp.LeftPlus (ZExp.CursorE e1, e2)
        (* Non-Empty Hole *)
        | (FirstChild, ZExp.CursorE (HExp.NonEmptyHole e)) (* 11o *) -> 
          ZExp.NonEmptyHoleZ (ZExp.CursorE e)
        | (Parent, ZExp.NonEmptyHoleZ (ZExp.CursorE e)) (* 11p *) -> 
          ZExp.CursorE (HExp.NonEmptyHole e)
        | _ -> raise InvalidAction
      end
    | _ -> raise InvalidAction 

  let rec performSyn ctx a (ze, ty) = 
    try 
      (performEMove a ze, ty) (* 12a *)
    with InvalidAction -> 
      begin match (a, (ze, ty)) with 
        (* Deletion *)
        | (Del, (ZExp.CursorE e, _)) (* 12b *) -> 
          ((ZExp.CursorE HExp.EmptyHole), HTyp.Hole)
        (* Construction *)
        | (Construct SAsc, (ZExp.CursorE e, ty)) (* 12c *) -> 
          (ZExp.RightAsc (e, ZTyp.CursorT ty), ty)
        | (Construct (SVar x), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* 12d *) -> 
          begin match Ctx.lookup ctx x with 
            | Some xty -> (ZExp.CursorE (HExp.Var x), xty)
            | None -> raise InvalidAction
          end
        | (Construct (SLam x), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* 12e *) -> 
          (ZExp.RightAsc (
              HExp.Lam (x, HExp.EmptyHole), 
              ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole, HTyp.Hole)),
           HTyp.Arrow (HTyp.Hole, HTyp.Hole))
        | (Construct SAp, (ZExp.CursorE e, ty)) -> 
          begin match HTyp.matched_arrow ty with 
            | Some (_, ty2) (* 12f *) -> (ZExp.RightAp (
                e,
                ZExp.CursorE HExp.EmptyHole), 
                                          ty2)
            | None (* 12g *) -> (ZExp.RightAp (
                HExp.NonEmptyHole e,
                ZExp.CursorE HExp.EmptyHole), 
                                 HTyp.Hole)
          end
        | (Construct SArg, (ZExp.CursorE e, _)) (* 12h *) -> 
          (ZExp.LeftAp (
              ZExp.CursorE HExp.EmptyHole, 
              e), 
           HTyp.Hole)
        | (Construct (SLit n), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) (* 12i *) -> 
          (ZExp.CursorE (HExp.NumLit n), 
           HTyp.Num)
        | (Construct SPlus, (ZExp.CursorE e, _)) -> 
          if HTyp.compat ty HTyp.Num (* 12j *) then 
            (ZExp.RightPlus (e, ZExp.CursorE HExp.EmptyHole), 
             HTyp.Num)
          else (* 12k *)
            (ZExp.RightPlus (HExp.NonEmptyHole e, ZExp.CursorE HExp.EmptyHole),
             HTyp.Num)
        | (Construct SNEHole, (ZExp.CursorE e', ty)) (* 12l *) -> 
          (ZExp.NonEmptyHoleZ (ZExp.CursorE e'), HTyp.Hole)
        (* Finish *)
        | (Finish, (ZExp.CursorE (HExp.NonEmptyHole e), HTyp.Hole)) (* 12m *) -> 
          let ty' = HExp.syn ctx e in 
          (ZExp.CursorE e, ty')
        (* Zipper Cases *)
        | (a, (ZExp.LeftAsc (ze, ty), _)) (* 12n *) -> 
          (ZExp.LeftAsc ((performAna ctx a ze ty), ty), ty)
        | (a, (ZExp.RightAsc (e, zty), _)) (* 12o *) -> 
          let zty' = performTyp a zty in 
          let ty' = ZTyp.erase zty' in 
          begin try 
              let _ = HExp.ana ctx e ty' in 
              (ZExp.RightAsc (e, zty'), ty')
            with 
            | HExp.IllTyped -> raise InvalidAction
          end
        | (_, (ZExp.LeftAp (ze1, e2), _)) (* 12p *) -> 
          let e1 = ZExp.erase ze1 in 
          let ty2 = HExp.syn ctx e1 in 
          let (ze1', ty3) = performSyn ctx a (ze1, ty2) in 
          begin match HTyp.matched_arrow ty3 with 
            | Some (ty4, ty5) -> 
              let _ = HExp.ana ctx e2 ty4 in 
              (ZExp.LeftAp (ze1', e2), 
               ty5)
            | None -> raise InvalidAction
          end
        | (_, (ZExp.RightAp (e1, ze2), _)) (* 12q *) -> 
          let ty2 = HExp.syn ctx e1 in 
          begin match HTyp.matched_arrow ty2 with 
            | Some (ty3, ty4) -> 
              let ze2' = performAna ctx a ze2 ty3 in 
              (ZExp.RightAp (e1, ze2'),
               ty4)
            | None -> raise InvalidAction
          end
        | (_, (ZExp.LeftPlus (ze1, e2), _)) (* 12r *) -> 
          let ze1' = performAna ctx a ze1 HTyp.Num in 
          (ZExp.LeftPlus (ze1', e2), 
           HTyp.Num)
        | (_, (ZExp.RightPlus (e1, ze2), _)) (* 12s *) -> 
          let ze2' = performAna ctx a ze2 HTyp.Num in 
          (ZExp.RightPlus (e1, ze2'), 
           HTyp.Num)
        | (_, (ZExp.NonEmptyHoleZ ze1, _)) (* 12t *) -> 
          let e1 = ZExp.erase ze1 in 
          let ty1 = HExp.syn ctx e1 in 
          let (ze1', _) = performSyn ctx a (ze1, ty1) in 
          (ZExp.NonEmptyHoleZ ze1', HTyp.Hole)
        | _ -> raise InvalidAction
      end 
  and performAna ctx a ze ty = match a with 
    | Move _ (* 13b *) -> 
      (* try to use the non-zipper move actions *)
      begin try 
          performEMove a ze
        with InvalidAction -> 
          (* if it doesn't work, keep going --
           * it'll hit the subsumption rule at the bottom *)
          performAna_postMoveCheck ctx a ze ty
      end
    | _ -> performAna_postMoveCheck ctx a ze ty 
  and performAna_postMoveCheck ctx a ze ty = 
    match (a, ze, ty) with 
    (* Deletion *)
    | (Del, ZExp.CursorE e, _) (* 13c *) -> 
      ZExp.CursorE HExp.EmptyHole
    (* Construction *)
    | (Construct SAsc, ZExp.CursorE e, _) (* 13d *) -> 
      ZExp.RightAsc (e, ZTyp.CursorT ty)
    | (Construct (SVar x), ZExp.CursorE HExp.EmptyHole, ty) when begin 
      match Ctx.lookup ctx x with 
      | Some xty -> HTyp.incompat ty xty
      | None -> false end (* 13e *) -> 
      ZExp.CursorE (HExp.NonEmptyHole (HExp.Var x))
    | (Construct (SLam x), ZExp.CursorE HExp.EmptyHole, ty) -> 
      begin match HTyp.matched_arrow ty with 
        | Some _ (* 13f *) -> ZExp.LamZ (x, ze)
        | None (* 13g *) -> ZExp.NonEmptyHoleZ (
            ZExp.RightAsc (
              HExp.Lam (x, HExp.EmptyHole),
              ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole, HTyp.Hole)
            ))
      end
    | (Construct SLit n, ZExp.CursorE HExp.EmptyHole, ty) when HTyp.incompat ty HTyp.Num (* 13h *) -> 
      ZExp.NonEmptyHoleZ (ZExp.CursorE (HExp.NumLit n))
    (* Finishing *)
    | (Finish, ZExp.CursorE (HExp.NonEmptyHole e), _) (* 13i *) -> 
      let _ = HExp.ana ctx e ty in 
      ZExp.CursorE e
    (* Zipper Cases *)
    | (_, ZExp.LamZ (x, ze'), ty) (* 13j *) -> 
      begin match HTyp.matched_arrow ty with 
        | Some (ty1, ty2) -> 
          let ctx' = Ctx.extend ctx (x, ty1) in 
          let ze'' = performAna ctx' a ze' ty2 in 
          ZExp.LamZ (x, ze'') 
        | None -> raise InvalidAction
      end
    (* Subsumption *) 
    | _ (* 13a *) -> 
      let e = ZExp.erase ze in 
      let ty1 = HExp.syn ctx e in 
      let (ze', ty1') = performSyn ctx a (ze, ty1) in 
      if HTyp.compat ty ty1' then 
        ze'
      else 
        raise InvalidAction
end

