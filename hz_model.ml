module Model = struct
  type t = ZExp.t * HType.t

  (* let empty = (HType.Arrow ((HType.Hole),(HType.Arrow ((HType.Num 1),(HType.Num 2)))))    *)
  (* let empty = Lam ((Var "x"),InProgressHole (Plus (NumLit 1, NumLit 3))) *)
  (* let empty = (FocusedE (Plus (NumLit 1, NumLit 3))),(Num) *)
  (* let empty = (ZExp.LeftPlus ((ZExp.FocusedE (NumLit 8)), (NumLit 7))), HType.Num *)
  let empty = ((ZExp.FocusedE HExp.EmptyHole), HType.Hole)

  (* react *)
  type rs = t React.signal
  type rf = ?step:React.step -> t -> unit
  type rp = rs * rf
end

