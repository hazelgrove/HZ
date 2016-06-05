open Hz_semantics
open Lwt.Infix

module Model = struct
  type t = ZExp.t * HType.t

  (* let empty = ((ZExp.FocusedE HExp.EmptyHole), HType.Hole) *)
  let empty = (ZExp.FocusedE (HExp.Plus (HExp.EmptyHole,HExp.EmptyHole)), 
               HType.Num)

  (* react *)
  type rs = t React.signal (* reactive signal *)
  type rf = ?step:React.step -> t -> unit (* update function *)
  type rp = rs * rf (* pair of rs and rf *)
end


