open Hz_semantics
open Lwt.Infix


module Model = struct
  type t = ZExp.t * HType.t

  let empty = ((ZExp.FocusedE HExp.EmptyHole), HType.Hole)

  (* react *)
  type rs = t React.signal
  type rf = ?step:React.step -> t -> unit
  type rp = rs * rf

end


