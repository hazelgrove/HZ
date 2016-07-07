module Controller = struct

  open Hz_semantics
  open Hz_model.Model

  (* updates the model stream by performing a new action *)
  let update a ((rs, rf) : rp) =
    let mOld = React.S.value rs in
    let m = (Action.performSyn Ctx.empty a mOld) in
    rf m
end
