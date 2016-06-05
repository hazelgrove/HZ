module Controller = struct

  open Hz_model
  (*   open Action
       open Model.ZExp
       open Model.HExp
       open Model *)

  exception Exception
  let update a ((rs, rf) : rp) =
    let mOld = React.S.value rs in
    let m = (Action.performSyn mOld a) in
    rf m

end
