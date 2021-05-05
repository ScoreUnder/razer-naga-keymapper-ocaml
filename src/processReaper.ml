open MyLib

let procs_need_reaping = Atomic.make IntSet.empty

let rec atomic_cas_f f a =
  let curr = Atomic.get a in
  let success = Atomic.compare_and_set a curr (f curr) in
  if success then curr else atomic_cas_f f a

let setup () =
  let chld_handler signal =
    if signal = Sys.sigchld then
      let procs_reaped =
        IntSet.fold
          (fun pid acc ->
            let p, _ = Unix.waitpid [ Unix.WNOHANG ] pid in
            if p = pid then IntSet.add pid acc else acc)
          (Atomic.get procs_need_reaping)
          IntSet.empty
      in
      atomic_cas_f (fun set -> IntSet.diff set procs_reaped) procs_need_reaping
      |> ignore
  in
  Sys.signal Sys.sigchld (Sys.Signal_handle chld_handler) |> ignore

let register_for_reaping pid =
  atomic_cas_f (IntSet.add pid) procs_need_reaping |> ignore
