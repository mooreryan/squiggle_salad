open! Base

(* I keep switching Base/Core, so here is some compatibility functions. *)
module Compat = struct
  module In_channel = Stdio.In_channel

  let eprintf = Stdio.eprintf

  let exit = Caml.exit

  let file_exists = Caml.Sys.file_exists
end

include Compat

let abort ?(exit_code = 1) msg =
  let () = eprintf "%s\n" msg in
  exit exit_code

let abort_unless_file_exists fname =
  if not (file_exists fname) then
    abort [%string "error: file '%{fname}' does not exist"]

let with_file_fold_lines ~f ~init fname =
  In_channel.with_file fname ~f:(fun ic -> In_channel.fold_lines ic ~f ~init)

let with_file_iter_lines ~f fname =
  In_channel.with_file fname ~f:(fun ic -> In_channel.iter_lines ic ~f)

let tally collection ~comp ~fold =
  fold collection ~init:(Map.empty comp) ~f:(fun counts s ->
      Map.update counts s ~f:(function None -> 1 | Some x -> x + 1))
