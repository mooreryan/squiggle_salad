open! Base
module U = Tomato_salad.Utils

let print_endline = Stdio.print_endline

let exit = Caml.exit

module Cli = struct
  let version = "0.1.0"

  open Cmdliner

  type opts = { ids : string; data : string }

  let make_opts ids data = { ids; data }

  let ids_term =
    let doc = "Path to IDs file.  Should be a single column of IDs to match." in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"IDS" ~doc)

  let data_term =
    let doc =
      "Path to data file.  Will match first whitespace separated column \
       against IDs."
    in
    Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"DATA" ~doc)

  let info =
    let doc = "Pick and print lines in a data file that match given IDs." in
    let man =
      [
        (* `S Manpage.s_description;
         * `P "TODO"; *)
        `S Manpage.s_examples;
        `Pre "  \\$ pick_lines ids.txt data.txt > matching_data.txt";
      ]
    in
    Cmd.info "pick_lines" ~doc ~man ~version

  let term = Term.(const make_opts $ ids_term $ data_term)

  let parse () =
    match Cmd.eval_value @@ Cmd.v info term with
    | Ok (`Ok opts) -> `Run opts
    | Ok `Help | Ok `Version -> `Exit 0
    | Error _ -> `Exit 1
end

let get_ids fname =
  U.with_file_fold_lines fname ~init:(Set.empty (module String)) ~f:Set.add

let process_data fname ids =
  let handle_data_line line =
    match String.split_on_chars ~on:[ '\t'; ' ' ] line with
    | [ id ] | id :: _ -> if Set.mem ids id then print_endline line
    | [] -> assert false
  in
  U.with_file_iter_lines fname ~f:handle_data_line

let () =
  let opts : Cli.opts =
    match Cli.parse () with `Run file -> file | `Exit code -> exit code
  in
  let ids = get_ids opts.ids in
  process_data opts.data ids
