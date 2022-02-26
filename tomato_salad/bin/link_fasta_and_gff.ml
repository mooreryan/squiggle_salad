open! Base
module U = Tomato_salad.Utils

let print_endline = Stdio.print_endline

let exit = Caml.exit

module Cli = struct
  open Cmdliner

  type opts = { fasta : string; gff : string }

  let make_opts fasta gff = { fasta; gff }

  let fasta_term =
    let doc = "Path to fasta file" in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"FASTA" ~doc)

  let gff_term =
    let doc = "Path to gff3 file" in
    Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"GFF3" ~doc)

  let info =
    let doc = "Fix the UniProt GFF3 files so they match the FASTA file" in
    let man =
      [
        `S Manpage.s_description;
        `P "TODO";
        `S Manpage.s_examples;
        `Pre "  \\$ link_fasta_and_gff seqs.faa seqs.gff3 > seqs_linked.gff3";
      ]
    in
    Cmd.info "link_fasta_and_gff" ~doc ~man

  let term = Term.(const make_opts $ fasta_term $ gff_term)

  let parse () =
    match Cmd.eval_value @@ Cmd.v info term with
    | Ok (`Ok opts) -> `Run opts
    | Ok `Help | Ok `Version -> `Exit 0
    | Error _ -> `Exit 1
end

let get_fasta_ids fname =
  let open Bio_io.Fasta in
  In_channel.with_file_fold_records_exn fname
    ~init:(Map.empty (module String))
    ~f:(fun ids r ->
      let full_id = Record.id r in
      match String.split ~on:'|' full_id with
      | [ _; short_id; _ ] -> Map.add_exn ids ~key:short_id ~data:full_id
      | _ ->
          U.abort [%string "ERROR: Bad sequence ID in FASTA file: '%{full_id}'"])

let process_gff fname ids =
  let handle_data_line line =
    match String.split ~on:'\t' line with
    | id :: rest ->
        let new_id =
          match Map.find ids id with
          | None ->
              U.abort
                [%string
                  "ERROR: '%{id}' is present in the GFF file but not the FASTA \
                   file"]
          | Some id -> id
        in
        let rest = String.concat ~sep:"\t" rest in
        print_endline [%string "%{new_id}\t%{rest}"]
    | _ -> U.abort [%string "ERROR: Bad line in GFF file: '%{line}'"]
  in
  (* Headers are a bit different...they can have the IDs scattered in the line,
     but we still need to map them. *)
  let handle_header_line line =
    let try_map_id token =
      match Map.find ids token with None -> token | Some new_id -> new_id
    in
    line |> String.split ~on:' ' |> List.map ~f:try_map_id
    |> String.concat ~sep:" " |> print_endline
  in

  U.with_file_iter_lines fname ~f:(fun line ->
      if String.is_prefix line ~prefix:"#" then handle_header_line line
      else handle_data_line line)

let () =
  let opts : Cli.opts =
    match Cli.parse () with `Run file -> file | `Exit code -> exit code
  in
  let ids = get_fasta_ids opts.fasta in
  process_gff opts.gff ids
