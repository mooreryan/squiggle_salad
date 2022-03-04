open! Base
module U = Tomato_salad.Utils
module Oc = Stdio.Out_channel

module Cli = struct
  let version = "0.1.0"

  open Cmdliner

  type opts = { hit_info : string; btab : string }

  let make_opts hit_info btab = { hit_info; btab }

  let info_term =
    let doc =
      "Path to target/hit info file.  Should be [id TAB info].  Repeated IDs \
       in the info file trigger exceptions."
    in
    Arg.(
      required & pos 0 (some non_dir_file) None & info [] ~docv:"HIT_INFO" ~doc)

  let btab_term =
    let doc = "Path to btab file." in
    Arg.(required & pos 1 (some non_dir_file) None & info [] ~docv:"BTAB" ~doc)

  let info =
    let doc = "Get info from a btab file" in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Get hit info from a btab file, but also print out some info about \
           the hits.";
        `S Manpage.s_examples;
        `Pre "  \\$ btab_hit_info hit_info.tsv btab.tsv > query_hit_info.txt";
      ]
    in
    Cmd.info "pick_lines" ~doc ~man ~version

  let term = Term.(const make_opts $ info_term $ btab_term)

  let parse () =
    match Cmd.eval_value @@ Cmd.v info term with
    | Ok (`Ok opts) -> `Run opts
    | Ok `Help | Ok `Version -> `Exit 0
    | Error _ -> `Exit 1
end

module Scores = struct
  type t = float list Map.M(String).t

  let init target target_bits =
    Map.of_alist_exn (module String) [ (target, [ target_bits ]) ]

  let update t target target_bits =
    Map.update t target ~f:(function
      | None -> [ target_bits ]
      | Some scores -> target_bits :: scores)
end

type query_info = { scores : Scores.t; total_bits : float; total_hits : int }

module Hit_info = struct
  type t = string Map.M(String).t

  let read_file fname : t =
    U.with_file_fold_lines fname
      ~init:(Map.empty (module String))
      ~f:(fun ids line ->
        match String.split ~on:'\t' line with
        | [ id; info ] -> Map.add_exn ids ~key:id ~data:info
        | _ -> U.abort [%string "ERROR: Bad info line: '%{line}'"])
end

let process_btab fname target_info =
  let open Bio_io.Mmseqs in
  Oc.prerr_endline "LOG -- Reading btab";
  let queries =
    In_channel.with_file_foldi_records_exn fname
      ~init:(Map.empty (module String))
      ~f:(fun i queries r ->
        if Int.(i > 0 && i % 200000 = 0) then (
          Oc.fprintf Oc.stderr "Btab line: %d\r" i;
          Oc.flush Oc.stderr);
        (* Use the info instead of the actual target if it is present. *)
        let target =
          Option.value ~default:r.target @@ Map.find target_info r.target
        in
        Map.update queries r.query ~f:(function
          | None ->
              let scores = Scores.init target r.bits in
              { scores; total_bits = r.bits; total_hits = 1 }
          | Some { scores; total_bits; total_hits } ->
              let scores = Scores.update scores target r.bits in
              {
                scores;
                total_bits = total_bits +. r.bits;
                total_hits = total_hits + 1;
              }))
  in
  Oc.prerr_endline "LOG -- Printing results";
  Map.iteri queries
    ~f:(fun ~key:query ~data:{ scores; total_bits; total_hits } ->
      Map.iteri scores ~f:(fun ~key:target ~data:scores ->
          let target_hits, target_bits =
            List.fold scores ~init:(0, 0.) ~f:(fun (num_hits, bits) this ->
                (num_hits + 1, bits +. this))
          in
          let target_bit_prop = target_bits /. total_bits in
          let target_hit_prop =
            Float.(of_int target_hits / of_int total_hits)
          in
          Stdio.printf "%s\t%s\t%.0f\t%.3f\t%d\t%.3f\n" query target target_bits
            target_bit_prop target_hits target_hit_prop))

let () =
  let opts : Cli.opts =
    match Cli.parse () with `Run file -> file | `Exit code -> Caml.exit code
  in
  Oc.prerr_endline "LOG -- Reading hit info";
  let hit_info = Hit_info.read_file opts.hit_info in
  process_btab opts.btab hit_info
