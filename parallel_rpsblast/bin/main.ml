open! Core

let file_exists = Sys.file_exists_exn ~follow_symlinks:true

module Opts = struct
  type t = {
    query : string;
    db : string;
    outdir : string;
    basename : string;
    force : bool;
    rpsblast : string;
    evalue : float;
    num_threads : int;
    extra_config : string option;
    verbosity : Little_logger.Logger.Level.t;
    max_tries : int;
  }

  let make query db outdir basename force rpsblast evalue num_threads
      extra_config verbosity max_tries =
    {
      query;
      db;
      outdir;
      basename;
      force;
      rpsblast;
      evalue;
      num_threads;
      extra_config;
      verbosity;
      max_tries;
    }

  (* There are some things that are kind of annoying to check with cmdliner, so
     check any other imprtant stuff about the opts here for now. Eventually, I
     will move these into the arg parsing. *)
  let check opts =
    let check_outdir opts =
      if
        (not opts.force)
        && Sys.file_exists_exn ~follow_symlinks:true opts.outdir
      then failwith "outdir exists but --force was not passed in"
    in
    let check_query opts =
      if not (file_exists opts.query) then failwith "query file does not exist"
    in
    (* TODO [check_db] get a glob based on the name, then check if there are DB
       files there. *)
    let check_num_threads opts =
      if opts.num_threads < 1 then failwith "num_threads should be > 0"
    in
    let check_max_tries opts =
      if opts.max_tries < 0 then failwith "num_threads should be >= 0"
    in
    check_outdir opts;
    check_query opts;
    check_num_threads opts;
    check_max_tries opts;
    ()
end

module Cli : sig
  val parse_cli : unit -> [> `Exit of int | `Run of Opts.t ]
  (** Will blow up in a variety of ways if the opts are bad :) *)
end = struct
  open Cmdliner

  module Verbosity = struct
    open Little_logger

    let log_level_term ?docs () =
      let verbosity_term =
        let doc =
          "Increase verbosity. Repeatable, but more than twice does not bring \
           more."
        in
        Arg.(value & flag_all & info [ "v" ] ~doc ?docs)
      in
      let quiet_term =
        let doc = "Silence all log messages. Takes over verbosity ($(b,-v))." in
        Arg.(value & flag & info [ "quiet" ] ~doc ?docs)
      in
      let choose quiet verbosity =
        if quiet then Logger.Level.Silent
        else
          match List.length verbosity with
          | 0 -> Logger.Level.Warning
          | 1 -> Logger.Level.Info
          | _ -> Logger.Level.Debug
      in
      Term.(const choose $ quiet_term $ verbosity_term)

    (* Adapted from the logs package. Original copyright:

       Copyright (c) 2015 The logs programmers. All rights reserved. Distributed
       under the ISC license, see terms at the end of the file.

       Permission to use, copy, modify, and/or distribute this software for any
       purpose with or without fee is hereby granted, provided that the above
       copyright notice and this permission notice appear in all copies.

       THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
       WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
       MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
       ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
       WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
       ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
       OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE. *)
  end

  (* TODO move this somewhere else. *)
  let version = "0.1.1"

  let query_term =
    let doc = "Path to query sequences" in
    Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"QUERY" ~doc)

  let db_term =
    let doc = "Path to target DB" in
    (* The db path will not always be a path to a real file. It is often just a
       "name" and the actual DB will have various suffices. *)
    Arg.(required & pos 1 (some string) None & info [] ~docv:"DB" ~doc)

  let outdir_term =
    let doc = "Out directory" in
    Arg.(value & opt string "." & info [ "o"; "outdir" ] ~docv:"OUTDIR" ~doc)

  let extra_config_term =
    let doc =
      "Extra config options for search program.  This option is pretty \
       flexible on the way it accepts arguments.  Genrally as long as you use \
       an equals sign (=) and single quotes, you will be alright to specify \
       the search program opts as you normally would.  Here are some \
       examples:--search-config='-s 7',--search-config='-num_threads 4 -evalue \
       1e-10', --search-config='--thread=10 -x -y 3 --color green'."
    in
    Arg.(
      value
      & opt (some string) None
      & info [ "search-config" ] ~docv:"SEARCH_CONFIG" ~doc)

  let basename_term =
    let doc = "Basename for output files" in
    Arg.(
      value & opt string "search"
      & info [ "b"; "basename" ] ~docv:"BASENAME" ~doc)

  (* TODO better converter for this *)
  let rpsblast_term =
    let env =
      let doc = "Name (or path) for rpsblast executable" in
      Cmd.Env.info "RPSBLAST" ~doc
    in
    let doc = "Name (or path) for rpsblast executable" in
    Arg.(
      value & opt string "rpsblast"
      & info [ "r"; "rpsblast" ] ~docv:"RPSBLAST" ~doc ~env)

  let evalue_term =
    let doc = "E-value for search" in
    Arg.(value & opt float 10. & info [ "e"; "evalue" ] ~docv:"EVALUE" ~doc)

  let num_threads_term =
    let doc = "Number of threads to use" in
    Arg.(value & opt int 1 & info [ "t"; "num-threads" ] ~docv:"THREADS" ~doc)

  let max_tries_term =
    let doc =
      "How many tries to rerun jobs?  Not per job retries, but this many tries \
       to get all jobs done."
    in
    Arg.(value & opt int 0 & info [ "retries" ] ~docv:"RETRIES" ~doc)

  let force_term =
    let doc = "If the outdir already exists, just keep going." in
    Arg.(value & flag & info [ "f"; "force" ] ~doc)

  let term =
    Term.(
      const Opts.make $ query_term $ db_term $ outdir_term $ basename_term
      $ force_term $ rpsblast_term $ evalue_term $ num_threads_term
      $ extra_config_term
      $ Verbosity.log_level_term ()
      $ max_tries_term)

  let info =
    let doc = "parallel_rpsblast" in
    let man =
      [
        `S Manpage.s_description;
        `P
          "Split input queries and run rpsblast in parallel to better utilize \
           multicore processors.";
      ]
    in
    Cmd.info "parallel_rpsblast" ~version ~doc ~man ~exits:[]

  let parse_cli () =
    match Cmd.eval_value @@ Cmd.v info term with
    | Ok (`Ok opts) ->
        Opts.check opts;
        `Run opts
    | Ok `Help | Ok `Version -> `Exit 0
    | Error _ -> `Exit 1
end

(* open! Async *)
module S = Shexp_process

let outfmt =
  "6 qaccver saccver pident length mismatch gapopen qstart qend sstart send \
   evalue bitscore stitle"

let make_outdir outdir = Unix.mkdir_p outdir ~perm:0o755

let split_prefix = "rpsblast_split_"

let split_search_suffix = ".tmp_rpsblast"

(* Split a fasta file into [num_splits]. Return the names of the files, ready to
   be used for other things. *)
let split_fasta fasta num_splits =
  let make_out_channels num_splits =
    Array.init num_splits ~f:(fun _ ->
        let name, oc = Filename.open_temp_file split_prefix "" in
        (name, oc))
  in
  let f out_channels =
    let open Bio_io.Fasta in
    In_channel.with_file_iteri_records_exn fasta ~f:(fun i r ->
        let _, oc = out_channels.(i % num_splits) in
        Out_channel.output_string oc @@ Record.to_string_nl r);
    (* Return the filenames. *)
    Array.map out_channels ~f:fst
  in
  let finally out_channels =
    Array.iter out_channels ~f:(fun (_, oc) -> Out_channel.close oc)
  in
  Exn.protectx ~f ~finally @@ make_out_channels num_splits

let make_commands query_splits opts =
  Array.fold query_splits ~init:([], []) ~f:(fun (outfiles, commands) query ->
      (* Just use the same outdir as the query, since the splits will already be
         in the outdir. *)
      let outfile = query ^ split_search_suffix in
      let cmd =
        S.run_exit_status opts.Opts.rpsblast
          [
            "-query";
            query;
            "-db";
            opts.db;
            "-evalue";
            Float.to_string opts.evalue;
            "-num_threads";
            "1";
            "-outfmt";
            outfmt;
            "-out";
            outfile;
          ]
      in
      (outfile :: outfiles, cmd :: commands))

(* TODO S.protect to clean up. *)
let eval_commands max_tries commands =
  let eval_failed (_, exit_status) =
    match exit_status with S.Exit_status.Exited 0 -> false | _ -> true
  in
  let rec loop i cmds =
    let results = List.zip_exn cmds (S.eval @@ S.fork_all cmds) in
    let rerun_these = List.filter results ~f:eval_failed in
    match rerun_these with
    | [] -> ()
    | _ ->
        if i < max_tries then (
          Little_logger.Logger.debug (fun () ->
              [%string "Trying again (%{(i+1)#Int})!"]);
          loop (i + 1) @@ List.map rerun_these ~f:fst)
  in
  loop 0 commands

let final_outfile outdir basename = Filename.concat outdir (basename ^ ".tsv")

(* TODO handle this in ocaml rather than with cat. *)
let cat_results search_files final_outfile =
  (* Note that the list of outfiles is passed in as CLI args to cat. Shouldn't
     be a problem as num threads shouldn't be crazy high, but still...*)
  S.eval @@ S.stdout_to final_outfile @@ S.run "cat" @@ search_files

(* TODO can't use glob here? :( Would be nice to just use rm and a glob. *)
let remove_files iter files =
  iter files ~f:(fun file -> if file_exists file then Sys.remove file)

let run (opts : Opts.t) =
  let open Little_logger in
  Logger.set_log_level opts.verbosity;
  make_outdir opts.outdir;
  Logger.sinfo "Splitting fasta";
  let query_splits = split_fasta opts.query opts.num_threads in
  let search_files, commands = make_commands query_splits opts in
  Logger.sinfo "Evaluating commands";
  eval_commands opts.max_tries commands;
  Logger.sinfo "Cleaning up";
  cat_results search_files @@ final_outfile opts.outdir opts.basename;
  remove_files Array.iter query_splits;
  remove_files List.iter search_files

let main () =
  match Cli.parse_cli () with `Run opts -> run opts | `Exit code -> exit code

let () = main ()
