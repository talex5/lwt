(******************************************************************************)
(* Lightweight thread library for OCaml
 * http://www.ocsigen.org/lwt
 * Module Lwt_condition
 ******************************************************************************
 * Copyright (c) 2009, Metaweb Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above
 *       copyright notice, this list of conditions and the following
 *       disclaimer in the documentation and/or other materials provided
 *       with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY METAWEB TECHNOLOGIES ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL METAWEB TECHNOLOGIES BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************)

open Lwt.Infix

type 'a t = ('a Lwt.u Lwt_sequence.t * string option)

let create ?label () = (Lwt_sequence.create (), label)

let wait ?mutex (cvar, label) =
  let waiter = Lwt.add_task_r ~thread_type:Lwt_tracing.Condition cvar in
  begin match label with
  | None -> ()
  | Some label -> Lwt_tracing.(!tracer.note_label) (Lwt.id_of_thread waiter) label end;
  let () =
    match mutex with
      | Some m -> Lwt_mutex.unlock m
      | None -> ()
  in
  Lwt.finalize
    (fun () -> waiter)
    (fun () ->
       match mutex with
         | Some m -> Lwt_mutex.lock m
         | None -> Lwt.return_unit)

let signal (cvar, _) arg =
  try
    Lwt.wakeup_later (Lwt_sequence.take_l cvar) arg
  with Lwt_sequence.Empty ->
    ()

let broadcast (cvar, _) arg =
  let wakeners = Lwt_sequence.fold_r (fun x l -> x :: l) cvar [] in
  Lwt_sequence.iter_node_l Lwt_sequence.remove cvar;
  List.iter (fun wakener -> Lwt.wakeup_later wakener arg) wakeners
