open! Core

(* [get_credits] should take the contents of an IMDB page for an actor and return a list
   of strings containing that actor's main credits.

   class="ipc-metadata-list-summary-item__t"

   ipc-metadata-list-summary-item__c
*)
let get_credits contents : string list =
  let open! Soup in
  (* print_string contents; *)
  let contents_parsed = parse contents in
  let the_nodes = contents_parsed $$ "a" in
  let nodes_as_a_list = to_list the_nodes in
  (* *)
  let valid_filter node =
    has_attribute "class" node
    && String.is_substring
         ~substring:"ipc-metadata-list-summary-item"
         (Option.value_exn (attribute "class" node))
    (* && not *)
    (* (String.is_substring
       ~substring:":"
       (Option.value_exn (attribute "href" node))) *)
  in
  let nodes_as_a_list = List.filter ~f:valid_filter nodes_as_a_list in
  (* let get_href node = Option.value_exn (attribute "href" node) in *)
  let output =
    List.map nodes_as_a_list ~f:(fun li ->
      texts li |> String.concat ~sep:"" |> String.strip)
  in
  output
;;

(* ignore (contents : string);
   failwith "TODO" *)

let print_credits_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "given an IMDB page for an actor, print out a list of their main \
       credits"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_credits contents) ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"imdb commands"
    [ "print-credits", print_credits_command ]
;;
