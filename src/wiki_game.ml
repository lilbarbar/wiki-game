open! Core

(* [get_linked_articles] should return a list of wikipedia article lengths contained in
   the input.

   Note that [get_linked_articles] should ONLY return things that look like wikipedia
   articles. In particular, we should discard links that are:
   - Wikipedia pages under special namespaces that are not articles (see
     https://en.wikipedia.org/wiki/Wikipedia:Namespaces)
   - other Wikipedia internal URLs that are not articles
   - resources that are external to Wikipedia
   - page headers

   One nice think about Wikipedia is that stringent content moderation results in
   uniformity in article format. We can expect that all Wikipedia article links parsed
   from a Wikipedia page will have the form "/wiki/<TITLE>". *)

module Website = struct
  module T = struct
    type t =
      { title_name : string
      ; url_link : string
      }
    [@@deriving compare, sexp, hash, equal]
  end

  include T
  module Hash_set = Hash_set.Make (T)
end

module Network = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = Website.t * Website.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)
  end

  type t = Connection.Set.t [@@deriving sexp_of]
end

let no_parantheses input_string : string =
  let first_step = String.tr input_string ~target:'(' ~replacement:'_' in
  String.tr first_step ~target:')' ~replacement:'_'
;;

module G = Graph.Imperative.Graph.Concrete (Website)

(* We extend our [Graph] structure with the [Dot] API so that we can easily render
   constructed graphs. Documentation about this API can be found here:
   https://github.com/backtracking/ocamlgraph/blob/master/src/dot.mli *)
module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes _ = [ `Dir `None ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes _ = [ `Shape `Box; `Fillcolor 1000 ]
    let vertex_name (v : Website.t) = no_parantheses v.title_name
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let get_linked_articles contents : string list =
  (* print_s [%message "here are the contents" (contents : string)]; *)
  let open! Soup in
  (* print_string contents; *)
  let contents_parsed = parse contents in
  let the_nodes = contents_parsed $$ "a" in
  let nodes_as_a_list = to_list the_nodes in
  (* *)
  let valid_filter node =
    String.is_substring
      ~substring:"wiki"
      (Option.value_exn (attribute "href" node))
    && not
         (String.is_substring
            ~substring:":"
            (Option.value_exn (attribute "href" node)))
  in
  let nodes_as_a_list = List.filter ~f:valid_filter nodes_as_a_list in
  let get_href node = Option.value_exn (attribute "href" node) in
  let output = List.map nodes_as_a_list ~f:get_href in
  output
;;

String.is_substring

(* let is_valid_link 
  List.filter   *)
(* ignore (contents : string);
  failwith "TODO" *)

let print_links_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Print all of the valid wiki page links on a page"
    [%map_open
      let how_to_fetch, resource = File_fetcher.param in
      fun () ->
        let contents = File_fetcher.fetch_exn how_to_fetch ~resource in
        List.iter (get_linked_articles contents) ~f:print_endline]
;;

(* [visualize] should explore all linked articles up to a distance of [max_depth] away
   from the given [origin] article, and output the result as a DOT file. It should use the
   [how_to_fetch] argument along with [File_fetcher] to fetch the articles so that the
   implementation can be tested locally on the small dataset in the ../resources/wiki
   directory. *)

let get_title contents : string =
  let open Soup in
  parse contents $ "title" |> R.leaf_text
;;

let _make_website contents : Website.t =
  let title = get_title contents in
  let just_title =
    String.concat
      ~sep:"_"
      (List.drop_last_exn (List.drop_last_exn (String.split ~on:' ' title)))
  in
  let url = "wiki/" ^ just_title in
  print_string (url ^ "\n");
  { title_name = title; url_link = url }
;;

let make_website contents : Website.t =
  print_s [%message "" ~input:(get_title contents : string)];
  let title =
    get_title contents
    |> String.substr_replace_all ~pattern:" - Wikipedia" ~with_:""
    |> String.substr_replace_all ~pattern:" " ~with_:"_"
  in
  (* String.concat
     ~sep:"_"
     (List.drop_last_exn (List.drop_last_exn (String.split ~on:' ' title))) in *)
  let url = "wiki/" ^ title in
  print_string (url ^ "\n");
  { title_name = title; url_link = url }
;;

let visualize ?(max_depth = 3) ~origin ~output_file ~how_to_fetch () : unit =
  let visited = Website.Hash_set.create () in
  let graph = G.create () in
  (* Set.iter network ~f:(fun (person1, person2) -> G.add_edge graph person1 person2);
     Dot.output_graph
     (Out_channel.create (File_path.to_string output_file))
     graph *)
  (* let current_website : Website.t = make_website contents_of_file in *)
  let rec recursive_visualize depth origin =
    match depth with
    | 0 -> ()
    | _ ->
      let contents_of_file =
        File_fetcher.fetch_exn how_to_fetch ~resource:origin
      in
      let current_website : Website.t = make_website contents_of_file in
      let linked_articles = get_linked_articles contents_of_file in
      let linked_websites =
        List.map linked_articles ~f:(fun x ->
          make_website (File_fetcher.fetch_exn how_to_fetch ~resource:x))
        (* |> List.filter ~f:(fun x -> not (Hash_set.mem visited x)) *)
      in
      List.iter linked_websites ~f:(fun x ->
        G.add_edge graph current_website x);
      Hash_set.add visited current_website;
      List.iter linked_websites ~f:(fun x ->
        recursive_visualize (depth - 1) x.url_link)
  in
  recursive_visualize max_depth origin;
  Dot.output_graph
    (Out_channel.create (File_path.to_string output_file))
    graph;
  print_string "done";
  ()
;;

(* ignore (max_depth : int);
  ignore (origin : string);
  ignore (output_file : File_path.t);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t); *)
(* failwith "TODO" *)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        visualize ~max_depth ~origin ~output_file ~how_to_fetch ();
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* [find_path] should attempt to find a path between the origin article and the
   destination article via linked articles.

   [find_path] should use the [how_to_fetch] argument along with [File_fetcher] to fetch
   the articles so that the implementation can be tested locally on the small dataset in
   the ../resources/wiki directory.

   [max_depth] is useful to limit the time the program spends exploring the graph. *)
let find_path ?(max_depth = 3) ~origin ~destination ~how_to_fetch () =
  ignore (max_depth : int);
  ignore (origin : string);
  ignore (destination : string);
  ignore (how_to_fetch : File_fetcher.How_to_fetch.t);
  failwith "TODO"
;;

let find_path_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "Play wiki game by finding a link between the origin and destination \
       pages"
    [%map_open
      let how_to_fetch = File_fetcher.How_to_fetch.param
      and origin = flag "origin" (required string) ~doc:" the starting page"
      and destination =
        flag "destination" (required string) ~doc:" the destination page"
      and max_depth =
        flag
          "max-depth"
          (optional_with_default 10 int)
          ~doc:"INT maximum length of path to search for (default 10)"
      in
      fun () ->
        match find_path ~max_depth ~origin ~destination ~how_to_fetch () with
        | None -> print_endline "No path found!"
        | Some trace -> List.iter trace ~f:print_endline]
;;

let command =
  Command.group
    ~summary:"wikipedia game commands"
    [ "print-links", print_links_command
    ; "visualize", visualize_command
    ; "find-path", find_path_command
    ]
;;
