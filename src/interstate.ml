open! Core
module City = String

module Interstate = struct
  include String

  let default = ""
end

module Highway_System = struct
  (* We can represent our social network graph as a set of connections, where a connection
     represents a friendship between two people. *)
  module Connection = struct
    module T = struct
      type t = City.t * City.t * Interstate.t [@@deriving compare, sexp]
    end

    (* This funky syntax is necessary to implement sets of [Connection.t]s. This is needed
       to defined our [Network.t] type later. Using this [Comparable.Make] functor also
       gives us immutable maps, which might come in handy later. *)
    include Comparable.Make (T)

    let of_string s =
      (* let open! Core in *)

      (* string is formated because the api does not work well with . *)
      let format_string =
        String.substr_replace_all s ~pattern:"." ~with_:""
      in
      let new_s =
        String.substr_replace_all format_string ~pattern:" " ~with_:"_"
      in
      let highway =
        Option.value_exn (List.hd (String.split new_s ~on:','))
      in
      let cities_in_interstate = List.tl (String.split new_s ~on:',') in
      let reversed_cities =
        List.rev (Option.value_exn cities_in_interstate)
      in
      let make_pair city1 city2 =
        City.of_string city1, City.of_string city2, highway
      in
      let rec get_connections cities : (string * string * string) list =
        match List.length cities with
        | 0 -> []
        | _ ->
          List.map
            (Option.value_exn (List.tl cities))
            ~f:(fun city ->
              make_pair (Option.value_exn (List.hd cities)) city)
          @ get_connections (Option.value_exn (List.tl cities))
      in
      get_connections (Option.value_exn cities_in_interstate)
      @ get_connections reversed_cities
    ;;

    (* | [ x; y ] -> Some (City.of_string x, City.of_string y)
       | _ -> None *)
  end

  type t = Connection.Set.t [@@deriving sexp_of]

  (* let get_interstate_from_string s =
     Option.value_exn (List.hd (String.split s ~on:','))
     ;; *)

  let of_file input_file =
    let connections =
      In_channel.read_lines (File_path.to_string input_file)
      |> List.concat_map ~f:(fun s -> Connection.of_string s)
    in
    Connection.Set.of_list connections
  ;;
end

let load_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file listing interstates and serialize graph as a sexp"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing interstates and the cities they go through"
      in
      fun () ->
        let highways = Highway_System.of_file input_file in
        (* This special syntax can be used to easily sexp-serialize values (whose types
           have [sexp_of_t] implemented). *)
        printf !"%{sexp: Highway_System.t}\n" highways]
;;

(* ignore (input_file : File_path.t);
        failwith "TODO"] *)

module G = Graph.Imperative.Graph.ConcreteLabeled (City) (Interstate)

module Dot = Graph.Graphviz.Dot (struct
    include G

    (* These functions can be changed to tweak the appearance of the generated
       graph. Check out the ocamlgraph graphviz API
       (https://github.com/backtracking/ocamlgraph/blob/master/src/graphviz.mli) for
       examples of what values can be set here. *)
    let edge_attributes e = [ `Dir `None; `Label (snd3 e) ]
    let default_edge_attributes _ = []
    let get_subgraph _ = None
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let visualize_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:
      "parse a file listing interstates and generate a graph visualizing \
       the highway network"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:
            "FILE a file listing all interstates and the cities they go \
             through"
      and output_file =
        flag
          "output"
          (required File_path.arg_type)
          ~doc:"FILE where to write generated graph"
      in
      fun () ->
        let network = Highway_System.of_file input_file in
        let graph = G.create () in
        Set.iter network ~f:(fun (person1, person2, interstate) ->
          (* [G.add_edge] auomatically adds the endpoints as vertices in the graph if
             they don't already exist. *)
          G.add_edge_e graph (person1, interstate, person2));
        Dot.output_graph
          (Out_channel.create (File_path.to_string output_file))
          graph;
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file]
;;

(* ignore (input_file : File_path.t);
        ignore (output_file : File_path.t);
        printf !"Done! Wrote dot file to %{File_path}\n%!" output_file] *)

let command =
  Command.group
    ~summary:"interstate highway commands"
    [ "load", load_command; "visualize", visualize_command ]
;;
