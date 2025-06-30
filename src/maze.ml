open! Core
module G = Graph.Imperative.Graph.Concrete (String)

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
    let vertex_attributes v = [ `Shape `Box; `Label v; `Fillcolor 1000 ]
    let vertex_name v = v
    let default_vertex_attributes _ = []
    let graph_attributes _ = []
  end)

let get_valid_neighbors matrix i j num_rows num_cols : string list =
  let options = [ i + 1, j; i - 1, j; i, j - 1; i, j + 1 ] in
  let options =
    List.filter options ~f:(fun (x, y) ->
      (x >= 0 && x < num_rows) && y >= 0 && y < num_cols)
  in
  let out = List.map options ~f:(fun (x, y) -> matrix.(x).(y)) in
  List.filter out ~f:(fun x -> not (String.equal x "#"))
;;

let solve_command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"parse a file containing a maze and find a solution"
    [%map_open
      let input_file =
        flag
          "input"
          (required File_path.arg_type)
          ~doc:"FILE a file containing a maze"
      in
      fun () ->
        let lines = In_channel.read_lines (File_path.to_string input_file) in
        let num_rows = List.length lines in
        let num_cols = String.length (List.hd_exn lines) in
        let matrix = Array.make_matrix ~dimx:num_cols ~dimy:num_rows "0" in
        let index = -1 in
        let min_index = 0 in
        let max_index = num_cols * num_rows in
        let get_row index : int = index / num_rows in
        let get_col index = index % num_rows in
        (* index % num_rows *)
        List.iter lines ~f:(fun x ->
          String.iter x ~f:(fun x ->
            let index = index + 1 in
            if Char.equal x '#'
            then matrix.(get_row index).(get_col index) <- "#"
            else if Char.equal x 'E'
            then matrix.(get_row index).(get_col index) <- "E"
            else
              matrix.(get_row index).(get_col index)
              <- string_of_int (get_row index)
                 ^ ","
                 ^ string_of_int (get_col index)));
        matrix.(1).(0) <- "S";
        let adjacncy_matrix = Hashtbl.create (module String) in
        Array.iteri matrix ~f:(fun i row ->
          Array.iteri row ~f:(fun j x ->
            if String.equal x "#"
            then ()
            else (
              let neighbors =
                get_valid_neighbors matrix i j num_rows num_cols
              in
              Hashtbl.add_exn
                adjacncy_matrix
                ~key:matrix.(i).(j)
                ~data:neighbors)))]
;;

let command =
  Command.group ~summary:"maze commands" [ "solve", solve_command ]
;;
