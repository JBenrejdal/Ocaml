open Base
open Stdio
open Graphics
open Ocamlwm23


let main () =

  let width = 640 in
  let height = 480 in
  let default_ratio = 0.5 in

  let active_color = Color.white in
  let inactive_color = Color.black in

  (* never increase ration above 0.95 or decrease below 0.05 *)
  let inc_ratio ratio = Float.min 0.95 (ratio +. 0.05) in
  let dec_ratio ratio = Float.max 0.05 (ratio -. 0.05) in

  (* create a new window *)
  let init_win count () =
    let w = Wm.Win("W" ^ (Int.to_string count), Color.random ()) in
    let c = Wm.Coord {px=0; py=0; sx=width;sy=height} in
    Tree.return  (w,c)  |> Tree.focus_first_leaf
  in

  (* create the canvas to draw windows *)
  let f = Printf.sprintf " %dx%d" width height in
  let () = Graphics.open_graph f in

  let recup = fun (Tree.TZ(_,t))-> t in


  (* event loop *)
  let rec loop oz count =
    (match oz with
     | None -> Stdio.printf "\nZERO WINDOW\n%!"; clear_graph()
     | Some z -> Stdio.printf "\n%s\n%!" (Wm.show_wmzipper z)
    );

    match Graphics.read_key () with
    | 'q' ->
      Stdio.printf "Total number of created windows: %d\nBye\n%!" count;
      raise Caml.Exit
    | 'h' ->
      Stdio.printf "\nhorizontal\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        | None ->
          let win = init_win count () in
          Wm.draw_wmzipper active_color win;
          Some win
        | Some z ->
          
            let new_win = init_win (count) () in
            let tree' =  Tree.Node((Wm.Split(Wm.Horizontal,default_ratio),Wm.get_coord (recup z)),recup z,recup new_win)in
            let newzip = Tree.change z (Wm.update_coord (Wm.get_coord (recup z)) tree') in
            let newzip2 = Tree.go_down newzip >>= Tree.go_right>>=(fun x -> Wm.draw_wmzipper inactive_color newzip; Wm.draw_wmzipper active_color x ; return x   ) in
            
           
           newzip2
        in (* compute new zipper after insertion  *)
        loop newzipoption (count+1) (* loop *)
      end

    | 'v' ->
      Stdio.printf "\nvertical\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        | None ->
          let win = init_win count () in
          Wm.draw_wmzipper active_color win;
          Some win
        | Some z ->
          
            let new_win = init_win (count) () in
            let tree' =  Tree.Node((Wm.Split(Wm.Vertical,default_ratio),Wm.get_coord (recup z)),recup z,recup new_win)in
            let newzip = Tree.change z (Wm.update_coord (Wm.get_coord (recup z)) tree') in
            let newzip2 = Tree.go_down newzip >>= Tree.go_right>>=(fun x -> Wm.draw_wmzipper inactive_color newzip; Wm.draw_wmzipper active_color x ; return x   ) in
            
           
           newzip2
        in (* compute new zipper after insertion  *)
        loop newzipoption (count+1) (* loop *)
      end

    | 'n' ->
      Stdio.printf "\nnext\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        | None -> None
        | Some z ->
          let stop z = match Tree.next_leaf z with
              | None -> Some z
              |_-> Tree.next_leaf z in
            let newzip = stop z >>=(fun x -> Wm.draw_wmzipper inactive_color z; Wm.draw_wmzipper active_color x ; return x   ) in
           
           newzip
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
    | 'p' ->
      
      Stdio.printf "\nprevious\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        | None -> None
        | Some z ->
            let stop z = match Tree.previous_leaf z with
              | None -> Some z
              |_-> Tree.previous_leaf z in
            let newzip = stop z >>=(fun x -> Wm.draw_wmzipper inactive_color z; Wm.draw_wmzipper active_color x ; return x   ) in
            
           
           newzip
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
    | '+' ->
      Stdio.printf "\nincrement size\n%!";
      let get_ratio = fun (Tree.TZ(c,t))-> match c with
          |Tree.Top-> None
          |Tree.LNContext(((Wm.Split(d,f)),c),ctx,r)-> Some (Tree.TZ(Tree.LNContext(((Wm.Split(d,inc_ratio f)),c),ctx,r),t))
          |Tree.RNContext(l,((Wm.Split(d,f)),c),ctx)-> Some (Tree.TZ(Tree.RNContext(l,((Wm.Split(d,dec_ratio f)),c),ctx),t))
        in
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        | None -> None
        | Some z ->
          let _save  = return (recup z) >>= (Tree.get_leaf_data) >>= (fun (Wm.Win(s,_),_) -> return s) in
          let _valeur = Option.value_exn _save in
          (get_ratio z) >>= ( fun (Tree.TZ(c,t)) -> match c with  
          |Tree.RNContext _-> Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
          Tree.go_down >>= Tree.go_right >>= (fun x -> Wm.draw_wmzipper active_color x;return x)

          | _ ->  Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
           Tree.go_down >>=  (fun x -> Wm.draw_wmzipper active_color x;return x))

          
        
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
    | '-' ->
      Stdio.printf "\ndecrement size\n%!";
      let get_ratio = fun (Tree.TZ(c,t))-> match c with
          |Tree.Top-> None
          |Tree.LNContext(((Wm.Split(d,f)),c),ctx,r)-> Some (Tree.TZ(Tree.LNContext(((Wm.Split(d,dec_ratio f)),c),ctx,r),t))
          |Tree.RNContext(l,((Wm.Split(d,f)),c),ctx)-> Some (Tree.TZ(Tree.RNContext(l,((Wm.Split(d,inc_ratio f)),c),ctx),t))
        in
      
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        
        let newzipoption = match oz with
        | None -> None
        | Some z ->
          let _save  = return (recup z) >>= (Tree.get_leaf_data) >>= (fun (Wm.Win(s,_),_) -> return s) in
          let _valeur = Option.value_exn _save in
          (get_ratio z) >>= ( fun (Tree.TZ(c,t)) -> match c with  
          |Tree.RNContext _-> Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
          Tree.go_down >>= Tree.go_right >>= (fun x -> Wm.draw_wmzipper active_color x;return x)

          | _ ->  Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
           Tree.go_down >>=  (fun x -> Wm.draw_wmzipper active_color x;return x))

            
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
      

    | 'r' ->
      Stdio.printf "\nremove\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        | None -> None
        | Some z ->
          let remove (Tree.TZ(c,t)) = match Tree.remove_leaf (Tree.TZ(c,t)) with
              | None -> None
              | Some(z,_)-> Some  z in
            let newzip = remove z >>= (fun (Tree.TZ(c,t)) -> match c with 
            | Tree.Top ->   return (Tree.TZ(c,Wm.update_coord (Wm.Coord{px=0; py=0; sx=width;sy=height} ) t)) >>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
                (fun x -> Wm.draw_wmzipper active_color x;return x)
            |_ ->  Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t))) >>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
            Tree.next_leaf >>=  (fun x -> Wm.draw_wmzipper active_color x;return x)) in
           
           newzip
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
    | c ->
      printf "cannot process command '%c'\n%!" c;
      loop oz count

  in
  try
    loop None 0
  with
  | Stdlib.Exit -> ()


let () = main ()
