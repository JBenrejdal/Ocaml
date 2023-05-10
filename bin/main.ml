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
  (*La fonction recup permet de recuperer le sous-arbre associé à un zipper donné*)
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
      (*La touche H permet de creer une nouvelle fenetre de manière horizontale*)
    | 'h' ->
      Stdio.printf "\nhorizontal\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre, on en crée une et on la dessine*)
        | None ->
          let win = init_win count () in
          Wm.draw_wmzipper active_color win;
          Some win
        (*Si il y a une ou plusieurs fenêtre, on en crée une nouvelle et on la dessine tout en mettant à jour les coordonnées*)
        | Some z ->
          
            let new_win = init_win (count) () in
            (*On crée un nouveau noeud*)
            let tree' =  Tree.Node((Wm.Split(Wm.Horizontal,default_ratio),Wm.get_coord (recup z)),recup z,recup new_win)in
            (*On met a jour le zipper avec les nouvelles coordonnées*)
            let newzip = Tree.change z (Wm.update_coord (Wm.get_coord (recup z)) tree') in
            (*On dessine le zipper tout en mettant le focus sur la dernière fenêtre*)
            let newzip2 = Tree.go_down newzip >>= Tree.go_right>>=(fun x -> Wm.draw_wmzipper inactive_color newzip; Wm.draw_wmzipper active_color x ; return x   ) in
            
           
           newzip2
        in (* compute new zipper after insertion  *)
        loop newzipoption (count+1) (* loop *)
      end
      (*La touche v permet de creer une nouvelle fenetre de manière verticale*)
    | 'v' ->
      Stdio.printf "\nvertical\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre, on en crée une et on la dessine*)
        | None ->
          let win = init_win count () in
          Wm.draw_wmzipper active_color win;
          Some win
        (*Si il y a une ou plusieurs fenêtre, on en crée une nouvelle et on la dessine tout en mettant à jour les coordonnées*)
        | Some z ->
            let new_win = init_win (count) () in
            (*On crée un nouveau noeud*)
            let tree' =  Tree.Node((Wm.Split(Wm.Vertical,default_ratio),Wm.get_coord (recup z)),recup z,recup new_win)in
            (*On met a jour le zipper avec les nouvelles coordonnées*)
            let newzip = Tree.change z (Wm.update_coord (Wm.get_coord (recup z)) tree') in
            (*On dessine le zipper tout en mettant le focus sur la dernière fenêtre*)
            let newzip2 = Tree.go_down newzip >>= Tree.go_right>>=(fun x -> Wm.draw_wmzipper inactive_color newzip; Wm.draw_wmzipper active_color x ; return x   ) in
            
           
           newzip2
        in (* compute new zipper after insertion  *)
        loop newzipoption (count+1) (* loop *)
      end
(*La touche n permet de déplacer le focus sur la prochaine fenêtre*)
    | 'n' ->
      Stdio.printf "\nnext\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre, on ne peut pas déplacer le focus, on renvoie donc "None"*)
        | None -> None
        (*Si il y a une ou plusieurs fenêtre, on déplace le focus sur la prochaine fenêtre si possible*)
        | Some z ->
          let stop z = match Tree.next_leaf z with
              | None -> Some z
              |_-> Tree.next_leaf z in
            let newzip = stop z >>=(fun x -> Wm.draw_wmzipper inactive_color z; Wm.draw_wmzipper active_color x ; return x   ) in
           
           newzip
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
      (*La touche n permet de déplacer le focus sur la fenêtre précédente*)
    | 'p' ->
      Stdio.printf "\nprevious\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre, on ne peut pas déplacer le focus, on renvoie donc "None"*)
        | None -> None
        (*Si il y a une ou plusieurs fenêtre, on déplace le focus sur la fenêtre precedente si possible*)
        | Some z ->
            let stop z = match Tree.previous_leaf z with
              | None -> Some z
              |_-> Tree.previous_leaf z in
            let newzip = stop z >>=(fun x -> Wm.draw_wmzipper inactive_color z; Wm.draw_wmzipper active_color x ; return x   ) in
            
           
           newzip
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
      (*La touche + permet d'augmenter la taille de la fenêtre*)
    | '+' ->
      Stdio.printf "\nincrement size\n%!";
      (*Fonction qui permet de modifier le ratio d'une fenêtre*)
      let get_ratio = fun (Tree.TZ(c,t))-> match c with
          |Tree.Top-> None
          |Tree.LNContext(((Wm.Split(d,f)),c),ctx,r)-> Some (Tree.TZ(Tree.LNContext(((Wm.Split(d,inc_ratio f)),c),ctx,r),t))
          |Tree.RNContext(l,((Wm.Split(d,f)),c),ctx)-> Some (Tree.TZ(Tree.RNContext(l,((Wm.Split(d,dec_ratio f)),c),ctx),t))
        in
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre on ne peut pas augmenter la taille de la fenêtre, on renvoie donc "None"*)
        | None -> None
        (*Si il y a une ou plusieurs fenêtre, on augmente la taille de la fenêtre qui a le focus, 
           pour ce faire on recupere les informations du noeud parent pour les mettre à jour, puis on revient sur la fenêtre initiale *)
        | Some z ->
          (get_ratio z) >>= ( fun (Tree.TZ(c,t)) -> match c with  
          (*Si c'est un fils droite*)
          |Tree.RNContext _-> Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
          Tree.go_down >>= Tree.go_right >>= (fun x -> Wm.draw_wmzipper active_color x;return x)
          (*Sinon*)
          | _ ->  Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
           Tree.go_down >>=  (fun x -> Wm.draw_wmzipper active_color x;return x))

          
        
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
      (*La touche - permet de diminuer la taille de la fenêtre*)
    | '-' ->
      Stdio.printf "\ndecrement size\n%!";
      (*Fonction qui permet de modifier le ratio d'une fenêtre*)
      let get_ratio = fun (Tree.TZ(c,t))-> match c with
          |Tree.Top-> None
          |Tree.LNContext(((Wm.Split(d,f)),c),ctx,r)-> Some (Tree.TZ(Tree.LNContext(((Wm.Split(d,dec_ratio f)),c),ctx,r),t))
          |Tree.RNContext(l,((Wm.Split(d,f)),c),ctx)-> Some (Tree.TZ(Tree.RNContext(l,((Wm.Split(d,inc_ratio f)),c),ctx),t))
        in
      
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre on ne peut pas diminuer la taille de la fenêtre, on renvoie donc "None"*)
        | None -> None
        (*Si il y a une ou plusieurs fenêtre, on augmente la taille de la fenêtre qui a le focus, 
           pour ce faire on recupere les informations du noeud parent pour les mettre à jour, puis on revient sur la fenêtre initiale *)
        | Some z ->
          (*Si c'est un fils droite*)
          (get_ratio z) >>= ( fun (Tree.TZ(c,t)) -> match c with  
          |Tree.RNContext _-> Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
          Tree.go_down >>= Tree.go_right >>= (fun x -> Wm.draw_wmzipper active_color x;return x)
          (*Sinon*)
          | _ ->  Tree.go_up (Tree.TZ(c,t)) >>=(fun (Tree.TZ(c,t)) -> return (Tree.TZ(c,Wm.update_coord (Wm.get_coord t) t)))>>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
           Tree.go_down >>=  (fun x -> Wm.draw_wmzipper active_color x;return x))

            
        in (* compute new zipper after insertion  *)
        loop newzipoption (count) (* loop *)
      end
      
(* La touche r permet de supprimer une fenêtre*)
    | 'r' ->
      Stdio.printf "\nremove\n%!";
      let (>>=) = Option.(>>=) in 
      let return = Option.return in
      begin
        let newzipoption = match oz with
        (*Si il n'y a pas de fenêtre on ne peut pas diminuer la taille de la fenêtre, on renvoie donc "None"*)
        | None -> None
        (*Si il y a une ou plusieurs fenêtre, on supprime la fenêtre qui a le focus, 
           pour ce faire on recupere les informations du zipper pour mettre à jour les coordonnées, puis on revient sur la fenêtre initiale *)
        | Some z ->
          let remove (Tree.TZ(c,t)) = match Tree.remove_leaf (Tree.TZ(c,t)) with
              | None -> None
              | Some(z,_)-> Some  z in
            let newzip = remove z >>= (fun (Tree.TZ(c,t)) -> match c with 
            (*Si après avoir supprime la fenêtre, le contexte de la fenêtre qui récupère le focus correspond a la racine, alors on l'affiche selon les coordonnées initiales*)
            | Tree.Top ->   return (Tree.TZ(c,Wm.update_coord (Wm.Coord{px=0; py=0; sx=width;sy=height} ) t)) >>=(fun x -> Wm.draw_wmzipper inactive_color x;return x)>>=
                (fun x -> Wm.draw_wmzipper active_color x;return x)
            (*Sinon,on recupere les informations du noeud parent pour les mettre à jour, puis on revient sur la fenêtre initiale *)
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
