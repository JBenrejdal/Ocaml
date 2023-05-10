open Graphics

type direction = Vertical | Horizontal [@@deriving show]
type window = Win of string * Color.t [@@deriving show]


type coordinate = Coord of {px: int; py: int; sx: int; sy: int} [@@deriving show]
type split = Split of direction * float (* ratio between 0 and 1 *) [@@deriving show]

let draw_win w coord (bc:Color.t) =

  let Coord{px; py; sx; sy}=coord in
  let Win(title,c)=w in


  (**Draw window contents *)
  let (r, g, b) = Color.to_rgb c in
  set_color (rgb r g b);
  fill_rect px py (sx-4) (sy-4);


  (** Draw window border *)
  let (r, g, b) = Color.to_rgb bc in
  set_color (rgb r g b);
  set_line_width 5;
  draw_rect (px) (py) (sx-3) (sy-3);
  

  


  (*** Draw window title *)
  set_color black;

  let text_x = px + sx/2  in
  let text_y = py + sy/2 in
  moveto (text_x) (text_y);
  draw_string title;



type wmtree = ((split * coordinate), (window * coordinate)) Tree.t [@@deriving show]
type wmzipper = ((split * coordinate), (window * coordinate)) Tree.z [@@deriving show]

let  get_coord (wt : wmtree) : coordinate =match wt with
  | Tree.Leaf (_, c) -> c
  | Tree.Node ((_,c), _, _) -> c
  

let change_coord c wt = match wt with
  | Tree.Leaf (w, _) -> Tree.Leaf (w, c)
  | Tree.Node ((s,_), l, r) -> Tree.Node ((s,c), l, r)


let rec draw_wmtree bc wt = match wt with
  (*Si c'est une feuille alors on dessine la fenêtre*)
  | Tree.Leaf (win, coord) ->
  draw_win win coord bc
  (*Sinon, on rapelle la fonction avec le sous-arbre gauche et le sous arbre droit respectivement*)
  | Tree.Node ((_, _), left, right) ->
  draw_wmtree bc left;
  draw_wmtree bc right

let  draw_wmzipper bc wz =  match wz with
  |Tree.TZ(_,t)->draw_wmtree bc t
  
(** Fonction qui prend en parametre des coordonne et un Split et envoie un couple de coordonnee*)       
let split_coord (Coord {px=x; py=y; sx=w; sy=h}) (Split(d, r))  = match d with
    (*Si la direction de la division est hozizontale*)
    | Horizontal ->
      let lw = int_of_float ((float_of_int w) *. r) in
      let lc = Coord {px=x; py=y; sx=lw; sy=h} in
      let rc = Coord {px=x+lw; py=y; sx=int_of_float ((float_of_int w) *. (1.-.r)); sy=h} in
      (lc, rc)
    (*Si la direction de la division est verticale*)
    | Vertical ->
      let lh = int_of_float ((float_of_int h) *. r) in
      let lc = Coord {px=x; py=y; sx=w; sy=lh} in
      let rc = Coord {px=x; py=y+lh; sx=w; sy=h-lh} in
      (lc, rc) 
     
let rec update_coord (c:coordinate) (t:wmtree) = match t with 
    | Tree.Leaf (w, _) -> Tree.Leaf (w, c)
    | Tree.Node ((s,_), l, r) ->
        (*On appelle la fonction split pour séparer et calculer les coordonnées, puis on rappelle la fonction récursivement*)
        let (lc, rc):(coordinate*coordinate) = split_coord c s in
        Tree.Node ((s,c), update_coord lc l, update_coord rc r)
     
      
