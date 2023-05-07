(** Color abstraction for our window manager.
    Can provide back and forth operations with RGB and internal representations
*)

open Base

type t = Int.t [@@deriving show]


let from_rgb r g b = 
   r * 65536 + g * 256 + b;;

let to_rgb t = 
    let r:int = t / 65536  and  g:int = (t / 256)%256  and  b:int = t%256 
    in (r,g,b) ;;

let to_int t  = 
  let (r, g, b) = to_rgb t in
  r * 65536 + g * 256 + b;;

let inverse t = 
  let (r, g, b) = to_rgb t in
  let r' = 255 - r in
  let g' = 255 - g in
  let b' = 255 - b in
  from_rgb r' g' b';;


let random () :int = 
  let r = Random.int(256) in 
  let g = Random.int(256) in 
  let b = Random.int(256) in 
  let col = r * 65536 + g * 256 + b in
  col;;


(** add 2 color component-wise: *)
(** the result is a valid color  *)
let (+) c1 c2 = 
  let (r1,g1,b1) = to_rgb c1 in
  let (r2,g2,b2) = to_rgb c2 in
  let r = min (r1 + r2) 255 in
  let g = min (g1 + g2) 255 in
  let b = min (b1 + b2) 255 in
  from_rgb r g b;;

let white = from_rgb 255 255 255;;
let black = from_rgb 0 0 0;;
let red   = from_rgb 255 0 0;;
let green = from_rgb 0 255 0;;
let blue  = from_rgb 0 0 255;;



let%test "idint" =
  let c = random () in
  to_int c = c;;

let%test "idrgb" =
  let c = random () in
  let (r,g,b) = to_rgb c in
  from_rgb  r g b = c;;

let%test "white" =
  let (r,g,b) = to_rgb white in
  (r = 255) && (g=255) && (b=255);;

let%test "black/white" = white = inverse black;;

let%test "whitecolors" = (red + green + blue) = white;;

let%test "addwhite" =
  white = white + white;;
