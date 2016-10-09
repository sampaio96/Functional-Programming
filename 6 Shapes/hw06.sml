use "lib.sml";

type point = int * int (* in Cartesian x-y coordinate *)

datatype shape = 
    Rect of point * point (* bottom-left and upper-right *)
  | Disc of point * int (* center and radius *)
  | Union of shape * shape
  | Without of shape * shape
  | Translate of shape * (int * int)
  | ScaleDown of shape * (int * int) (* x factor, y factor *)
  | ScaleUp of shape * (int * int) (* x factor, y factor *)

fun squared (x : int) : int = x * x

fun containsRect ((xmin,ymin) : point, (xmax,ymax) : point) ((x,y) : point) = 
    xmin <= x andalso x < xmax andalso
    ymin <= y andalso y < ymax

(* TASK *)

(* Purpose: contains(s,p) == true if p is in the shape, or false otherwise *)
fun contains (s : shape) ((x,y) : point) : bool = 
    case s of 
        Rect (ll,ur) => containsRect (ll, ur) (x,y)
      | Disc ((cx,cy),r) => squared(x-cx)+squared(y-cy) < r*r
      | Union(s1,s2) => contains s1 (x,y) orelse contains s2 (x,y)
      | Without (s1,s2) => contains s1 (x,y) andalso (not (contains s2 (x,y)))
      | Translate (s1,(xx,yy)) => contains s1 (x-xx,y-yy)
      | ScaleDown (s1,(xx,yy)) => contains s1 (x*xx,y*yy)
      | ScaleUp (s1,(xx,yy)) => contains s1 (x div xx,y div yy)


val Translation_test = Translate (Union(Rect((0,0),(100,100)),
                   Union(Rect((100,100),(200,200)),
                              Disc((100,100),40))),(10,10))

val ScaleDown_test = ScaleDown (Union(Rect((0,0),(100,100)),
                   Union(Rect((100,100),(200,200)),
                              Disc((100,100),40))),(10,10))
val ScaleUp_test = ScaleUp(Union(Rect((0,0),(100,100)),
                   Union(Rect((100,100),(200,200)),
                              Disc((100,100),40))),(10,10))

(* TASK *)
fun boundingbox (s : shape) : point * point =
    case s of 
        Rect (ll,ur) => (ll,ur)
      | Disc ((cx,cy),r) => ((cx-r,cy-r),(cx+r,cy+r))
      | Union(s1,s2) =>
                let val ((a,b),(c,d)) = boundingbox (s1) in
                      let val ((e,f),(g,h)) = boundingbox (s2) in 
                            ((Int.min(Int.min(a,e),Int.min(c,g)) , Int.min(Int.min(b,f),Int.min(d,h))) , (Int.max(Int.max(a,e),Int.max(c,g)), Int.max(Int.max(b,f),Int.max(d,h))))
                      end
                end
      | Without (s1,s2) => boundingbox (s1)
      | Translate (s1,(xx,yy)) => let val ((a,b),(c,d)) = boundingbox (s1) in ((a+xx,b+yy),(c+xx,d+yy)) end
      | ScaleDown (s1,(xx,yy)) => let val ((a,b),(c,d)) = boundingbox (s1) in ((a div xx,b div yy),(c div xx,d div yy)) end
      | ScaleUp (s1,(xx,yy)) => let val ((a,b),(c,d)) = boundingbox (s1) in ((a*xx,b*yy),(c*xx,d*yy)) end


val bowtie = Union(Rect((0,0),(100,100)),
                   Union(Rect((100,100),(200,200)),
                              Disc((100,100),40)))
(* to test before you've implemented the bounding box, do 
   writeshape(200,200, bowtie,"output2.bmp");
*)

val example = 
    let val b = Rect((100,50),(250,150))
        val t = Rect((250,120),(375,135))
        val h = Disc((100,175),60)
        val es = Rect((75,205),(125,250))
        val le = Disc((75,175),5)
        val re = Disc((125,175),5)
        val lre = Union(le, re)
    in 
        Union(Union(b, t),
              Without(h, Union(es,lre)))
    end
(* to test, do 
   writeshape(415,285, example,"output2.bmp");
*)

(* rectangle border with r as the inside border *)
fun rectb (r as ((minx,miny) : point, (maxx,maxy) : point),
           thickness : int) = 
    Without (Rect ((minx - thickness, miny - thickness),
                   (maxx + thickness, maxy + thickness)),
             Rect r)
(* 
use this to text boundingbox
val example_bb = Union (example , rectb(boundingbox example,25)) 
*)


val sierptri_box = Rect((0,0),(512,512))
val sierptri_example = ScaleUp(example,(8,8))
val sierptri_example2 = ScaleUp(Union(example,rectb(((40,50),(375,385)),4)),(2,2))

(* TASK *)
fun sierptri (base : shape) (n : int) : shape =
  case n of
    0 => base
    | _ => sierptri
  (let val ((a,b),(c,d)) = boundingbox (base) in
        Union(Union (base, Translate(base,(c-a,0))), Translate(base,((c-a) div 2,d-b)))
      end) (n-1)

(* prints the points (x,y) such that x is in [0,width] and y is in [0,height] *)
fun writeshape (width : int, height : int, s : shape, filename : string) : unit = 
    write_bitmap (width,height, contains s, filename) 
        
(* assumes that the all points in the shape are non-negative (otherwise you can Translate) *)
fun writeshape_bb (s : shape, filename : string) : unit = 
    let val ((minx,miny),(maxx,maxy)) = boundingbox s
    in 
        writeshape (maxx+minx,maxy+miny,s,filename) 
    end


fun sierpcarpet_helper (base : shape) (n : int) : shape =
   case n of 0 => base
            |_ => let val ((xmin,ymin),(xmax,ymax)) = boundingbox(base)
                      val column1 : shape = Union(base,
                                                  (Union(Translate(base,( (0,(ymax-ymin)) 
                                                                                         )), 
                                                         Translate(base,((0,(2*(ymax-ymin))))))))
                      val column2 : shape = Union(Translate(base,( ((xmax-xmin),0))),
                                                  Translate(base,( ((xmax-xmin),(2*(ymax-ymin))))) )
                      val column3 : shape = Union(Union(Translate(base, ((2*(xmax-xmin)),0)),
                                                        Translate(base, ((2*(xmax-xmin)),(ymax-ymin)))),
                                                     
                                                  Union(Translate(base, ((2*(xmax-xmin)),(2*(ymax-ymin)))),
                                                        Rect((0,0),(0,0))) )      
                     in sierpcarpet_helper (Union(column1,
                                      (Union(column2,column3))))
                                  (n-1) 
                      end

fun sierpcarpet (n : int) : shape =
   sierpcarpet_helper(Rect((0,0),(10,10))) (n)