(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita
	w danym punkcie *)
type kartka = point -> int

(** Epsilon oraz funkcja do sprawdzania rownosci float'ow*)
let eps = 1e-8
let compare_eps a b = a -. eps < b && b < a +. eps


(** Zwraca iloczyn skalarny wektorow ab i cd *)
let iloczyn_skalarny (ax, ay) (bx, by) (cx, cy) (dx, dy) = 
	let (a, b, c, d) = (ax -. bx, ay -. by, cx -. dx, cy -. dy) in 
		(a *. c) +. (b *. d)


(** Zwraca iloczyn wektorowy wektorow ab i cd *)
let iloczyn_wektorowy (ax, ay) (bx, by) (cx, cy) (dx, dy) = 
	let (a, b, c, d) = (ax -. bx, ay -. by, cx -. dx, cy -. dy) in 
		(a *. d) -. (b *. c)


(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
	o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
	a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
	od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
	(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
	w pozostałych przypadkach 0 razy *)
let prostokat ((ax, ay): point) ((cx, cy): point) : kartka = 
	fun ((x, y): point) -> 
		if ax -. eps <= x && x <= cx +. eps && ay -. eps <= y && y <= cy +. eps
			then 1
		else 0


(** Zwraca odleglosc pomiedzy punktami a i b na plaszczyznie *)
let odleglosc ((ax, ay): point) ((bx, by): point) = 
	let (diff_x, diff_y) = (ax -. bx, ay -. by) in
		sqrt (diff_x *. diff_x +. diff_y *. diff_y)


(** [kolko o r] zwraca kartkę, reprezentującą kółko domknięte o środku w 
		punkcie [o] i promieniu [r] *)
let kolko (o: point) r : kartka = 
	fun x -> if odleglosc o x <= r +. eps then 1 else 0


(** [odbij_punkt one two v] zwraca punkt [v] odbity wzgledem prostej
	wyznaczonej przez punkty [one] i [two] *)
let odbij_punkt (one: point) (two: point) (v: point) = 
	let (x, y) = (fst two -. fst one, snd two -. snd one) in
	let mnoznik = 2. *. (iloczyn_skalarny v one two one) in
	let mnoznik = mnoznik /. (iloczyn_skalarny two one two one) in
	let wynik = (mnoznik *. x, mnoznik *. y) in
		(fst wynik -. fst v +. (2. *. fst one),
		 snd wynik -. snd v +. (2. *. snd one))


(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
	punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
	w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do
	[p2]) jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
	przebicie po prawej stronie prostej powinno więc zwrócić 0.
	Przebicie dokładnie na prostej powinno zwrócić tyle samo,
	co przebicie kartki przed złożeniem. Po stronie lewej -
	tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
	który nałożył się na punkt przebicia. *)
let zloz (one: point) (two: point) (a: kartka) : kartka = 
	fun (v: point) ->
		let wekt = iloczyn_wektorowy two one v one in
			if compare_eps wekt 0. then a v
			else if  iloczyn_wektorowy two one v one < 0. then 0
			else (a v) + (a (odbij_punkt one two v))


(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n
	(zloz ... (zloz p1_1 p2_1 k)...)]
	czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
	z listy *)
let skladaj (lista: (point * point) list) kartka = 
	List.fold_left (fun acc (a, b) -> zloz a b acc) kartka lista



(** Testy *)
let k = prostokat (0., 0.) (42., 29.);;
let k = skladaj [(15., 2.), (27., 26.); (15., 15.), (1., 27.); (4., 18.), (3., 18.)] k;;

assert ((k (11., 3.)) = 1);;
assert ((k (7., 1.)) = 1);;
assert ((k (-4., 6.)) = 1);;
assert ((k (2., 3.)) = 2);;
assert ((k (14., 6.)) = 2);;
assert ((k (-1., 14.)) = 2);;
assert ((k (5., 2.)) = 3);;
assert ((k (13., 8.)) = 4);;
assert ((k (1., 10.)) = 4);;
assert ((k (6., 15.)) = 8);;
assert ((k (3., 17.)) = 8);; 


let k = prostokat (-21., -14.5) (21., 14.5);;
let k = skladaj [(11., -17.), (11., 2.); (-3., 3.5), (-7., 8.5); (-14., -9.5), (-4., -7.5)] k;;

assert ((k (0., 10.)) = 0);;
assert ((k (-6., 6.5)) = 2);;
assert ((k (-16., -9.5)) = 2);;
assert ((k (-14., -1.5)) = 3);;
assert ((k (-13., -6.5)) = 4);;
assert ((k (3., 0.)) = 4);;
assert ((k (-1.5, -1.)) = 4);;
assert ((k (-3., -5.)) = 6);;
assert ((k (-0.7, -1.9)) = 7);;
assert ((k (2., -4.5)) = 8);;
