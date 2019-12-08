(** Punkt na płaszczyźnie *)
type point = float * float

(** Poskładana kartka: ile razy kartkę przebije szpilka wbita w danym punkcie *)
type kartka = point -> int

let eps = 0.000000001;;
let (=) a b = a -. eps < b && b < a +. eps;;

(** Zwraca iloczyn skalarny wektorow ab i cd *)
let iloczyn_skalarny ((ax, ay): point) ((bx, by): point) ((cx, cy): point) ((dx, dy): point) = 
	let (a, b, c, d) = (ax -. bx, ay -. by, cx -. dx, cy -. dy) in 
		begin
		(* Printf.printf "%f %f %f %f -> %f\n" a b c d ((a *. d) +. (b *. c)); *)
		(a *. c) +. (b *. d)
		end

(** Zwraca iloczyn wektorowy wektorow ab i cd *)
let iloczyn_wektorowy ((ax, ay): point) ((bx, by): point) ((cx, cy): point) ((dx, dy): point) = 
	let (a, b, c, d) = (ax -. bx, ay -. by, cx -. dx, cy -. dy) in 
		begin
		(* Printf.printf "%f %f %f %f -> %f\n" a b c d ((a *. d) -. (b *. c)); *)
		(a *. d) -. (b *. c)
		end

(** [prostokat p1 p2] zwraca kartkę, reprezentującą domknięty prostokąt
	o bokach równoległych do osi układu współrzędnych i lewym dolnym rogu [p1]
	a prawym górnym [p2]. Punkt [p1] musi więc być nieostro na lewo i w dół
	od punktu [p2]. Gdy w kartkę tę wbije się szpilkę wewnątrz
	(lub na krawędziach) prostokąta, kartka zostanie przebita 1 raz,
	w pozostałych przypadkach 0 razy *)
let prostokat ((ax, ay): point) ((cx, cy): point) : kartka = 
	fun ((x, y): point) -> 
		if ax <= x && x <= cx && ay <= y && y <= cy then 1 else 0

(** Zwraca kwadrat odleglosci pomiedzy punktami a i b na plaszczyznie *)
let odleglosc ((ax, ay): point) ((bx, by): point) = 
		(ax -. bx)*.(ax -. bx) +. (ay -. by)*.(ay -. by)

(** [kolko o r] zwraca kartkę, reprezentującą kółko domknięte o środku w 
		punkcie [o] i promieniu [r] *)
let kolko (o: point) r : kartka = 
	fun x -> if odleglosc o x <= r *. r then 1 else 0

(** [odbij_punkt one two v] zwraca punkt [v] odbity wzgledem prostej wyznaczonej 
	przez punkty [one] i [two] *)
let odbij_punkt (one: point) (two: point) (v: point) = 
	(* (* Printf.printf "odbij_punkt (%f, %f) (%f, %f) (%f, %f)\n" (fst one) (snd one) (fst two) (snd two) (fst v) (snd  v); *) *)
	let (x, y) = (fst two -. fst one, snd two -. snd one) in
	(* (* Printf.printf "(x, y) - (%f, %f)\n" x y; *) *)
	(* Printf.printf "%f <> %f\n" (iloczyn_skalarny v (0., 0.) two one) (iloczyn_skalarny v one two one); *)
	let mnoznik = 2. *. (iloczyn_skalarny v one two one) in
	(* (* Printf.printf "mnoznik1 - %f\n" mnoznik; *) *)
	let mnoznik = mnoznik /. (iloczyn_skalarny two one two one) in
	(* (* Printf.printf "mnoznik2 - %f\n" mnoznik; *) *)
	let wynik = (mnoznik *. x, mnoznik *. y) in
		begin
		(* (* Printf.printf "(%f, %f)\n" (fst wynik) (snd wynik); *) *)
		(fst wynik -. fst v +. (2. *. fst one), snd wynik -. snd v +. (2. *. snd one))
		end

(** [zloz p1 p2 k] składa kartkę [k] wzdłuż prostej przechodzącej przez
	punkty [p1] i [p2] (muszą to być różne punkty). Papier jest składany
	w ten sposób, że z prawej strony prostej (patrząc w kierunku od [p1] do [p2])
	jest przekładany na lewą. Wynikiem funkcji jest złożona kartka. Jej
	przebicie po prawej stronie prostej powinno więc zwrócić 0.
	Przebicie dokładnie na prostej powinno zwrócić tyle samo,
	co przebicie kartki przed złożeniem. Po stronie lewej -
	tyle co przed złożeniem plus przebicie rozłożonej kartki w punkcie,
	który nałożył się na punkt przebicia. *)
let zloz (one: point) (two: point) (a: kartka) : kartka = 
	fun (v: point) ->
		(* (* Printf.printf "iloczyn -> %f\n" (iloczyn_wektorowy two one v one); *) *)
		if iloczyn_wektorowy two one v one < 0. then
			begin
			(* print_endline "zla strona"; *)
			0
			end
		else
			begin
			(* (* Printf.printf "(%f, %f) (%f, %f)\n" (fst one) (snd one) (fst two) (snd two); *) *)
			(* (* Printf.printf "(%f, %f) -> %d\n" (fst (odbij_punkt one two v)) (snd (odbij_punkt one two v)) (a (odbij_punkt one two v)); *) *)
			(* (* Printf.printf "%d + %d\n" (a v) (a (odbij_punkt one two v)); *) *)
			(a v) + if iloczyn_wektorowy two one v one = 0. then 0 else (a (odbij_punkt one two v))
			end

(** [skladaj [(p1_1,p2_1);...;(p1_n,p2_n)] k = zloz p1_n p2_n (zloz ... (zloz p1_1 p2_1 k)...)]
	czyli wynikiem jest złożenie kartki [k] kolejno wzdłuż wszystkich prostych
	z listy *)
let skladaj (lista: (point * point) list) kartka = 
	List.fold_left (fun acc (a, b) -> zloz a b acc) kartka lista




