datatype 'a AVLTree = Nil | Br of ((int*('a))*('a AVLTree)*('a AVLTree));


local

datatype order = LESS | EQUAL | GREATER

fun compare(x,y) = if (x<y) then LESS else (if x=y then EQUAL else GREATER)

fun max(a, b) = if a > b then a else b;

fun node_height (Nil) = 0
		| node_height (Br ((key, data), left, right)) = 1 + max(node_height(left), node_height(right));

fun is_search_tree(Nil , _,_) = true
	 |is_search_tree (Br ((key, data), left, right), min , max) = if ( (key<max) andalso (key>min))
				then (if ((is_search_tree(left,min , key)) andalso (is_search_tree(right,key,max))) then true else false)
				else false ;
				fun rotate_left node =
				 case node of Br((x),A,Br(y,B,C)) => (Br(y,Br(x,A,B),C))
																		| _ => node;


				fun rotate_right node =
				 case node of Br((y),Br(x,A,B),C) => (Br(x,A, Br(y,B,C)))
																		|	_ => node;


				fun rotate_lr (Br ((key, data), left , right )) =
							rotate_right((Br ((key, data), (rotate_left(left)) , right )));


				fun rotate_lr (Br ((key, data), left , right )) =
							rotate_right((Br ((key, data), (rotate_left(left)) , right )));



				fun rotate_rl (Br((key,data), left , right) ) =

					rotate_left( Br((key,data), left, rotate_right(right ) ) );



				fun get_key (Nil) = 0
					 |get_key(node) = case node of Br((key,_) , _ , _) => key;

fun is_tree_balanced (Br ((key, data), left, right)) = if ((abs(node_height(left) - node_height(right)) <= 1) andalso
				(is_tree_balanced(left) = true) andalso
				 (is_tree_balanced(right) = true))  then true
				  else false
	| is_tree_balanced (Nil) = true;

fun is_node_balanced (x) = if abs(x)<=1 then true else false;


	fun balance_node ((Br(x, left, right)), new_key) =
	let

			val key1 = get_key(left)
			val key2 = get_key(right)
			val subtree_left_height = node_height(left)
			val subtree_right_height = node_height(right)
			val balance = subtree_left_height - subtree_right_height

	in
		if (is_node_balanced(balance)) then Br(x, left, right)

		else ( if (balance > 1 andalso new_key < key1)
		then (rotate_right((Br(x, left, right))))

		else(if( balance < (~1) andalso new_key > key2)
				then (rotate_left((Br(x, left, right))))

		else(if (balance>1 andalso new_key > key1)
				then (rotate_lr((Br(x, left, right))))

				else (rotate_rl((Br(x, left, right)))))))
	end;

	in

fun legalAVL(Nil) = true
		|legalAVL (Br ((key, data), left, right)) =  is_tree_balanced(Br ((key, data), left, right));

		fun insert ((key2,data2), Nil)  = Br ((key2,data2), Nil, Nil)
			| insert ((key2,data2), (Br ((key1,data1), left, right))) =
					case compare (key2,key1) of
										EQUAL => ((Br ((key1,data1), left, right)))
									| GREATER => balance_node((Br ((key1,data1), left, insert ((key2,data2), right))), key2)
									| LESS => balance_node((Br ((key1,data1), insert ((key2,data2), left) , right)), key2);


end;

(* legalAVL (Br((3,1),Nil,Nil)); *)

(* legalAVL (Br((3,4), Br((2,1), Br((1,0), Nil, Nil), Nil), Nil)); *)

(* YEEEEEEEEEEEEEEESSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSHHHHHHHHHHHHHHHHHHHHHH *)
(* DOOOOOOOONE *)

(*
fun p (k) = print (Int.toString k);

fun printTree t p =
  let
    fun spaces n =  if n = 0 then "" else " " ^ (spaces (n - 1))
    fun helper t p n  =
      case t of
        Nil => print ((spaces n) ^ "Nil\n")
      | Br((k,v), l, r) => (helper r p (n + 2);
                          print (spaces n); p k; print "\n";
                          helper l p (n + 2))
   in
   helper t p 0
  end;
	*)
(*
insert((3,0) , Nil);

insert((4,5), it);

insert ((9,5), it);

insert((7,0),it);

insert((8,0),it);

insert((13,0) , Nil);

insert((17,5), it);

insert ((~23,5), it);

insert((67,0),it);

insert((~3,0),it);*)
(*

insert((123,0) , Nil);

insert((43,5), it);

insert ((78,5), it);

insert((98,0),it);

insert((~24,0),it);
*)

(*printTree it p;*)

(*
	if (abs(balance) <= 1) then (Br(x, left, right))
	    else ( if (balance>1 andalso new_key < key1)
	    then rotate_right( (Br(x, left, right))

      else (  if( balance<(~1) andalso new_key > key2)
			then (rotate_left ((Br(x, left, right)))

			else ( if (balance>1 andalso new_key > key1)
			then (rotate_lr((Br(x, left, right))))

			else (Br(x, left, right))))))) *)




	(* fun balance_node (Br((key,data), Br((key1,val1), B, C), Br((key2,val2), D, E))) =
	let
			val balance = node_height(Br((key1,val1), B, C)) - node_height(Br((key2,val2), D, E))

	in

	  if (abs(balance) <= 1) then (Br((key,data), Br((key1,val1), B, C), Br((key2,val2), D, E))) else ( if (balance>1 andalso key < key1)  then
		 rotate_right( (Br((key,data), Br((key1,val1), B, C), Br((key2, val2), D, E))))
	else (  if( balance<(~1) andalso key>key2)
				then (rotate_left (Br((key,data), Br((key1,val1), B, C), Br((key2,val2), D, E))))
				else ( if (balance>1 andalso key>key1)
							then (rotate_lr((Br((key,data), Br((key1,val1), B, C), Br((key2,val2), D, E)))))
							else (rotate_rl((Br((key,data), Br((key1,val1), B, C), Br((key2,val2), D, E))))))))
	end; *)
