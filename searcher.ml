open String;;

let reslist = ref [||];;
let docwordcounts = ref [];;
let totaldocs = ref 0;;

let spcindex s = 
	let indx = ref (-1) and l = length s in
		for i = 0 to l-1 do
			if s.[i] = ' ' && !indx = -1 then
				indx:= i
		done; !indx

let getsub lst =
	let retlist = ref [] and l = List.length lst in
	for i = 0 to l-2 do 
		retlist:= !retlist@ [List.nth lst i]
	done; !retlist

let insert infon word docid wordid iter=
	let found = ref false and iter2 = ref 0 in
		while not (!found) do
			try
				let el = List.nth infon !iter2 in
				match el with
				|(did,indexes) -> if did = docid then
									(!reslist.(iter)<-(word,((getsub infon)@[(docid,indexes@[wordid])])) ; found:= true)
									else iter2:=!iter2 +1 
			with e->
				(!reslist.(iter)<-(word,infon@[docid,[wordid]]) ; found:=true)
		done

let invertindex word docid wordindx =
	let found = ref false and iter1 = ref 0 in
		while not (!found) do
			try
			let el = !reslist.(!iter1) in
				match el with
				| (wrd,info) -> if word = wrd then (insert info word docid wordindx !iter1; found:=true)
								else iter1:= !iter1 +1
			with e -> reslist:= Array.append !reslist [|(word,[(docid,[wordindx])])|] ; found:= true
		done

let docsearch doc docid=
	let str = ref doc and wordcount = ref 0 in
		let i = ref (spcindex !str) in
			while not (!i = (-1)) do
				let l = length !str in
				if !i = 0 && !i<>l-1 then
					(str:=sub !str 1 (l-1);
					i:= spcindex !str)
				else if !i=l-1 then
					((invertindex (lowercase (sub !str 0 !i)) docid !wordcount); i:= -1)
				else
					((invertindex (lowercase (sub !str 0 !i)) docid !wordcount) ; (str:= sub !str (!i+1) (l- !i -1)); i:= spcindex !str; wordcount:= !wordcount+1)
			done;docwordcounts:= !docwordcounts@[!wordcount +1] ; totaldocs:= !totaldocs+1

let parse str =
	let findoc = ref str and l = length str and sep = [',';'\n';'.'] in
	for i = 0 to l-1 do
		if List.mem str.[i] sep then
			findoc:=(sub !findoc 0 i)^" "^(sub !findoc (i+1) (l-i-1))
	done;!findoc

let search doclist =
	let l = List.length doclist in
		for i = 0 to l -1 do
			let doc = List.nth doclist i in
			docsearch (parse doc) i
		done


let sorter a b =
	match (a,b) with
	| (wrd1,_),(wrd2,_) -> if wrd1> wrd2 then 1
							else if wrd1< wrd2 then -1
						else 0

let breakuptofive str = 
	let i = String.index str '.' and l = String.length str in
	if l-1>6 then
		sub str 0 (i+6)
	else
		str

let indxtostr indxlist d=
	let f = ref true and tot = ref 0 and retstr = ref "" in
	while !f do
	try
		retstr:= !retstr^(string_of_int (List.nth indxlist !tot))^";" ;tot:= !tot+1 
	with e -> let tf = float_of_int !tot /. float_of_int (List.nth !docwordcounts d) in
				(retstr:=((string_of_float tf))^":["^(String.sub !retstr 0 (length !retstr -1))^"]),";f:=false)
	done; !retstr

let infoToStr resarr =
	Array.sort sorter resarr;
	let l = Array.length resarr and resstr = ref "" in
	for i = 0 to l-1 do
		let tempstr = ref "" in 
		let el = resarr.(i) in
			match el with
			| (word, info) -> let docnum = List.length info in
								let tempsub = ref "[" in
								for i = 0 to docnum-1 do
									let totalindexes= ref 0 in
										match List.nth info i with
										| d, indx -> tempsub:= !tempsub^"("^(string_of_int d)^":"^(indxtostr indx d)
								done;
								let idf = (log (float_of_int !totaldocs /. float_of_int docnum)) /. (log 10.0) in
								tempstr:= ((string_of_float idf))^" "^(sub !tempsub 0 (length !tempsub -1))^"]";
								resstr:= !resstr^word^" "^ !tempstr^"\n"
	done;!resstr

let rec parsefile in_channel doc_list=
	try
		let s=input_line in_channel in
	parsefile in_channel (s::doc_list)
	with End_of_file-> doc_list;;

let read_filedoc in_file=let in_channel = open_in in_file in List.rev (parsefile in_channel []);;

open Printf;;

let () =
	 let ic = Sys.argv.(1) and oc = open_out Sys.argv.(2) in
	 let documents = read_filedoc ic in
	 search documents ; fprintf oc "%s" (infoToStr !reslist)