type wartosc = 
    | Nonexistent
    | Single of float*float
    | Double of (float*float)*(float*float)

let wartosc_dokladnosc x p = 
    let result = 
        if x>=0.0 then Single ((x-.(p/.100.0*.x)),(x+.(p/.100.0*.x)))
           else Single ((x+.(p/.100.0*.x)),(x-.(p/.100.0*.x)))
    in
result

let wartosc_od_do x y = Single (x,y)

let wartosc_dokladna x = Single (x,x)

let in_wartosc x y = 
    match x with 
        | Double ((a,b),(c,d))-> if (a<=y && y<=b) || (c<=y && y<=d) then true else false
        | Nonexistent -> false
        | Single (a,b) -> if a<=y && y<=b then true else false


let min_wartosc x =
    match x with 
        | Double ((a,b),(c,d))-> a
        | Nonexistent -> nan
        | Single (a,b) -> a

let max_wartosc x = 
    match x with 
        | Double ((a,b),(c,d))-> d
        | Nonexistent -> nan
        | Single (a,b) -> b

let sr_wartosc x =
    match x with 
        | Double ((a,b),(c,d))-> nan
        | Nonexistent -> nan
        | Single (a,b) -> if a=neg_infinity && b=infinity then nan else (a+.b)/.2.0

let plus a b = 
    match a with 
        | Double ((q,w),(e,r))-> 
            (match b with 
                | Double ((t,y),(u,i))-> Single (neg_infinity,infinity) 
                | Nonexistent -> Nonexistent
                | Single (t,y) -> if w+.y>=e+.t then Single (neg_infinity,infinity) else Double  ((neg_infinity,(w+.y)),((e+.t),infinity))
            )
        | Nonexistent -> Nonexistent
        | Single (q,w) -> 
            (match b with 
                | Double ((t,y),(u,i))-> 
                    if y+.w>=u+.q then Single (neg_infinity,infinity) else Double  ((neg_infinity,(y+.w)),((u+.q),infinity))
                | Nonexistent -> Nonexistent
                | Single (t,y) -> Single ((q+.t),(w+.y))
            )

let minus a b = 
     match a with 
        | Double ((q,w),(e,r))-> 
            (match b with 
                | Double ((t,y),(u,i))-> Single (neg_infinity,infinity)
                | Nonexistent -> Nonexistent
                | Single (t,y) -> if (w-.t)>=(e-.y) then Single (neg_infinity,infinity) else Double ((neg_infinity,(w-.t)),((e-.y),infinity))
            )
        | Nonexistent -> Nonexistent
        | Single (q,w) ->
            (match b with 
                | Double ((t,y),(u,i))-> if (w+.y)>=(q+.u) then Single (neg_infinity,infinity) else Double ((neg_infinity,(w+.y)),((q+.u),infinity))
                | Nonexistent -> Nonexistent
                | Single (t,y) -> Single ((q-.y),(w-.t))
            )

let razy a b = 
    let special_comp l k = if ((compare l k)=0) then true else false (*Do porównywania nan*)
    in
    let max_val_two l k = (* największa wartosc z 2 floatow*)
        if special_comp l nan then k
            else if special_comp k nan then l
                else if l>k then l else k
    in
    let min_val_two l k = (* najmniejsza wartosc z 2 floatow*)
        if special_comp l nan then k
            else if special_comp k nan then l
                else if l<k then l else k
    in
    let max_val h j k l = (* największa wartosc z 4 floatow*)
        let para_a = if special_comp h nan then j (* wiekszy z h j*)
            else if special_comp j nan then h
                else if h> j then h else j
        in
        let para_b = if special_comp k nan then l (* wiekszy z k l*)
            else if special_comp l nan then k
                else if k>l then k else l
        in
    if (special_comp h nan && special_comp j nan) then max_val_two k l
        else if (special_comp k nan && special_comp l nan) then max_val_two h j
                else if para_a>para_b then para_a else para_b (* para zawsze bedzie rozna od nan!!!*)
    in
    let min_val h j k l = (* najmniejsza wartosc z 4 floatow*)
    let para_a = if special_comp h nan then j (* mniejszy z h j*)
            else if special_comp j nan then h
                else if h < j then h else j
        in
        let para_b = if special_comp k nan then l  (* mniejszy z k l*)
            else if special_comp l nan then k
                else if k<l then k else l
        in
    if (special_comp h nan && special_comp j nan) then min_val_two k l
        else if (special_comp k nan && special_comp l nan) then min_val_two h j
                else if para_a<para_b then para_a else para_b (* para zawsze bedzie rozna od nan!!!*)
    in
    let special_abs input = if input=(-0.0) then 0.0 else input (*nie wiedzialem ze w ocamlu jest cos takiego jak ujemne 0..., trzeba sie go pozbyc*)
    in

    match a with 
        | Double ((q,w),(e,r))-> 
            (match b with 
                | Double ((t,y),(u,i))-> if (max_val_two (w*.u) (e*.y))>=(min_val_two (w*.y) (e*.u)) then Single (neg_infinity,infinity) (*czy nie pelny?*)
                    else Double ((neg_infinity,special_abs(max_val_two (w*.u) (e*.y))),(special_abs(min_val_two (w*.y) (e*.u)),infinity))
                | Nonexistent -> Nonexistent
                | Single (t,y) -> if (t=0.0 && y=0.0) then Single (0.0,0.0) (*mnozenie zera*)
                    else if (t=neg_infinity && y=infinity) then Single (neg_infinity,infinity) (*mnozenie z -inf inf*)
                        else if (max_val_two (w*.t) (w*.y))>=(min_val_two (e*.t) (e*.y)) then Single (neg_infinity,infinity) (*czy nie pelny?*)
                            else Double ((neg_infinity,special_abs(max_val_two (w*.t) (w*.y))),(special_abs(min_val_two (e*.t) (e*.y)),infinity))
            )
        | Nonexistent -> Nonexistent
        | Single (q,w) -> 
            (match b with 
                | Double ((t,y),(u,i))-> if (q=0.0 && w=0.0) then Single (0.0,0.0) (*mnozenie zera*)
                    else if (q=neg_infinity && w=infinity) then Single (neg_infinity,infinity) (*mnozenie z -inf inf*)
                        else if (max_val_two (q*.y) (w*.y))>=(min_val_two (q*.u) (w*.u)) then Single (neg_infinity,infinity) (*czy nie pelny?*)
                            else Double ((neg_infinity,special_abs(max_val_two (q*.y) (w*.y))),(special_abs(min_val_two (q*.u) (w*.u)),infinity))
                | Nonexistent -> Nonexistent
                | Single (t,y) ->  
                    if ((q=0.0 && w=0.0) || (t=0.0 && y=0.0)) then Single (0.0,0.0) (*mnozenie zera*)
                        else if ((q=neg_infinity && w=infinity) || (t=neg_infinity && y=infinity)) then Single (neg_infinity,infinity) (*mnozenie z -inf inf*)
                            else Single (special_abs(min_val (q*.t) (q*.y) (w*.t) (w*.y)),special_abs(max_val (q*.t) (q*.y) (w*.t) (w*.y)))
            )


let podzielic a b = 
    let special_comp l k = if ((compare l k)=0) then true else false (*Do porównywania nan*)
    in
    let special_abs input = if input=(-0.0) then 0.0 else input (*nie wiedzialem ze w ocamlu jest cos takiego jak ujemne 0..., trzeba sie go pozbyc*)
    in
    let rozbij oo pp = (*dzieli przedzial na taki specjalny ktory bedzie latwiejszy do podzielenia*)
        if oo>=0.0 then ((nan,nan),(oo,pp))
            else if pp<=0.0 then ((oo,pp),(nan,nan))
                else ((oo,0.0),(0.0,pp))
    in
    let podziel_s_s oo pp = (*ta sa zmienione przedzialy, oo dzielimy przez pp, do Single/Single*)
        match oo with
            ((oa,ob),(oc,od)) ->  
                (match pp with
                    ((pa,pb),(pc,pd)) ->
                        let max_dod = if (pb=0.0 && not (special_comp oa nan)) || (pc=0.0 && not (special_comp od nan)) then infinity 
                            else if (special_comp (od/.pc) nan) then 
                                if (special_comp (oa/.pb) nan) then nan else (oa/.pb)
                            else if (special_comp (oa/.pb) nan) then (od/.pc)
                                else if (od/.pc)>(oa/.pb) then (od/.pc) else (oa/.pb)
                        in
                        let min_dod = 
                            if (special_comp (ob/.pa) nan) then
                                if (special_comp (oc/.pd) nan) then nan else (oc/.pd)
                            else if (special_comp (oc/.pd) nan) then (ob/.pa)
                                else if (ob/.pa)>(oc/.pd) then (oc/.pd) else (ob/.pa)
                        in
                        let max_uj = (*ta najbardziej z prawej*)  
                            if (special_comp (oc/.pa) nan) then
                                if (special_comp (ob/.pd) nan) then nan else (ob/.pd)
                            else if (special_comp (ob/.pd) nan) then (oc/.pa)
                                else if (oc/.pa)>(ob/.pd) then (oc/.pa) else (ob/.pd)
                        in
                        let min_uj = (*ta najbardziej z lewej*) 
                            if (pc=0.0 && not (special_comp oa nan)) || (pb=0.0 && not (special_comp od nan)) then neg_infinity
                                else if (special_comp (od/.pb) nan) then
                                    if (special_comp (oa/.pc) nan) then nan else (oa/.pc)
                                else if (special_comp (oa/.pc) nan) then (od/.pb)
                                    else if (oa/.pc)>(od/.pb) then (od/.pb) else (oa/.pc)
                        in
                    if (special_abs max_uj=0.0 && special_abs min_dod=0.0) then Single (min_uj,max_dod)
                        else if (special_comp max_dod nan) && (special_comp min_dod nan) then Single (min_uj,special_abs max_uj)
                            else if (special_comp min_uj nan) && (special_comp max_uj nan) then Single (special_abs min_dod,max_dod)
                                else Double ((min_uj,special_abs max_uj),(special_abs min_dod,max_dod))
                )
    in
    let dodaj a b = (*skleja przedziały, ale takie "ładne"*)
        match a with 
            | Double ((q,w),(e,r))-> 
                (match b with 
                    | Double ((t,y),(u,i))-> 
                        let max_L = if w>y then w else y in
                        let min_R = if e<u then e else u in
                            if max_L>min_R then Single (neg_infinity,infinity)
                            else Double  ((neg_infinity,max_L),(min_R,infinity))
                    | Nonexistent -> Nonexistent
                    | Single (t,y) -> if w>=t && y>=e then Single (neg_infinity,infinity)
                        else if w>=t then Double ((neg_infinity,y),(e,infinity))
                            else if y>=e then Double ((neg_infinity,w),(t,infinity))
                                else Double ((q,w),(e,r))
                )
            | Nonexistent -> Nonexistent
            | Single (q,w) -> 
                (match b with 
                    | Double ((t,y),(u,i))-> if y>=q && w>=u then Single (neg_infinity,infinity)
                        else if y>=q then Double ((neg_infinity,w),(u,infinity))
                            else if w>=u then Double ((neg_infinity,y),(q,infinity))
                                else Double ((t,y),(u,i))
                    | Nonexistent -> Nonexistent
                    | Single (t,y) -> 
                        if q=neg_infinity && y=infinity then 
                            if w>=t then Single (neg_infinity,infinity)
                            else Double ((neg_infinity,w),(t,infinity))
                        else 
                            if t<q then 
                                if w>y then Single (t,w) else Single (t,y)
                            else
                                if w>y then Single (q,w) else Single (q,y)
                )
    in
    match a with
        | Double ((q,w),(e,r)) -> 
            (match b with
                | Double ((t,y),(u,i)) -> Single (neg_infinity,infinity)
                | Nonexistent -> Nonexistent
                | Single (t,y) -> if (t=0.0 && y=0.0) then Nonexistent else 
                    dodaj (podziel_s_s (rozbij q w) (rozbij t y)) (podziel_s_s (rozbij e r) (rozbij t y))
            )
        | Nonexistent -> Nonexistent
        | Single (q,w) ->
            (match b with
                | Double ((t,y),(u,i)) -> if (q=0.0 && w=0.0) then Single(0.0,0.0) else 
                    dodaj (podziel_s_s (rozbij q w) (rozbij t y)) (podziel_s_s (rozbij q w) (rozbij u i))
                | Nonexistent -> Nonexistent
                | Single (t,y) -> if (t=0.0 && y=0.0) then Nonexistent
                    else if (q=0.0 && w=0.0) then Single(0.0,0.0) else podziel_s_s (rozbij q w) (rozbij t y)
            )