fun multiply([] , n) = ((n*n) div 10, [(n*n) mod 10])
    | multiply(x::xl, n) =
        let
            val (carry, yl) = multiply(xl, n);
            val (carry2, y) = ((x * n + carry) div 10, (x * n + carry) mod 10);
        in
            (carry2, y::yl)
        end;

fun conc (x,[]) = if (x = 0) then [] else [x]
    | conc (x, a::al) = 
    if (x = 0) then conc(a, al)
    else x::a::al



fun smalloreq (a, b) =
    if (length a > length b) then false
    else if (length b > length a) then true
    else
        let
            fun small_help ([],[]) = true
            | small_help (a::al, b::bl) =
                if (a > b) then false
                else if (a < b) then true
                else small_help(al, bl);  
        in
            small_help(a, b)
        end

fun adder([],[],0) = (0,[])
    | adder(a::al, b::bl, x) =
        if (x > 0) then 
            let 
                val (carry, yl) = adder(al, b::bl, x-1);
                val (carry2, y) = ((a + carry) div 10, (a + carry) mod 10);
            in
                (carry2, y::yl)
            end
        else
            let
                val (carry, yl) = adder(al, bl, 0);
                val (carry2, y) = ((a + b + carry) div 10, (a + b + carry) mod 10);
            in
                (carry2, y::yl)
            end

fun add(a,[]) = a
    | add([],b) = b
    | add(a,b) = 
        if (length a > length b) then conc(adder(a, b, length a - length b))
        else conc(adder(b, a, length b - length a))

fun finddigit(a, b , c) =
    if (smalloreq(conc(multiply(a,b)),c)) then b
    else finddigit(a, b-1, c)

fun sub(a,b) = add(a, map(fn x => ~x) b)

fun sqroot(a,b, []) = ([],b)
    | sqroot (a, b, c1::c2::c) =
        let 
            val curq = conc(0,b@[c1,c2]);
            val d = finddigit(a, 9, curq);
            val rem = sub(curq,conc(multiply(a,d)));
            val (ans, frem) = sqroot(add(a@[d],[d]),rem,c);
        in
            (d::ans, frem)
        end

fun ltos(l) = 
    let
        val x = implode(map(fn x => chr(x+48)) l )
    in
        if (x = "") then "0" else x
    end

fun isqrtld (s : string) =
    let 
        val ls = conc(0,map(fn x => (ord x) - 48) (explode s));
    in
        if (length(ls) mod 2 = 1) then 
            let
                val maybe = finddigit([], 9, [hd ls]);
                val (ans, rem) = sqroot(add([maybe],[maybe]), conc(0,[(hd ls) - (maybe*maybe)]), tl ls);
            in
                (ltos(maybe::ans), ltos(rem))
            end
        else 
            let
                val (ans, rem) = sqroot([], [], ls);
            in
                (ltos(ans), ltos(rem))
            end
    end

        




