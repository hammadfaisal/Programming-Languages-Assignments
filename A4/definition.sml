signature BIGINT =
sig
    type bigint
    exception invalid_bigint of string
    val fromInt : int -> bigint
    val add : bigint * bigint -> bigint
    val sub : bigint * bigint -> bigint
    val multiply : bigint * bigint -> bigint
    val divide : bigint * bigint -> bigint
    val modulo : bigint * bigint -> bigint
    val abs : bigint -> bigint
    val neg : bigint -> bigint
    val isZero : bigint -> bool
    val less : bigint * bigint -> bool
    val equal : bigint * bigint -> bool
    val fromString : string -> bigint
    val toString : bigint -> string
    val shiftLeft : bigint * int-> bigint
    val toInt : bigint -> int
end

(*representaion of 0 can be both (true,[]) or (false,[]) *)
structure BigInt : BIGINT =
struct 
    type bigint = bool * (int list)

    exception invalid_bigint of string

    (*converts from bigint to int assuming the number is small enough*)
    fun toInt (x : bigint) : int = 
        let
            val (sign, list) = x
            fun toInt' (x::xs, y) = toInt'(xs, 10*y + x)
                | toInt' ([], y) = y
        in
            if sign = true then
                toInt'(list,0)
            else
                ~(toInt'(list,0))
        end

    (*converts an int to bigint*)
    fun fromInt(x : int) : bigint = 
        let 
            fun fromInt' (x,y)  = 
                if x = 0 then
                    y
                else
                    fromInt'(x div 10,(x mod 10)::y)
        in
            if x < 0 then
                (false, fromInt'(~x,[]))
            else
                (true, fromInt'(x,[]))
        end

    (*adds p zeroes to the front of list*)
    fun pad(p : int, x : int list) : int list = 
        if p = 0 then
            x
        else
            0::pad(p - 1, x)

    (*makes size of both lists same by adding zeroes in front of smaller one*)
    fun normalize(x : int list, y: int list) : (int list * int list) = 
        if (length x) > (length y) then
            (x, pad ((length x) - (length y), y))
        else
            (pad ((length y) - (length x), x), y)

    (*removes zeroes from front of int list*)
    fun removezeros (x::xs) = 
        if x = 0 then
            removezeros xs
        else
            x::xs
        | removezeros [] = []

    (*adds two bigints*)
    fun add (x : bigint, y : bigint) : bigint = 
        let
            val (signx, list1) = x
            val (signy, list2) = y
            val (listx, listy) = normalize(list1, list2)
            fun add' ([],[]) = ([],0)
                | add' (x::xs, y::ys) = 
                    let
                        val (sum, carry) = add'(xs, ys)
                    in
                        (((x + y + carry) mod 10)::sum, (x + y + carry) div 10)
                    end
            
            fun sub' ([],[]) = ([],0)
                | sub' (x::xs, y::ys) = 
                    let
                        val (diff, carry) = sub'(xs, ys)
                    in
                        if (x - y - carry) < 0 then
                            ((x - y - carry + 10)::diff, 1)
                        else
                            ((x - y - carry)::diff, 0)
                    end

            fun tenscomp (x::xs) = 
            let
                val (tenscomp, carry) = tenscomp xs
            in
                (((9-x+carry) mod 10)::tenscomp, (9-x+carry) div 10)
            end
                | tenscomp [] = ([],1)
            
            
        in
            if (signx = signy) then
                (signx, removezeros(#1(add'(0::listx, 0::listy))))
            else
                if (signx = true) then
                    let 
                        val (diff, carry) = sub'(listx, listy)
                    in
                        if carry = 0 then
                            (true, removezeros(diff))
                        else
                            (false, removezeros(#1(tenscomp(removezeros(diff)))))
                    end
                else
                    let 
                        val (diff, carry) = sub'(listy, listx)
                    in
                        if carry = 0 then
                            (true, removezeros(diff))
                        else
                            (false, removezeros(#1(tenscomp(removezeros(diff)))))
                    end
        end
    
    (*subtracts a bigint from another*)
    fun sub (x : bigint, y : bigint) : bigint = add(x, (not(#1(y)), #2(y)))

    (*returns absolute value of a bigint*)
    fun abs (x : bigint) : bigint = (true, #2(x))

    (*changes sign of the bigint*)
    fun neg (x : bigint) : bigint = (not(#1(x)), #2(x))

    (*multiplies two bigints*)
    fun multiply (x : bigint, y : bigint) : bigint = 
        let
            val (signx, list1) = x
            val (signy, list2) = y
            val (listx, listy) = normalize(list1, list2)
            fun multiplyWithInt (x::xs, y) = 
                let
                    val (prod, carry) = multiplyWithInt(xs, y)
                in
                    (((x * y + carry) mod 10)::prod, (x * y + carry) div 10)
                end
                | multiplyWithInt ([], y) = ([], 0)
        
            (*multiplies x with each digit of y alongwith appropriate power of 10*)
            fun multiply' (x,[]) = (true,[])
                | multiply' (x, y::ys) =
                    let
                        val (prod, carry) = multiplyWithInt(x, y)
                    in
                        add((true,(carry::prod)@pad(length ys, [])), multiply'(x, ys))
                    end 
        in
            (signx = signy, removezeros(#2(multiply'(listx, listy))))
        end
    
    (*returns true if x is less than y*)
    fun less (x : bigint, y : bigint) : bool = #1(sub(x,y)) = false andalso #2(sub(x,y)) <> []

    (*returns true if x is equal to y*)
    fun equal (x : bigint, y : bigint) : bool = #2(sub(x,y)) = []

    (*returns true if x is zero*)
    fun isZero (x : bigint) : bool = removezeros(#2(x)) = []

    (*divides a bigint by another*)
    fun divide (x : bigint, y : bigint) : bigint = 
        let
            val (signx, list1) = x
            val (signy, list2) = y
            val (listx, listy) = (removezeros list1, removezeros list2)
            fun divide' (x, y) =
                if (less(x, y)) then (true,[])
                else if less(x, (true,#2(y) @ pad(length(#2(x))-length(#2(y)),[]))) then
                    add((true,1::pad(length(#2(x))-length(#2(y))-1,[])), divide'(sub(x, (true,#2(y) @ pad(length(#2(x))-length(#2(y))-1,[]))), y))
                else
                    add((true,1::pad(length(#2(x))-length(#2(y)),[])), divide'(sub(x, (true,#2(y) @ pad(length(#2(x))-length(#2(y)),[]))), y))

        in
            if (listy = []) then
                raise invalid_bigint "Division by zero"
            else
                (signx = signy, removezeros(#2(divide'((true,listx), (true,listy)))))
        end

    fun modulo (x : bigint, y : bigint) : bigint = sub(x, multiply(divide(x,y), y))

    (*helper function for fromString*)
    fun fromSt (x : string) : bigint = 
        let 
            val listx = explode x
            val sign = not (hd listx = #"~")
            fun fromString' (x::xs) = 
                if Char.isDigit x then
                    (Char.ord x - 48)::fromString'(xs)
                else
                    raise invalid_bigint "Invalid character in string"
                | fromString' [] = []
        in
            if ((hd listx) = #"~" orelse (hd listx) = #"+") then
                (sign, removezeros(fromString'(tl listx)))
            else
                (true, removezeros(fromString'(listx)))
        end
    
    (*converts a string to a bigint*)
    fun fromString(x : string) : bigint = 
        if (String.size(x) = 0) then
            (true,[])
        else
            fromSt(x)

    (*converts a bigint to a string*)
    fun toString (x : bigint) : string = 
        let
            val (sign, num) = x
        in
            if num = [] then
                "0"
            else if sign = false then
                "~" ^ implode (map (fn x => Char.chr (x + 48)) num)
            else
                implode (map (fn x => Char.chr (x + 48)) num)
        end

    (*shifts a bigint to left adding zeroes at the end. similar to multiplying by a power of 10*)
    fun shiftLeft (x : bigint, n : int) : bigint = 
        let
            val (sign, num) = x
        in
            (sign, num @ pad(n, []))
        end
end


signature RATIONAL =
sig
    type rational
    type bigint
    exception rat_error
    val make_rat: bigint * bigint -> rational option
    val rat: bigint -> rational option
    val reci: bigint -> rational option
    val neg: rational -> rational
    val inverse : rational -> rational option
    val equal : rational * rational -> bool (* equality *)
    val less : rational * rational -> bool (* less than *)
    val add : rational * rational -> rational (* addition *)
    val subtract : rational * rational -> rational (* subtraction *)
    val multiply  : rational * rational -> rational (* multiplytiplication *)
    val divide : rational * rational -> rational option (* divideision *)
    val showRat : rational -> string
    val showDecimal : rational -> string
    val fromDecimal : string -> rational
    val toDecimal : rational -> string
end

functor Rational (BigInt : BIGINT) : RATIONAL =
struct
    type bigint = BigInt.bigint
    type rational = bigint * bigint
    exception rat_error

    (*helper functions*)
    fun first(x, y) = x
    fun second(x, y) = y
    fun revlist ([], ys) = ys
        | revlist (x::xs, ys) = revlist (xs, x::ys)

    (*helper function to calculate gcd of two bigints*)
    fun gcd (x, y) = 
        if BigInt.isZero(y) then
            x
        else
            gcd(y, BigInt.modulo(x,y))

    fun make_rat (x, y) = 
        if BigInt.isZero(y) then
            NONE
        else
            let 
                val gc = gcd(BigInt.abs(x), BigInt.abs(y))
            in 
                if (BigInt.less(BigInt.fromInt(0), y) = BigInt.less(BigInt.fromInt(0), x)) then
                    SOME (BigInt.divide(BigInt.abs(x), gc), BigInt.divide(BigInt.abs(y), gc))
                else
                    SOME (BigInt.neg(BigInt.divide(BigInt.abs(x), gc)), BigInt.divide(BigInt.abs(y), gc))
            end

    fun rat (x) = make_rat(x, BigInt.fromInt(1))

    fun reci (x) = make_rat(BigInt.fromInt(1), x)

    fun neg (x) = (BigInt.neg(first(x)), second(x))

    fun inverse (x) = make_rat(second(x), first(x))

    (*rational can only be made through make_rat so is always stored in fractional normal form*)
    fun equal (x, y) = BigInt.equal(first(x), first(y)) andalso BigInt.equal(second(x), second(y))

    fun less (x, y) = BigInt.less(BigInt.multiply(first(x), second(y)), BigInt.multiply(first(y), second(x)))

    fun add (x, y) = 
        let
            val (num1, den1) = x
            val (num2, den2) = y
        in
            case make_rat(BigInt.add(BigInt.multiply(num1, den2), BigInt.multiply(num2, den1)), BigInt.multiply(den1, den2)) of
                NONE => raise rat_error
                | SOME z => z
        end

    fun subtract (x, y) =
        let
            val (num1, den1) = x
            val (num2, den2) = y
        in
            case make_rat(BigInt.sub(BigInt.multiply(num1, den2), BigInt.multiply(num2, den1)), BigInt.multiply(den1, den2)) of
                NONE => raise rat_error
                | SOME z => z
        end

    fun multiply (x, y) =
        let
            val (num1, den1) = x
            val (num2, den2) = y
        in
            case make_rat(BigInt.multiply(num1, num2), BigInt.multiply(den1, den2)) of
                NONE => raise rat_error
                | SOME z => z
        end
    
    fun divide (x, y) =
        let
            val (num1, den1) = x
            val (num2, den2) = y
        in
            case make_rat(BigInt.multiply(num1, den2), BigInt.multiply(den1, num2)) of
                NONE => NONE
                | SOME z => SOME z
        end

    fun showRat (x) =
        let
            val (num, den) = x
        in
            BigInt.toString(num) ^ "/" ^ BigInt.toString(den)
        end
    
    (*decimal normal form*)
    fun showDecimal (x) =
        let 
            fun done(x, []) = false
                | done(x, y::ys) = if (BigInt.equal(x,y)) then true else done(x, ys)

            fun findRecurrence (x,y, prev) =
                if (done(x, prev)) then
                    x::prev
                else
                    findRecurrence(BigInt.modulo(BigInt.multiply(x, BigInt.fromInt(10)), y), y, x::prev)
            
            fun convertToDecimal (y , [] ) = []
                | convertToDecimal (y, x::xs) = 
                    BigInt.toInt(BigInt.divide(BigInt.multiply(x,BigInt.fromInt(10)),y))::convertToDecimal(y, xs)

            fun convertToDecimal' (x::xs, y, recur, till) =
                if (BigInt.equal(x,recur)) then
                    implode(map (fn x => Char.chr (x + 48)) (convertToDecimal(y,revlist(till,[])))) ^ "(" ^ implode(map (fn x => Char.chr (x + 48)) (convertToDecimal(y,x::xs))) ^ ")"
                else
                    convertToDecimal' (xs, y, recur, x::till)
                | convertToDecimal' ([], y, recur, till) = raise rat_error
        
            val (num, den) = x
            val divi = findRecurrence(BigInt.modulo(BigInt.abs(num),den), den, [])
        in
            if (BigInt.less(num,BigInt.fromInt(0))) then
                "~" ^ BigInt.toString(BigInt.divide(BigInt.abs(num),den)) ^ "." ^ convertToDecimal' (revlist(tl(divi),[]), den, hd(divi), [])
            else
                BigInt.toString(BigInt.divide(BigInt.abs(num),den)) ^ "." ^ convertToDecimal' (revlist(tl(divi),[]), den, hd(divi), [])
        end

    fun toDecimal (x) = showDecimal(x)

    (*no spaces allowed*)
    fun fromDecimal (x) = 
        let 
            val listx = explode x
            val sign = not (hd listx = #"~")
            fun separate([],[]) = ("",[])
              | separate([],y) = (implode(revlist(y,[])), [])
              | separate(x::xs, []) = 
                if (x = #"(" orelse x = #")" orelse x = #".") then
                    ("", xs)
                else if (x = #"~") then
                    separate(xs, [])
                else
                    separate(xs, [x])
              | separate(x::xs, y) = 
                if (x = #"(" orelse x = #")" orelse x = #".") then
                    (implode(revlist(y,[])), xs)
                else if (x = #"~") then
                    separate(xs, y)
                else
                    separate(xs, x::y)
              

            val (i,nr) = separate(listx,[])
            val (n,rb) = separate(nr,[])
            val (r,waste) = separate(rb,[])
            val bii = BigInt.fromString(i)
            val bin = BigInt.fromString(n)
            val bir = BigInt.fromString(r)
            val biin = BigInt.add(BigInt.shiftLeft(bii, String.size n), bin)
            val biinr = BigInt.add(BigInt.shiftLeft(biin, String.size r), bir)
            val frat = make_rat(BigInt.sub(biinr,biin), BigInt.sub(BigInt.shiftLeft(BigInt.fromInt(1), String.size(n) + String.size(r)), BigInt.shiftLeft(BigInt.fromInt(1), String.size(n))))
        in
            case frat of
                NONE => raise rat_error
                | SOME z => if (sign) then z else neg(z)
        end

end


