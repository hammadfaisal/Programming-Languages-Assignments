(*create markdown to html translator in SML*)

(*handles bold and italics*)
fun astHandler(ac, b, i) =
    if ac = 0 then (b,i,"")
    else if ac = 1 then
        if i = false then
            (b,true,"<i>")
        else
            (b,false,"</i>")
    else if ac = 2 then
        if b = false then
            (true,i,"<b>")
        else
            (false,i,"</b>")
    else if ac = 3 then
        if i = false then
            if b = false then
                (true,true,"<b><i>")
            else
                (false,true,"</b><i>")
        else
            if b = false then
                (true,false,"<b></i>")
            else
                (false,false,"</b></i>")
    else
        raise Fail "Redundant or Invalid asterisks"

(*separates char list till the given character to find link*)
fun findTill ([],ch) = raise Fail "Link does not have closing bracket"
    | findTill (x::xs,ch) = 
        if x = ch then
            ([],xs)
        else
            let
                val (a,b) = findTill(xs,ch)
            in
                (x::a,b)
            end

(*finds link corresponding to a label*)
fun associate (a, []) = raise Fail "Label does not have corresponding link"
    | associate (a, (x::xs)) =
        let
            val (b,c) = x
        in
            if a = b then
                c
            else
                associate(a,xs)
        end

(*inline parsing. checks for bold italics underlines links*)
fun parseText(links,esc, u, ac, b, i, []) = 
    let
        val (b1,i1,txt) = astHandler(ac, b, i)
    in
        if b1 orelse i1 then
            raise Fail "Bold or italics not closed"
        else
            if u then
                [txt, "</u>"]
            else
                [txt]
    end
    | parseText(links,esc, u, ac, b, i, (x::xs)) = 
        let 
            val (b1,i1,txt) = astHandler(ac, b, i)
        in 

        if esc then
            (str x)::(parseText (links,false, u, 0, b, i, xs))
        else if x = #"*" then
            if ac = 0 then
                parseText(links,false, u, 1, b, i, xs)
            else if ac = 1 then
                parseText(links,false, u, 2, b, i, xs)
            else if ac = 2 then
                parseText(links,false, u, 3, b, i, xs)
            else
                raise Fail "Reduntant or Invalid asterisk"
        else if x = #"_" then
            if u = false then
                txt::"<u>"::(parseText(links,false, true, 0,b1, i1, xs))
            else
                txt::(parseText(links,false, true, 0,b1, i1, xs))
        else if x = #"\\" then    
            txt::(parseText(links,true, u,0, b1, i1, xs))
        else if x = #" " then
            if u = false then
                txt::" "::(parseText(links,false, u, 0, b1, i1, xs))
            else
                txt::"</u> "::(parseText(links,false, u, 0, b1, i1, xs))
        else if  (x = #"<" andalso ((String.isPrefix "http://" (implode(xs))) orelse (String.isPrefix "https://" (implode(xs)))))  then
            let
                val (a,b) = findTill (xs, #">")
                val link = implode(a)
            in
                txt::"<a href=\""^link^"\">"^link^"</a>"::(parseText(links,false, u, 0, b1, i1, b))
            end
        else if x = #"[" then
            let
                val (a,b) = findTill (xs, #"]")
                val linkText = implode(a)
            in
                if String.isPrefix "(" (implode(b)) then
                    let
                        val (c,d) = findTill (tl b, #")")
                        val link = implode(c)
                    in
                        txt::"<a href=\""^link^"\">"^linkText^"</a>"::(parseText(links,false, u, 0, b1, i1, d))
                    end
                else if String.isPrefix "[" (implode(b)) then
                    let
                        val (c,d) = findTill (tl b, #"]")
                        val link = implode(c)
                    in
                        txt::"<a href=\""^(associate(link,links))^"\">"^linkText^"</a>"::(parseText(links,false, u, 0, b1, i1, d))
                    end
                else if String.isPrefix " [" (implode(b)) then
                    let
                        val (c,d) = findTill (tl (tl b), #"]")
                        val link = implode(c)
                    in
                        txt::"<a href=\""^(associate(link,links))^"\">"^linkText^"</a>"::(parseText(links,false, u, 0, b1, i1, d))
                    end
                else
                    txt::"["::(parseText(links,false, u, 0, b1, i1, xs))
            end
        else if x = #"\n" then
            if u = false then
                txt::(parseText(links,false, u, 0, b1, i1, xs))
            else
                txt::"</u>"::(parseText(links,false, u, 0, b1, i1, xs))

        else
            txt::(str x)::(parseText(links,false, u, 0, b1, i1, xs))
        end    



(*Header*)
fun headerCheck (links,s) = 
        let
            val numHashes = String.size (hd (String.tokens (fn c => c <> #"#") s))
            val header = concat(parseText(links,false,false,0,false,false,explode(String.substring(s, numHashes, String.size s - numHashes))))
        in
            if numHashes = 1 then
                "<h1>" ^ header ^ "</h1>"
            else if numHashes = 2 then
                "<h2>" ^ header ^ "</h2>"
            else if numHashes = 3 then
                "<h3>" ^ header ^ "</h3>"
            else if numHashes = 4 then
                "<h4>" ^ header ^ "</h4>"
            else if numHashes = 5 then
                "<h5>" ^ header ^ "</h5>"
            else if numHashes = 6 then
                "<h6>" ^ header ^ "</h6>"
            else
                raise Fail "Invalid header"
        end
    
(*checks if string is empty*)    
fun emp s = 
    if String.tokens (fn c => c = #" ") s = [] then
        true
    else
        false

(*checks if it is an unordered list*)
fun ulcheck x =
    if String.isPrefix "- " x then
        0
    else if String.isPrefix " - " x then
        1
    else if String.isPrefix "  - " x then
        2
    else if String.isPrefix "   - " x then
        3
    else 
        ~1

(*checks if it is an ordered list*)
fun olcheckhelp(sp,num,dot,y::ys) =
    if (y >= #"0" andalso y <= #"9") then
        olcheckhelp(sp,true,false,ys)
    else if (num = true andalso y = #".") then
        olcheckhelp(sp,false,true,ys)
    else if (dot = true andalso y = #" ") then
        sp
    else if ((sp > 3) orelse num = true) then
        ~1
    else if (y = #" ") then    
        olcheckhelp(sp+1,false,false,ys)
    else 
        ~1
    | olcheckhelp(sp,num,dot,[]) = ~1

fun olcheck x =
        olcheckhelp(0,false,false,explode x)

(*removes atmost ign spaces from start*)
fun remign (ign, [])= []
    | remign (ign, (x::xs)) =
        if ((ign = 0) orelse (x <> #" "))then
            x::xs
        else
            remign ((ign-1), xs)

(*extracts the list from the text*)
fun ulfinder (ign,pemp, []) = ([],[])
    | ulfinder (ign,pemp,x::xs) =
        let 
            val x2 = remign(ign,explode(x))
            val ulx2 = ulcheck(implode x2)
        in
        if (emp(x) = false andalso pemp = true andalso (hd x2) <> #" " andalso ulx2 = ~1) then
            ([],x::xs)
        else 
            let
                val (ul, rest) = ulfinder (ign,emp x, xs)
            in
                (implode(x2)::ul, rest)
            end
        end

(*extracts the list from the text*)
fun olfinder (ign,pemp, []) = ([],[])
    | olfinder (ign,pemp,x::xs) =
        let 
            val x2 = remign(ign,explode(x))
            val olx2 = olcheckhelp(0,true,false,x2)
        in
        if (emp(x) = false andalso pemp = true andalso (hd x2) <> #" " andalso olx2 = ~1) then
            ([],x::xs)
        else 
            let
                val (ol, rest) = olfinder (ign,emp x, xs)
            in
                (implode(x2)::ol, rest)
            end
        end

(*extracts the blockquote from the text*)
fun bqfinder ([]) = ([],[])
    | bqfinder (x::xs) =
        if String.isPrefix ">" x then
            let
                val (bq, rest) = bqfinder xs
            in
                (String.extract(x,1,NONE)::bq, rest)
            end
        else
            ([],x::xs)

(*parses codeblock*)
fun codehandler ([]) = []
    | codehandler (x::xs) =
        if x = #"&" then
            explode("&amp;")@(codehandler xs)
        else if x = #"<" then
            explode("&lt;")@(codehandler xs)
        else if x = #">" then
            explode("&gt;")@(codehandler xs)
        else
            x::(codehandler xs)

(*extracts the codeblock from the text*)
fun cbfinder ([]) = ([],[])
    | cbfinder (x::xs) =
        if String.isPrefix "    " x then
            let
                val (cb, rest) = cbfinder xs
            in
                ((implode(codehandler(explode(String.extract(x,4,NONE)))))::cb, rest)
            end
        else
            ([],x::xs)

(*extracts the table from the text*)
fun tablefinder([]) = raise Fail "Table Not Closed"
    | tablefinder(x::xs) =
        if String.isPrefix ">>" x then
            ([],xs)
        else
            let
                val (table, rest) = tablefinder xs
            in
                (x::table, rest)
            end

(*remove prefix of string till dot*)
fun remTillDot s = 
    let
        fun remTillDotHelp (x::xs) =
            if x = #"." then
                xs
            else
                remTillDotHelp xs
    in
        implode(remTillDotHelp(explode s))
    end

(*reverses list*)
fun revlist ([], ys) = ys
    | revlist (x::xs, ys) = revlist (xs, x::ys)

(*removes intial empty strings from list*)
fun remEmp ([]) = []
    | remEmp (x::xs) =
        if emp x then
            remEmp xs
        else
            x::xs

(*parses table*)
fun tablehandler (links,[]) = []
    |   tablehandler (links,x::xs) = 
        let
            val elems = String.fields (fn c => c = #"|") x
            fun tableCell ([]) = []
                | tableCell (x::xs) =
                    ("<td>"^ concat(parseText(links,false,false,0,false,false,explode(x))) ^"</td>")::(tableCell xs)
        in
            ("<tr>"^ concat(tableCell elems) ^"</tr>")::(tablehandler(links, xs))
        end

(*parses the unordered list*)
fun ulhandler (links,lemp,[], []) = []
    |   ulhandler(links,lemp,prev, []) =
        "<li>"::parse (links,false,lemp,false, revlist(remEmp(prev),[]))@["</li>"]
    |   ulhandler(links,lemp,prev, (x::xs)) =
        if (prev <> [] andalso String.isPrefix "- " x) then
            "<li>"::parse(links,false,lemp,false, revlist(remEmp(prev),[]))@["</li>"]@(ulhandler(links,false,[], (String.extract(x,1,NONE))::xs))
        else if (String.isPrefix "- " x) then
            ulhandler(links,false,[String.extract(x,1,NONE)], xs)
        else
            ulhandler(links,emp(x),x::prev, xs)

(*parses the ordered list*)
and olhandler (links,lemp, [], []) = []
    |   olhandler(links,lemp, prev, []) =
        "<li>"::parse (links,false,lemp,false, revlist(remEmp(prev),[]))@["</li>"]
    |   olhandler(links,lemp,prev, (x::xs)) =
        if (prev <> [] andalso olcheck(x) = 0) then
            "<li>"::parse(links,false,lemp,false, revlist(remEmp(prev),[]))@["</li>"]@(olhandler(links,false,[], (remTillDot x)::xs))
        else if (olcheck(x) = 0) then
            olhandler(links,false,[remTillDot x], xs)
        else
            olhandler(links,emp(x),x::prev, xs)

(*line by line parsing of the markdown text*)
and parse (links,scb,checkp,ppar, []) = 
    if ppar = true then
        ["</p>"]
    else
        []
    | parse (links,scb,checkp,ppar, (x::xs)) = 
    let 
        val x2 = explode x
    in
    if (String.isPrefix "#" x) then 
        if ppar = true then
            "</p>"::(headerCheck(links, x))::(parse (links,scb,checkp,false, xs))
        else
            (headerCheck(links,x))::(parse (links,scb,checkp,false, xs))
    else if (String.isPrefix ">" x) then
        let
            val (bq, rest) = bqfinder (x::xs)
        in
            if ppar = true then
                "</p>"::"<blockquote>"::(parse (links,scb,checkp,false, bq)@("</blockquote>"::(parse (links,scb,checkp,false, rest))))
            else
                "<blockquote>"::(parse (links,scb,checkp,false, bq)@("</blockquote>"::(parse (links,scb,checkp,false, rest))))
        end
    else if (ulcheck(x) > ~1) then
        let 
            val (ul, rest) = ulfinder (ulcheck(x),false,x::xs)
        in
            if ppar = true then
                "</p>"::"<ul>"::(ulhandler(links,false,[],ul)@("</ul>"::(parse (links,scb,checkp,false, rest))))
            else
                "<ul>"::(ulhandler(links,false,[],ul)@("</ul>"::(parse (links,scb,checkp,false, rest))))
        end
    else if (olcheck(x) > ~1) then
        let 
            val (ol, rest) = olfinder (olcheck(x),false,x::xs)
        in
            if ppar = true then
                "</p>"::"<ol>"::(olhandler(links,false,[],ol)@("</ol>"::(parse (links,scb,checkp,false, rest))))
            else
                "<ol>"::(olhandler(links,false,[],ol)@("</ol>"::(parse (links,scb,checkp,false, rest))))
        end
    else if (String.isPrefix "---" x ) then
        if ppar = true then
            "</p>"::"<hr>"::(parse (links,scb,checkp,false, xs))
        else
            "<hr>"::(parse (links,scb,checkp,false, xs))
    else if ((scb andalso String.isPrefix "    " x) orelse (String.isPrefix "        " x)) then
        let 
            val (cb, rest) = cbfinder (x::xs)
        in
            if ppar = true then
                "</p>"::"<pre><code>"::(cb@("</code></pre>"::(parse (links,scb,checkp,false, rest))))
            else
                "<pre><code>"::(cb@("</code></pre>"::(parse (links,scb,checkp,false, rest))))
        end
    else if (String.isPrefix "<<" x) then
        let 
            val (table, rest) = tablefinder (xs)
        in
            if ppar = true then
                "</p>"::"<center><table border=1>"::(tablehandler(links,table))@("</table></center>"::(parse (links,scb,checkp,false, rest)))
            else
                "<center><table border=1>"::(tablehandler(links,table))@("</table></center>"::(parse (links,scb,checkp,false, rest)))
        end

    else if checkp = true then
        if emp x then
            if ppar = true then
                "</p>"::(parse (links,scb,true,false, xs))
            else
                parse (links,scb,true,false, xs)
        else
            if ppar = true then
                concat(parseText(links,false,false,0,false,false,x2))::(parse (links,scb,true,true, xs))
            else
                "<p>"::(concat(parseText(links,false,false,0,false,false,x2))::(parse (links,scb,true,true, xs)))
    else
        if emp x then
            parse (links,scb,true,false, xs)
        else
            concat(parseText(links,false,false,0,false,false,x2))::(parse (links,scb,false,false, xs))
    end;

fun replaceTabs s = 
    let
        fun replaceTabsHelp ([]) = []
            | replaceTabsHelp (x::xs) =
                if x = #"\t" then
                    #" "::(#" "::(#" "::(#" "::(replaceTabsHelp xs))))
                else
                    x::replaceTabsHelp xs
    in
        implode(replaceTabsHelp(explode s))
    end;

(*extracts reference style links from text*)
fun linkExtractor ([]) = ([],[])
    | linkExtractor (x::xs) = 
        let
            val (l1,lrem) = linkExtractor(xs)
        in
        if String.isPrefix "[" x then
            let
                val (linkText, rest) =  findTill(tl(explode(x)), #"]")
                val (linkEx, rest2) = findTill(rest, #"h")
                val (link, rest3) = findTill(#"h"::(rest2@[#" "]), #" ")
            in
                ([(implode(linkText), implode(link))]@l1, lrem)
            end
        else
            (l1, x::lrem)
        end
            

fun mdt2html s =
    let 
        val file = TextIO.openIn s
        val inp = (String.fields (fn c => c = #"\n") (replaceTabs(TextIO.inputAll file)))
        val (links, inp2) = linkExtractor inp
        val lines = parse(links,true,true,false,inp2)
        val _ = TextIO.closeIn file
        val outp = TextIO.openOut ((hd(String.fields (fn c => c = #".") s))^".html")
    in
        TextIO.output(outp, String.concatWith "\n" lines);
        TextIO.closeOut outp
    end;
