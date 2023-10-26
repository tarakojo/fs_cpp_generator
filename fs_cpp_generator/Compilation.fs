module CPP.Compilation

open Syntax

type B = System.Text.StringBuilder

let (++) (b: B) x : B = b.Append(x.ToString())

let (--) (b: B) (f: B -> B) = f b

let rec sequence delim f lis b =
    match lis with
    | [] -> b
    | h :: [] -> b -- f h
    | h :: t -> b -- f h ++ delim -- sequence delim f t

let tuple f lis b = b ++ "(" -- sequence "," f lis ++ ")"

let braceTuple f lis b = b ++ "{" -- sequence "," f lis ++ "}"

let paren f x b = b ++ "(" -- f x ++ ")"

let rec ty t b =
    match t with
    | TyInt (signed, bits) ->
        if signed then
            b ++ "int" ++ bits ++ "_t "
        else
            b ++ "uint" ++ bits ++ "_t "
    | TyDouble -> b ++ "double "
    | TyChar -> b ++ "char"
    | TyBool -> b ++ "bool"
    | TyConst t' -> b ++ "const " -- ty t'
    | TyRef t' -> b ++ "& " -- ty t'
    | TyPtr t' -> b ++ "* " -- ty t'
    | TyFunPtr (ret, args) -> b -- tyopt ret ++ "(*)" -- tuple ty args ++ ")"
    | TyStdFunction (ret, args) -> b ++ "std::function<" -- tyopt ret -- tuple ty args ++ ">"
    | TyClass (name, []) -> b ++ name
    | TyClass (name, ps) -> b ++ name -- ptuple ps

and tyopt t b =
    match t with
    | None -> b ++ "void"
    | Some t' -> b -- ty t'

and param p b =
    match p with
    | PType t -> b -- ty t
    | PConst e -> b -- expr e

and ptuple lis b =
    b ++ "<" -- sequence "," param lis ++ ">"

and expr e b =
    match e with
    | EVar (x, []) -> b ++ x
    | EVar (x, ps) -> b ++ x -- ptuple ps
    | EElem (e', f) -> b -- expr e' ++ "." ++ f
    | EIndex (e', i) -> b -- expr e' ++ "[" -- expr i ++ "]"
    | EPtrOf e' -> b ++ "&" -- paren expr e'
    | EFunPtr (name, ps) -> b ++ name -- ptuple ps
    | EThisPtr -> b ++ "this"
    | EDeref e' -> b ++ "(*" -- paren expr e' ++ ")"
    | ECast (t, e') -> b ++ "(" -- paren ty t -- paren expr e' ++ ")"
    | EInt (x, t) -> b -- eint x t
    | EBool x -> b ++ x
    | EDouble x -> b ++ x
    | EChar x -> b ++ "'" ++ x ++ "'"
    | EUnary (op, e') -> b ++ "(" ++  op -- paren expr e' ++ ")"
    | EBin (op, e1, e2) -> b ++ "(" -- expr e1 ++ op -- expr e2 ++ ")"
    | ECall (f, args) -> b ++"((" -- expr f ++ ")" -- tuple expr args ++ ")"
    | ECtor (name, ps, args) -> b ++ "(" ++ name -- ptuple ps -- braceTuple expr args ++ ")"
    | ENew (t, args) -> b ++ "(new " -- ty t -- braceTuple expr args ++ ")"
    | EIf (cond, et, ef) ->
        b ++ "(" -- paren expr cond ++ " ? "
        -- paren expr et
        ++ " : "
        -- paren expr ef
        ++ ")"
    | ELambda (capture, args, body) ->
        let b =
            match capture with
            | CAllVal -> b ++ "[=]"
            | CAllRef -> b ++ "[&]"
            | CList cs -> b ++ "[" -- sequence "," capture' cs ++ "]"

        b -- tuple arg args -- block body


and eint x t b =
    match t with
    | TyInt (_, 8)
    | TyInt (_, 16)
    | TyInt (true, 32) -> b ++ x
    | TyInt (false, 32) -> b ++ x ++ "l"
    | TyInt (true, 64) -> b ++ x ++ "ll"
    | TyInt (false, 64) -> b ++ x ++ "ull"
    | _ -> failwith "invalid integer"


and capture' c b =
    match c with
    | CVal x -> b ++ x
    | CRef x -> b ++ "&" ++ x

and arg (t, x) b = b -- ty t ++ " " ++ x

and stmt s b =
    match s with
    | SLet (t, x, None) -> b -- ty t ++ " " ++ x ++ ";\n"
    | SLet (t, x, Some e) -> b -- ty t ++ " " ++ x ++ " = " -- expr e ++ ";\n"
    | SSet (e1, e2) -> b -- expr e1 ++ "=" -- expr e2 ++ ";\n"
    | SExpr e -> b -- expr e ++ ";\n"
    | SIf (cond, bt, bf) ->
        b ++ "if" -- paren expr cond -- block bt ++ "else"
        -- block bf
    | SWhile (cond, bl) -> b ++ "while" -- paren expr cond -- block bl
    | SDoWhile (bl, cond) ->
        b ++ "do" -- block bl ++ "while"
        -- paren expr cond
        ++ ";\n"
    | SFor (init, cond, update, body) ->
        b ++ "for(" -- sequence "" stmt init ++ ";"
        -- expr cond
        ++ ";"
        -- sequence "" stmt update
        ++ ")"
        -- block body
    | SContinue -> b ++ "continue;\n"
    | SBreak -> b ++ "break;\n"
    | SSwitch (cond, patterns, defualt_) ->
        b ++ "switch" -- paren expr cond ++ "{\n"
        -- sequence "\n" (fun (x, bl) b -> b ++ "case " ++ x ++ " : \n" -- block bl) patterns
        ++ "default : \n"
        -- block defualt_
        ++ "\n};\n"
    | SReturn e -> b ++ "return " -- expr e ++ ";\n"
    | SDelete e -> b ++ "delete " -- expr e ++ ";\n"
    | SBlock bl -> b -- block bl

and block bl b =
    b ++ "{\n" -- sequence "" stmt bl ++ "\n}\n"

let paramKind k b =
    match k with
    | PKType x -> b ++ "typename " ++ x
    | PKConst (t, x) -> b -- ty t ++ " " ++ x

let funDef (def: FunDef) b =
    let b =
        match def.param with
        | [] -> b
        | _ ->
            b ++ "template<"
            -- sequence "," paramKind def.param
            ++ ">\n"

    b -- tyopt def.retType ++ " " ++ def.name
    -- tuple arg def.args
    ++ block def.body

let accessibility acc b =
    match acc with
    | Public -> b ++ "public"
    | Protected -> b ++ "protected"
    | Private -> b ++ "private"

let classDef def b =
    let b =
        match def.param with
        | [] -> b
        | _ ->
            b ++ "template<"
            -- sequence "," paramKind def.param
            ++ ">\n"

    let b = b ++ "class " ++ def.name ++ "{\n"

    for (t, x, acc) in def.fields do
        b -- accessibility acc ++ " : \n" -- ty t
        ++ " "
        ++ x
        ++ ";\n"
        |> ignore

    for (d, acc) in def.methods do
        b -- accessibility acc ++ " : \n" -- funDef d
        |> ignore

    b ++ "\n};\n"

let def d b =
    match d with
    | DFun d' -> b -- funDef d'
    | DVar (t, x, None) -> b -- ty t ++ " " ++ x ++ ";\n"
    | DVar (t, x, Some e) -> b -- ty t ++ " " ++ x ++ " = " -- expr e ++ ";\n"
    | DClass d' -> b -- classDef d'
