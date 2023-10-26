module CPP.Syntax


type Type =
    | TyInt of signed: bool * bits: int
    | TyDouble
    | TyChar
    | TyBool 
    | TyConst of Type
    | TyRef of Type
    | TyPtr of Type
    | TyFunPtr of ret: Type option * args: Type list
    | TyStdFunction of ret: Type option * args: Type list
    | TyClass of string * Param list

and Param =
    | PType of Type
    | PConst of Expr

and Expr =
    | EVar of string * Param list
    | EElem of Expr * string
    | EIndex of Expr * Expr
    | EPtrOf of Expr
    | EFunPtr of string * Param list
    | EThisPtr
    | EDeref of Expr
    | ECast of Type * Expr
    | EInt of bigint * Type
    | EBool of bool
    | EDouble of double
    | EChar of char
    | EUnary of operator: string * Expr
    | EBin of operator: string * Expr * Expr
    | ECall of Expr * Expr list
    | ECtor of name: string * param: Param list * args: Expr list
    | ENew of Type * Expr list
    | EIf of cond: Expr * Expr * Expr
    | ELambda of capture: Capture * args: (Type * string) list * body: Block

and Capture =
    | CAllVal
    | CAllRef
    | CList of Capture' list

and Capture' =
    | CVal of string
    | CRef of string

and Stmt =
    | SLet of Type * string * Expr option
    | SSet of Expr * Expr
    | SExpr of Expr
    | SIf of Expr * Block * Block
    | SWhile of Expr * Block
    | SDoWhile of Block * Expr
    | SFor of init: Block * cond: Expr * update: Block * body: Block
    | SContinue
    | SBreak
    | SSwitch of Expr * (int * Block) list * Block
    | SReturn of Expr
    | SDelete of Expr
    | SBlock of Block

and Block = Stmt list

type ParamKind =
    | PKType of string
    | PKConst of Type * string

type FunDef =
    { name: string
      param: ParamKind list
      args: (Type * string) list
      retType: Type option
      body: Block }

type Accessibility =
    | Public
    | Protected
    | Private

type ClassDef =
    { name: string
      param: ParamKind list
      fields: (Type * string * Accessibility) list
      methods: (FunDef * Accessibility) list }

type Def =
    | DFun of FunDef
    | DVar of Type * string * Expr option
    | DClass of ClassDef


