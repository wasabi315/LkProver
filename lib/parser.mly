%{
  open Lk
%}

%token <string> SYM
%token BOTTOM NOT AND OR IMP LPAREN RPAREN PROVES COMMA EOF

%start <Sequent.t> sequent

%right IMP
%left OR
%left AND
%right prec_uni

%%

paren(rule):
  | ret = delimited(LPAREN, rule, RPAREN) { ret }

sequent:
  | p1 = Prop_set PROVES p2 = Prop_set EOF { (p1, p2) }

Prop_set:
  | ps = separated_list(COMMA, prop) { Prop_set.of_list ps }

prop:
  | BOTTOM { Prop.Bottom }
  | sym = SYM { Prop.Sym sym }
  | NOT p = prop %prec prec_uni { Prop.Not p }
  | p1 = prop AND p2 = prop { Prop.And (p1, p2) }
  | p1 = prop OR p2 = prop { Prop.Or (p1, p2) }
  | p1 = prop IMP p2 = prop { Prop.Imp (p1, p2) }
  | p = paren(prop) { p }
