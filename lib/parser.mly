%token <string> SYM
%token NOT AND OR IMP LPAREN RPAREN PROVES COMMA EOF

%start <Lk.sequent> sequent

%right IMP
%left OR
%left AND
%right prec_uni

%%

paren(rule):
  | ret = delimited(LPAREN, rule, RPAREN) { ret }

sequent:
  | p1 = propset PROVES p2 = propset EOF { Lk.Sequent (p1, p2) }

propset:
  | ps = separated_list(COMMA, prop) { Lk.PropSet.of_list ps }

prop:
  | sym = SYM { Lk.Sym sym }
  | NOT p = prop %prec prec_uni { Lk.Not p }
  | p1 = prop AND p2 = prop { Lk.And (p1, p2) }
  | p1 = prop OR p2 = prop { Lk.Or (p1, p2) }
  | p1 = prop IMP p2 = prop { Lk.Imp (p1, p2) }
  | p = paren(prop) { p }
