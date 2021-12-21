# LkProver
An Automated LK deduction. Emits a latex snippet of `bussproof`'s `prooftree`.

## Example
```sh
echo '|- ((p -> q) -> p) -> p' | dune exec bin/main.exe
```
```latex
\begin{prooftree}
\AxiomC{}
\RightLabel{(axiom)}
\UnaryInfC{$p \vdash p, q$}
\RightLabel{($\rightarrow R$)}
\UnaryInfC{$\vdash p, p \rightarrow q$}
\AxiomC{}
\RightLabel{(axiom)}
\UnaryInfC{$p \vdash p$}
\RightLabel{($\rightarrow L$)}
\UnaryInfC{$(p \rightarrow q) \rightarrow p \vdash p$}
\RightLabel{($\rightarrow R$)}
\UnaryInfC{$\vdash ((p \rightarrow q) \rightarrow p) \rightarrow p$}
\end{prooftree}
```

## Available connectives
- not : `¬`, `~`, `!`
- and : `∧`, `^`
- or : `∨`, `v`, `V`
- implication : `→`, `->`
- proves : `⇒`, `=>`, `⊢`, `|-`
