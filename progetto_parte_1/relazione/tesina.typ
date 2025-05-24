#import "template.typ": *

#show: template
#set par(spacing: 0.65em)

#let annotation(body)={
par(
  [#h(1.25em)#body]
)
}

#let appendix(body) = {
  set heading(numbering: "A", supplement: [Appendix])
  counter(heading).update(0)
  body
}
#set strike(stroke: 1.5pt + red)

= Grammatica labelled-BNFC

$
&"<XML>" &-> &epsilon | "<Import>" "<XML>" | "<Section>" "<XMLS>"  \
&"<XMLS>" &-> &epsilon | "<Section>" "<XMLS>" \
&"<Import>" &-> &"\"<\"" "import" "\">\"" "<Path>" "\"<\\\"" "import" "\">\"" | "String" \
&"<Section>" &-> &"\"<\"" "section" "name" "=" "<Ident>" "\">\"" "<SBody>" "\"<\\\"" "section" "\">\"" \
&"<SBody>" &-> &epsilon | "<Inherit>" "<SBody>" | "<Field>" "<SBodyF>" \
&"<SBodyF>" &-> &epsilon | "<Field>" "<SBodyF>" \
&"<Inherit>" &-> &"\"<\"" "inherit" "\">\"" "<Ident>" "\"<\\\"" "inherit" "\">\"" \
&"<Field>" &-> &"\"<\"" "field" "name" "=" "<Ident>" "\">\"" "<Value>" "\"<\\\"" "field" "\">\"" \
&"<Value>" &-> &"<Integer>" | "true" | "false" | "<String>" | "$" "<Ident>" | "$" "<Ident>" "." "<Ident>" ;
$

- la grammatica non ha conflitti perchè bison li segnalerebbe
- nella grammatica .cf Import messo come string e poi modificato file Flex per matchare <improt>.*</import>
  come fossero stringhe. 

= Import

- implementato nel parser

== Recursive and multiple imports
Imports are implemented only for UNIX like paths.

We chose to implement the scope of the variable for the whole file (both before and after
its declaration).
This means that variables are visible in the whole file (including the imported files) if
inherited correctly.

So recursive and multiple imports became basically the same "problem". We choose to
implement a log that warns the user if a recursive or a multiple import is detected, but
we continue to parse as the user would expect (ignoring the repeted imports, and thus also
the backedges of cycles).

This check is implemented thanks to a global list populated with the already parsed files.
Files are represented by an unique ID thanks to `stat()` in `sys/stat.h`. The path of the
erroneusly imported file is returned in the warn message to let the user resolve the
problem.

TODO: gli import dovrebbero essere relativi al file da cui importo non da dove viene chiamato il parser!

= DataStructure

== Resolving

Trasformiamo l'abstract syntax tree nella nostra srtuttura dati, in questa operazione veniene anche controllato:
- field duplicati
- sezioni duplicate
- ci sono gli inherit per le variabili non locali

Creata la struttura risolviamo le variabili non locali ed eventuali cicli

TODO: 
- testare operazioni su DataStructure: Leo (remove dg)
- commenti: F
- migliori errori con linee: F
- relazione 
- gli import dovrebbero essere relativi al file da cui importo non da dove viene chiamato il parser!: DG
- eliminare funzioni inutilizzate: 
- free globals: DG
- risolvere tutti TODO

TODO: (finali solo se c'è tempo)
- dare warning con inherit ricorsivi?: DG
- fixare caratteri speciali nell'output di `make demo`: F