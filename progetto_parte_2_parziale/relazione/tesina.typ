#import "template.typ": *
#import "@preview/fletcher:0.5.5" as fletcher: diagram, node, edge
#import "@preview/colorful-boxes:1.4.3": colorbox

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

= Scelte di design
- Le dichiarazioni di variabili contengono obbligatoriamente l'assegnamento (motivazione:
  vedi pagina 3 punto 2 `implementazione-compilatori-imperativi.pdf`).
- Le costanti possono avere solo tipi primitivi (visto che le costanti non devono allocare
  spazio in memoria) (FIXME: questa è ovvia la toglierei).
- La stringa è considerata un tipo primitivo (vedi pagina 3 punto 3
  `implementazione-compilatori-imperativi.pdf`).
- I due tipi di iterazione indeterminata sono derivabili dall'implementazione del loop
  come previsto dalla sintassi concreta di Blue.
- `fn` è syntax sugar di `fn()`.


== Modifiche alla sintassi concreta
- Dalla sintassi concreta sono state tolte classi, pre post condizioni, etc. perchè non
  coerenti con quanto richiesto (FIXME: questa è ovvia la toglierei).
- Nelle function call no punto (ovviamente perchè non abbiamo le classi).
- Blue supporta solo commenti single line (con "--" o "=="). I commenti "==" sono
  documentazione per classi/funzioni ed è stata tolta dalla nostra implementazione perchè
  non richiesti. Inoltre sono stati aggiunti i commenti multi-line "/*"..."*/".
- Aggiunta keyword `return` nelle funzioni (al posto di avere i nomi delle variabili di
  ritorno che avrebbero richiesto di avere dichiarazioni senza assegnamento).

== Risoluzione conflitti
- Il conflitto state 107 LExp vs RExp non è un problema. Essendo le LExp sottoinsieme
  delle RExp basta scegliere sempre la Shift.
- Le dichiarazioni di funzioni, nell'ambiente più esterno, devono essere precedute dalla
  keyword `routines` similmente a quanto succede in Blue per le dichiarazioni delle
  routines nelle classi. Per evitare ambiguità tipo:
  ```
  var
    a: Integer := b
  main ...
  ```
  dove main potrebbe essere sia un nome di funzione che un nome di una variabile.
- Aggiunto `;` alla fine degli Statement (a parte If e Loop) e delle dichiarazioni di
  costanti e variabili per evitare ambiguità tipo:
  ```
  var
    a: Integer := b
  ( ...
  ```
  dove con un lookahead di 1 non è chiaro se "b (" è l'inizio di una chiamata di funzione
  oppure "(" è l'inizio di una LExp.
  Oppure ambiguità tipo:
  ```
  var
    a: Integer := b
  * ...
  ```
  dove "*" potrebbe essere sia un deref che una moltiplicazione.
  Per lo stesso motivo è stato aggiunto un ";" anche nell'exit dei loop.