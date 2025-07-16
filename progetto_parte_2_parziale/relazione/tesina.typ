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

= Scelte design
- No dichiarazioni variabili senza assegnamento
- No pre post condizioni
- commenti (multi line aggiunto, "==" tolto) ??
- costanti solo tipi primitivi (visto che le costanti non devono allocare spazio in memoria)
- la stringa è considerata un tipo primitivo
- aggiunta i valori dopo il return nelle funzioni (ritornano tuple) (al posto di avere i nomi delle variabili di ritorno)

- opzioni loop:
    - implementarli come da sintassi concreta (automaticamente è come fare whiledo e dowhile)
    - limitare la sintassi concreta e fare i due casi (mettendo la condizione all'inizio o alla fine)
- leo ha deciso warning (o no) se nelle funzioni ci sono le parentesi senza niente dentro
- nelle function call no punto (ovviamente perchè non abbiamo le classi)