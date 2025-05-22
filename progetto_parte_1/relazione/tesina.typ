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

- la grammatica non ha conflitti perchè bison li segnalerebbe
- nella grammatica .cf Import messo come string e poi modificato file Flex per matchare <improt>.*</import>
  come fossero stringhe. 
  TODO: 
  <import>a</import
  # commento
  >
  non funziona

= Import

- implementato nel parser
- import ciclici vengono detectati mantenendo una lista globale dei file parsati.

Trasformiamo l'abstract syntax tree nella nostra srtuttura dati, in questa operazione veniene anche controllato:
- field duplicati
- sezioni duplicate
- ci sono gli inherit per le variabili non locoali

Creata la struttura risolviamo le variabili non locali ed eventuali cicli