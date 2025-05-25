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

= Grammatica labelled-BNFC

Si è scelto di implementare il progetto in C mediante BNFC. La grammatica con cui è stato
generato il codice da BNFC è la seguente:

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

Il parser generato da Bison a partire da tale grammatica non presenta alcun tipo di
conflitto.

Di seguito sono riportate tutte le modifiche effettuate al lexer ed al parser generati per
implementare quanto specificato nella consegna. Ogni sezione tratta una specifica
richiesta, indipendentemente dal fatto che questa sia stata realizzata a livello di lexer
o parser.

#colorbox(
    title: "Versioni",
    color: "gray",
    radius: 2pt,
    width: auto,
)[
    Di seguito le versioni / standard degli strumenti utilizzati per realizzare questo
    progetto:
    + `C`: standard `C11`;
    + `BNFC`: 2.9.5;
    + `Flex`: 2.6.4-8.
    + `Bison`: 2:3.7.5+dfsg-1;

    È fornito un `Dockerfile` con le versioni usate per semplificare l'uso del progetto.
]

#pagebreak()

= Import

Il non terminale `<Import>` in BNFC ha un'unica regola:
```
ImportFile.     Import ::= String ;
```
dove `String` è il token predefinito di BNFC che riconosce le stringhe delimitate da apici. Il lexer prodotto da BNFC è stato poi modificato e reso conforme con quanto richiesto dalla consegna: gli apici sono stati sostituiti dalle le espressioni regolari `<{IMP}*import{IMP}*>` (in apertura) e `<\/{IMP}*import{IMP}*>` (in chiusura), dove `{IMP}` è un qualsiasi whitespace e/o commento; sono stati anche creati nuovi stati `IMPORT` e `ESCAPEDI` in maniera analoga a quanto viene generato per `String`.


== Path
Il controllo della correttezza della stringa rappresenate il path, e quindi anche l'import del file, è delegata al parser.
In particolare la funzione `make_ImportFile` generata da BNFC è stata modificata: anzichè creare un'istanza della struttura `Import`, chiama ricorsivamente la funzione `pXML` sul file da importare.
Tale funzione è pensata per essere usata esclusivamente con path UNIX-like.

I path dei file da importare possono essere di due tipi:
+ path assoluti: in questo caso non c'è ambiguità, viene importato il file corrispondente se esiste, altrimenti viene ritornato un errore e viene fermato il parsing;
+ path relativi: vengono "risolti" rispetto al path del file da cui sono stati importati (o rispetto a dove è stato chiamato l'eseguibile `build` se importati dallo standard input) tramite l'apposita funzione `relative_path`.

== Import ricorsivi e multipli
// TODO: questa sezione copiarla da typst online che avevo modificato qualche roba
Per gestire import ricorsivi e multipli è stata creato un array globale `parsed_files` che viene popolato ad ogni import con il nome del file importato ed un puntatore al file che lo ha importato.

Ad esempio, consideriamo 4 file che i cui import sono descritti dal seguente grafo delle dipendenze:

#figure(
  diagram(
    node-stroke: 1pt,
    node((1,0), [1]),
    edge("-|>", bend: -40deg),
    node((0,1), [2]),
    edge("-|>"),
    node((2,1), [3]),
    edge((2,1), (1,0), "-|>", bend: -40deg),
    node((2,2), [4]),
    edge((0,1), (2,2), "-|>", bend: -40deg),
  ),
  caption: "Esempio grafo dipendenze di import."
)

alla fine dell'elaborazione degli import da parte del parser l'array `parsed_files` sarebbe diventato:
$ mat(
  1, 2, 3, 4;
  2, 1, 2, 1;
) $
dove la prima riga indica i nomi dei file, mentre la seconda riga i puntatori ai file che hanno importato il file nella stessa colonna (ad esempio il file 4 è importato dal file 1).

Dunque per individuare gli input ricorsivi sarà sufficiente percorrere la catena dei riferimenti e verificare che non ce ne siano di ripetuti. In questo esempio nella catena $2 -> 1 -> 2$ il $2$ è ripetuto, e quindi sono import ricorsivi.

Invece per verificare che non ci siano input ripetuti (ma non ricorsivi) è sufficiente controllare che non ci siano file ripetuti nella lista `parsed_files`.

Entrambe queste situazioni producono un corrispondende warning, senza però bloccare il parsing (gli import ripetuti e/o ricorsivi vengono semplicemente ignorati).

/*
TODO: gli import dovrebbero essere relativi al file da cui importo non da dove viene chiamato il parser!

Trasformiamo l'abstract syntax tree nella nostra srtuttura dati, in questa operazione veniene anche controllato:
- field duplicati
- sezioni duplicate
- ci sono gli inherit per le variabili non locali

Creata la struttura risolviamo le variabili non locali ed eventuali cicli
*/

#pagebreak()

= DataStructure

== Creazione della struttura dati

== Risoluzione puntatori
I riferimenti vengono risolti dalle funzioni del modulo `ResolveRefs` direttamente sulla
struttura dati `DataStructure` costruita.
Nello specifico la funzione `resolveSections` scorre ricorsivamente tutte le `Sections` e
itera sui loro `Fields`. Per ogni valore del tipo `Ref_unresolved` (overo una reference
ancora non risolta), viene chiamata la funzione `resolveField`.
`resolveField` esegue una visita simil DFS scoprendo tutta la catena di reference (sul
field su cui è stata chiamata) fino ad un valore già risolto.

Sempre in stile DFS, i field vengono marcati come `Visited` o `Not_visited` per scoprire
eventuali dipendenze cicliche. In tal caso viene stampato un errore e viene abortito il
parsing.

Anche nel caso di reference a sezioni / field non esistenti il parsing viene fermato con
un errore.

== Metodi

#pagebreak()

= Pretty Printer
Questa sezione descrive il funzionamento del Pretty Printer progettato per il nostro linguaggio.

== Struttura principale

Il printer è composto da quattro funzioni principali, ognuna responsabile della stampa di un particolare tipo di nodo nella struttura dati:
- _printSections_: stampa le sezioni del programma;
- _printInherits_: stampa gli inherits di una sezione;
- _printFields_: stampa i campi di una sezione;
- _printVal_: stampa il valore di un campo.
\
La funzione _printSections_ prende in input una lista di sezioni (un oggetto di tipo _Sections_) e, per ciascuna sezione, stampa il nome, gli eventuali commenti associati, gli elementi ereditati (inherits) e i campi (fields). La stampa degli elementi ereditati, per ogni sezione, è gestita dalla funzione _printInherits_, che riceve in input un oggetto di tipo _DynamicArray_, ovvero un vettore di elementi di tipo _Inherited_ e stampa ciascun elemento insieme agli eventuali commenti associati. I campi, rappresentati da una lista di oggetti Fields, sono stampati tramite la funzione _printFields_, che per ogni _field_ riporta il nome, i commenti eventualmente presenti e il valore. Quest'ultimo è elaborato dalla funzione _printVal_, che lo stampa in base al tipo (intero, stringa, booleano, riferimento), includendo anche in questo caso i commenti associati. Tutte queste funzioni fanno uso della procedura ausiliaria _get_current_comment_, che consente di ottenere il commento corrente sulla base dell'indice globale i.

== Commenti
Per la gestione dei commenti viene utilizzata una variabile globale _comments_, di tipo _DynamicArray_. Si tratta di un vettore contenente elementi di tipo _Comment\__,  ciascuno composto dai seguenti campi:
- _file_path_: il percorso del file in cui è presente il commento;
- _line_: il numero di riga del file in cui si trova il commento;
- _comment_: il testo del commento.
\
Quando si stampa una sezione, un _inherit_, un _field_, o un valore vengono stampati anche i commenti associati, se ve ne sono. In particolare la stampa avviene come segue:
- Per _Sections_, _inherit_ e _fields_, sono considerati sia il valore _top_loc.last_line_#footnote[_last_line_ indica la riga in cui termina il delimitatore di un elemento.
Ad esempio, nel seguente frammento:\ `1 <field name=var2> $var1
2 </field
3 >` \ _top_loc.last_line_ = 1, mentre _bottom_loc->last_line_ = 3.
] che il valore _bottom_loc.last_line_, ovvero la posizione iniziale e finale dei delimitatori dell’elemento nel file. Un commento è stampato sopra il delimitatore di un elemento se:
  + il percorso del file del commento coincide con quello dell'elemento;
  + la riga del commento è minore o uguale a _top_loc.last_line_ (o _bottom_loc.last_line_) dell'elemento.
- Per i _Val\__ viene invece utilizzato solo bottom_loc, poiché si assumono definiti su una singola riga. Il commento viene quindi stampato se:
  + il percorso del file coincide;
  + la riga del commento è minore o uguale a _bottom_loc->last_line_.

In entrambi i casi, se un commento soddisfa le condizioni, viene stampato prima dell’elemento corrispondente, e l’indice globale _i_ viene incrementato per passare al commento successivo. Al contrario, se le condizioni non sono soddisfatte, si procede con la stampa dell’elemento corrente e si continua il controllo con l’elemento successivo.

#pagebreak()

= Demo

I principali casi di test sono disponibili nella cartella _tests_.\
È possibile verificarne il corretto funzionamento eseguendo il comando make demo da riga di comando.

#grid(
  columns: 3,
  column-gutter: 10pt,
  row-gutter: 5pt,

  [*ID*],[*Descrizione*],[*Comportamento atteso*],
  [1],[Ereditarietà da una sezione precedente],[Esecuzione corretta, senza errori né warning],
  [2],[Ereditarietà da una sezione successiva],[Esecuzione corretta, senza errori né warning],
  [3],[Sovrascrittura di una variabile],[Segnala la sovrascrittura; viene mantenuto l'ultimo valore assegnato],
  [4],[Sezione definita più volte],[Segnala la sezione ripetuta; esecuzione interrotta],
  [5],[Import ricorsivo],[Segnala la ricorsione sull'import; la catena viene interrotta correttamente],
  [6],[Import ripetuto dello stesso file],[Segnala l'import ripetuto; viene mantenuta una sola occorrenza],
  [7],[Mancata ereditarietà per una variabile non locale],[Segnala che la sezione non è ereditata; esecuzione interrotta],
  [8],[Riferimenti ricorsivi tra variabili],[Segnala i riferimenti ricorsivi. Interrotta l'esecuzione],
  [9],[Riferimento a una sezione inesistente],[Segnala che la sezione riferita è inesistente; esecuzione interrotta],
  [10],[Riferimento a una variabile inesistente],[Segnala che la variabile riferita è inesistente; esecuzione interrotta],
  [11],[Ereditarietà superflua],[Segnala l'inherit non necessario. Esegue senza modificare l'input.],
)

/*
TODO: 
- testare operazioni su DataStructure: Leo (remove dg)
- migliori errori con linee: F
- relazione 
- gli import dovrebbero essere relativi al file da cui importo non da dove viene chiamato il parser!: DG
- eliminare funzioni inutilizzate: 
- free globals: DG
- risolvere tutti TODO

TODO: (finali solo se c'è tempo)
- dare warning con inherit ricorsivi?: DG
*/
