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

= Scelte di design (Integrate nella definizione del linguaggio)
- Le dichiarazioni di variabili contengono obbligatoriamente l'assegnamento (motivazione:
  vedi pagina 3 punto 2 `implementazione-compilatori-imperativi.pdf`).
- Le costanti possono avere solo tipi primitivi (visto che le costanti non devono allocare
  spazio in memoria) (FIXME: questa è ovvia la toglierei).
- La stringa è considerata un tipo primitivo (vedi pagina 3 punto 3
  `implementazione-compilatori-imperativi.pdf`).
- I due tipi di iterazione indeterminata sono derivabili dall'implementazione del loop
  come previsto dalla sintassi concreta di Blue.
- `fn` è syntax sugar di `fn()`. (FIXME: non è una nostra scelta, è una cosa di blue)

== Modifiche alla sintassi concreta (Integrate nella definizione del linguaggio)
- Dalla sintassi concreta sono state tolte classi, pre post condizioni, etc. perchè non
  coerenti con quanto richiesto (FIXME: questa è ovvia la toglierei).
- Nelle function call no punto (ovviamente perchè non abbiamo le classi).
- Blue supporta solo commenti single line (con "--" o "=="). I commenti "==" sono
  documentazione per classi/funzioni ed è stata tolta dalla nostra implementazione perchè
  non richiesti. Inoltre sono stati aggiunti i commenti multi-line "/*"..."*/".
- Aggiunta keyword `return` nelle funzioni (al posto di avere i nomi delle variabili di
  ritorno che avrebbero richiesto di avere dichiarazioni senza assegnamento).
// TODO: queste sono gia dopo, ma anche quelle sopra. Cosa serve questo paragrafo?
- Le funzioni tornano solo un tipo
- aggiunti i tipi `val`, `valres` e `ref` per i parametri delle funzioni.
- La keyword `routines` (che in Blue viene usata solo per la dichiarazione di routines nelle classi)
  viene usata anche all'interno del corpo delle funzioni in modo che ci possano essere
  dichiarazioni di routine annidate
// TODO: dire che per routines intendiamo sia funzioni che procedure, come in Blue

== Risoluzione conflitti (Alcuni conflitti sono già stati risolti nella definizione del linguaggio; il conflitto 105 è scritto sotto)
- Il conflitto state 105 LExp vs RExp non è un problema. Essendo le LExp sottoinsieme
  delle RExp basta scegliere sempre la Shift.
// TODO: MODIFICARE: anche in quelli più interni
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
  dove "\*" potrebbe essere sia un deref che una moltiplicazione.
  Per lo stesso motivo è stato aggiunto un ";" anche nell'exit dei loop.


= Cose da controllare durante l'analisi semantica statica

- Controllo tipi
- durante gli assegnamenti (e dichiarazioni) il numeri di L-expr e di R-expr deve essere lo stesso (Si può fare anche nel parser)
- Il numero di argomenti della funzione deve essere giusto
- Il numero di L-expr e il numero di tipi di ritorno nelle espressioni "x1, x2, .. = fun(p1, p2, ..)" deve essere uguale
- sostituire le costanti (da fare nel TAC?)
- nelle call delle routine annotare la lista dei parametri coi tipi, modalità e nomi.
- Controllare che nessuna costante sia a sinistra di un assegnamento O in un parametro ref o valres



= Definizione del linguaggio: scelte di design e risoluzione dei conflitti
Il linguaggio definito per questo progetto è un linguaggio di programmazione imperativo con scoping statico. La sua grammatica si basa sulla sintassi concreta del linguaggio Blue.

Poiché Blue è un linguaggio orientato agli oggetti, per garantire le funzionalità richieste dalla consegna, come per esempio il dereferenziamento, sono stati introdotti alcuni elementi. Tuttavia, non sono presenti né classi né interfacce.

== Tipi
Il linguaggio definito prevede i seguenti tipi primitivi:  `Boolean`, `Integer`, `Real`, `String`#footnote[#cite( <implementazione-compilatori-imperativi>, form:"full" ), p. 3.3] e `Char`.
Sono inoltre supportati gli array che sono dichiarati utilizzando nel modo seguente: `Array<Type, Integer>`, dove `Type` rappresenta il tipo degli elementi e  `Integer` indica la dimensione dell'array. Infine vi sono i puntatori, che sono dichiarati con la sintassi `&Type`.

=== Type system (MEGLIO SOTTO)
Per il nostro linguaggio, abbiamo definito la seguente gerarchia dei tipi:
#image("img/types.jpg", width: 100%)


==== Expression

===== Literal

$
space/ ("env" ⊢ "Literal" { "val" = "LitInt" {"intVal" = e} } : "INT")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitChar" {"charVal" = e} } : "CHAR")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitString" {"stringVal" = e} } : "STRING")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitBool" {"boolVal" = e} } : "BOOL")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitDouble" {"doubleVal" = e} } : "REAL")
$

===== Ident
$
space/ ("env" ⊢ "ExpIdent" { "name" = id } : "env"(id))
$

== Variabili
Le variabili sono dichiarate con la keyword `var` e possono essere di tipo primitivo o di tipo array. Non è consentito dichiarare variabili senza assegnamento #footnote[#cite( <implementazione-compilatori-imperativi>, form:"normal" ), p. 3.2].
== Costanti
Le costanti sono dichiarate con la keyword `const` e possono essere di tipo primitivo (non devono allocare spazio in memoria). Anche in questo caso non è consentito dichiarare costanti senza assegnamento #footnote[#cite( <implementazione-compilatori-imperativi>, form:"normal" ), p. 3.2].

== Loop
Per il nostro linguaggio sono previste le forme di iterazione non determinata `while-do` e `do-while`. Entrambe le strutture sono naturalmente implementate dall'elemento `loop-exit on` della sintassi concreta di Blue.

== Condizionali
Il linguaggio immplementa le strutture condizionali semplici `if-then` e `if-then-else`, che permettono di eseguire blocchi di codice in base a condizioni specifiche.

== Routine
Le routine comprendono sia funzioni che procedure. Una funzione senza parametri può essere invocata sia con la sintassi completa `function()` sia in forma abbreviata `function`, che è semplicemente zucchero sintattico per la prima. Rispetto alla sintassi concreta del linguaggio Blue, sono rimosse pre e post-condizioni; inoltre i parametri di ritorno non vengono nominati esplicitamente nella dichiarazione: si specificano solamente i tipi restituiti mediante la notazione `-> Type`. Si nota che non è possibile restituire tuple. Per gestire il ritorno esplicito dei valori, è stato introdotto il costrutto `return`.
Le dichiarazioni di funzioni, in ogni ambiente, devono essere precedute dalla keyword `routines`, in modo simile a quanto avviene in Blue per le dichiarazioni delle routine nelle classi. Questo è necessario per evitare ambiguità, come nel seguente esempio:
  ``` Blue
  var
    a: Integer := b
  main ...
  ```
  dove `main` potrebbe rappresentare sia il nome di una funzione che il nome di una variabile.

Quando si invoca una funzione è possibile specificare la modalità di passaggio dei parametri in ingresso (`val` , `valres` e `ref`).
Infine, poiché il linguaggio non prevede il concetto di classi, le funzioni non possono essere invocate con la notazione a punto, come `.function()`.

== Commenti
Il linguaggio supporta due tipi di commenti:

- Commenti a riga singola, introdotti dal simbolo `--`, e

- Commenti a blocco, delimitati da `/*` e `*/`.

== Separatori
Al fine di disambiguare la grammatica e evitare conflitti è fondamentale l'introduzione di separatori alla fine degli Statement, salvo If e Loop, e delle dichiarazioni di costanti e variabili.\ Senza i separatori, infatti, il parser LALR(1) non sarebbe in grado di distinguere con certezza la struttura del codice in determinate situazioni.

Ad esempio:
```
  var
    a: Integer := b
  ( ...
  ```
  In questo caso, con un lookahead di 1 token, non è possibile determinare se `b (` rappresenti l'inizio di una chiamata a funzione oppure se `(` sia l'inizio di una Left Expression.
  
  Un'altra ambiguità possibile è la seguente:
  ```
  var
    a: Integer := b
  * ...
  ```

  dove `*` potrebbe essere sia un deref che una moltiplicazione.
  Per lo stesso motivo è stato aggiunto un `;` anche nell'exit dei loop.


== Risoluzione conflitti rimanenti
La grammatica definita presenta un solo conflitto, che si verifica nello stato 104, tra le produzioni di `LExp` e `RExp`.
Questo conflitto non crea problemi per l'analisi sintattica, dal momento che le `LExp` sono un sottoinsieme delle `RExp`. 
Pertanto, è sufficiente scegliere sempre l'azione di shift.


// TODO: spiegare come lo abbiamo implementato, approccio pipeline: prima viene creato l'AST, poi vengono fatti i check di semantica statica ed infine viene generato il TAC.

= Definizione del lexer e del parser
Lexer e Parser sono stati realizzati modificando i file-prototipo generati automaticamente da BNFC, in modo tale da adattarli alle specifiche della consegna. La grammatica utilizzata per la costruzione di lexer e parser è quella presente nel file `grammar.cf` e genera il linguaggio descritto in precedenza.

== Modifiche al lexer
- Oltre la posizione iniziale di un token è stata aggiunta anche la sua posizione finale, in modo da poter generare errori più precisi.

== Modifiche al Parser
Il parser è stato modificato in modo da utilizzare la sintassi astratta da noi definita nel file `AbsSyn.hs` (a differenza di quella generata da BNFC). La struttura delle regole non è stata alterata.


= TODO:
- [ ] disambiguare funzioni con lo stesso nome nel tac


= Definizione del Type system
== Gerargia dei tipi
Per il nostro linguaggio, abbiamo definito la seguente gerarchia dei tipi:\ \
#image("img/types.jpg", width: 100%, )

== Regole formali per il type system
Per definire il tipo di ogni nodo dell'albero di sintassi astratta, sono state definite delle regole formali. Queste regole sono utilizzate per determinare il tipo di espressioni, letterali e identificatori, in base all'ambiente corrente e ai tipi associati.
\ \

Di seguito sono riportate le regole formali utilizzate per definire tali tipi.

=== Expression

=== Literal

$
space/ ("env" ⊢ "Literal" { "val" = "LitInt" {"intVal" = e} } : "INT")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitChar" {"charVal" = e} } : "CHAR")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitString" {"stringVal" = e} } : "STRING")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitBool" {"boolVal" = e} } : "BOOL")
$
$
space/ ("env" ⊢ "Literal" { "val" = "LitDouble" {"doubleVal" = e} } : "REAL")
$

=== Ident
$
space/ ("env" ⊢ "ExpIdent" { "name" = id } : "env"(id))
$

===


SBAGLIATE
$
("env" ⊢ e : "BOOL")/("env" ⊢ "Not" { "exp" = e } : "BOOL")
$
$
("env" ⊢ e : T)/("env" ⊢ "Ref" { "exp" = e } : "&"T) ?
$
$
("env" ⊢ e : "*"T) /("env" ⊢ "Deref" { "exp" = e } : T)
$

= Generazione del Three Address Code (TAC)

Oltre alle istruzioni viste a lezione sono state aggiunte le seguenti istruzioni:
- `ArrayGen addr size` per la creazione di un array di dimensione `size`.
- `Custom str` per la generazione di un istruzione personalizzata, viene utilizzata per l'istruzione `END_PROGRAM` che indica la fine del programma
- `Comment` per la generazione di commenti nel codice TAC.

Altro da dire:
- Le stringhe (letterali) vengono allocate in una sezione statica, e sono accessibili tramite un indirizzo statico.
- Le espressioni e i parametri vengono generate da sinistra a destra (perché più intuitivo di destra verso sinistra?). Invece gli assegnamenti multipli vengono trattati come zucchero sintattico di assegnamenti "su più linee", ad esempio:
```
x, y, z := 0, 1, 2;
```
risulta nello stesso codice TAC di:
```
x := 0;
y := 1;
z := 2;
```
- le modalità di passaggio sono gestite nel seguente modo:
  - Per valore: i parametri formali vengono copiati nei nomi dei parametri locali e quindi viene chiamata la funzione.
  - Per ref: prima della chiamata di funzione ai parametri locali viene assegnata la reference dei parametri formali, quindi viene chiamata la funzione. All'interno del corpo della funzione prima di ogni utilizzo di parametri locali viene generata un'istruzione di deref. 
  - Per valres: prima della chiamata di funzione ai parametri locali viene assegnata la reference dei parametri formali, quindi viene chiamata la funzione. Prima del corpo della funzione i parametri locali vengono deferenziati in una copia locale (con un apice alla fine del nome). Prima di ogni `return` quindi viene assegnato il valore della copia locale al parametro locale deferenziato.
- Le funzioni di relazione tra stringhe e array sono generate direttamente nel TAC, con nomi che seguono la convenzione `string<relop>'` o `array<relop><type>'`, dove `<relop>` è l'operatore di relazione e `<type>` è il tipo intero dell'array.
- sia le espressioni booleane che le condizioni per il flow-control sono valutate con short-circuit usando il fall-trough

Il modulo di generazione del TAC (reperibile nella cartella `TAC`) utilizza una monade di stato per generare il codice, consentendo di mantenere traccia di informazioni di stato, quali:
- contatori per la creazione di temporanei, di label e di indirizzi statici
- lo stream di output per il codice generato (al contrario, come tutti gli altri stream)
- uno stack di stream di codice per la gestione di routine annidate
- uno stream per i dati statici (stringhe)
- Una lista di label che viene associata alla prossima istruzione
- Una lista di coppie di label, per la gestione degli exit on/continue all'interno dei loop (possibilmente annidati)
- Un insieme che tiene traccia quali funzioni di comparazione tra stringhe o array sono già state generate

Nel file `TAC/State.hs`, oltre alla definizione dello stato appena descritto sono presenti anche funzioni di supporto per la gestione dello stato.

Notazioni usate nel printer:
- Gli identificatori e nomi di sono annotati con la posizione nel file originale in questo modo: `<ident>@<line>:<column>`
- i commenti sono preceduti da `#`
- Le copie locali dei parametri passati per `valres` hanno il nome del parametro seguito da un apice (')
- le funzioni di relazione tra stringhe e array hanno un apice (') alla fine del nome
- le variabili che puntano a dati statici sono precedute da `$`

// Cose che potrebbe chiedere:
// - Switch
// - break
// - loop determinato
// - array checked
// - try/catch

#bibliography("bibliografia.bib", title: "Bibliografia" , style: "ieee")