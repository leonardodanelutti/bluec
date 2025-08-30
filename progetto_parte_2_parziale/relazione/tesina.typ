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

/*
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

*/

#colorbox(
    title: "Versioni",
    color: "gray",
    radius: 2pt,
    width: auto,
)[
    Di seguito le versioni degli strumenti utilizzati per realizzare questo
    progetto:
    + `BNFC`: 2.9.5;
    + `Happy`: 2.1.5;
    + `Alex`: 3.5.3.0;
    + `GHC`: 9.10.1.

    È fornito un `Dockerfile` con le versioni usate per semplificare l'uso del progetto.
]

= Definizione del linguaggio: scelte di design e risoluzione dei conflitti
Il linguaggio definito per questo progetto è un linguaggio di programmazione imperativo con scoping statico. La sua grammatica si basa sulla sintassi concreta del linguaggio Blue.

Poiché Blue è un linguaggio orientato agli oggetti, per garantire le funzionalità richieste dalla consegna, come per esempio il dereferenziamento, sono stati introdotti alcuni elementi. Tuttavia, non sono presenti né classi né interfacce.

== Tipi
Il linguaggio definito prevede i seguenti tipi primitivi:  `Boolean`, `Integer`, `Real`, `String`#footnote[Front-end di un compilatore per un linguaggio imperativo: note generali (p. 3, par. 3)] e `Char`.
Sono inoltre supportati gli array che sono dichiarati utilizzando nel modo seguente: `Array<Type, Integer>`, dove `Type` rappresenta il tipo degli elementi e  `Integer` indica la dimensione dell'array. Infine vi sono i puntatori, che sono dichiarati con la sintassi `&Type`.

== Costanti
Le costanti sono dichiarate con la keyword `const` e possono essere solo di tipo primitivo (non devono allocare spazio in memoria). Anche in questo caso non è consentito dichiarare costanti senza assegnamento #footnote[Front-end di un compilatore per un linguaggio imperativo: note generali (p. 3, par. 2)].

```blue
const
	a, b : Integer := 0, 1;
```

== Variabili
Le variabili sono dichiarate con la keyword `var` e possono essere di tipo primitivo, di tipo array o di tipo reference. Non è consentito dichiarare variabili senza assegnamento#footnote[Front-end di un compilatore per un linguaggio imperativo: note generali (p. 3, par. 2)].

```blue
var
	c : Real := 1.0;
```

=== Array Expressions

Oltre alla definizione classica, il costrutto array expression permette di inizializzare un array valutando un'espressione che può dipendere dall'indice dell'array, sia nel contesto monodimensionale che in quello multidimensionale.

```blue
-- Esempio di array expression
array: Array<Array<Int; 5>; 2> := [j*2 + i : i, j];
```

== Loop
Per il nostro linguaggio sono previste le forme di iterazione non determinata `while-do` e `do-while`. Entrambe le strutture sono naturalmente implementate dall'elemento `loop-exit on` della sintassi concreta di Blue.

```blue
-- Simulazione di while-do
loop
    exit on _;
    _
end loop

-- Simulazione di do-while
loop
    _
    exit on _;
end loop
```
\

Inoltre, è prevista la forma di iterazione determinata `for each`, che consente di iterare su un intervallo di valori.
La struttura viene implementata nel modo seguente:

```blue
-- Simulazione del ciclo for each
for _ in _ do
    _
end for
```
\

Nei cicli a iterazione indeterminata sono ammesse le istruzioni `break` e  `continue`. Nei cicli a iterazione determinata, tali istruzioni sono permesse soltanto se il ciclo è contenuto all'interno di un ciclo indeterminato. In questo caso, l'istruzione `break` provoca l'uscita dal ciclo più esterno.

== Condizionali
Il linguaggio immplementa le strutture condizionali semplici `if-then` e `if-then-else`, che permettono di eseguire blocchi di codice in base a condizioni specifiche.

```blue
if _ then
    _
else
    _
end if
```
\

Inoltre, è stato introdotto il costrutto `if-then-else` per la categoria sintattica delle espressioni, che consente di valutare un'espressione in base a una condizione booleana.

== Routine
Le routine comprendono sia funzioni che procedure. Una funzione senza parametri può essere invocata sia con la sintassi completa `function()` sia in forma abbreviata `function`, che è semplicemente zucchero sintattico per la prima. Rispetto alla sintassi concreta del linguaggio Blue, sono rimosse pre e post-condizioni; inoltre i parametri di ritorno non vengono nominati esplicitamente nella dichiarazione: si specificano solamente i tipi restituiti mediante la notazione `-> Type`. Si noti che non è possibile restituire tuple. Per gestire il ritorno esplicito dei valori, è stato introdotto il costrutto `return`.
Le dichiarazioni di funzioni, in ogni ambiente, devono essere precedute dalla keyword `routines`, in modo simile a quanto avviene in Blue per le dichiarazioni delle routine nelle classi. Questo è necessario per evitare ambiguità, come nel seguente esempio:
```blue
var
    a: Integer := b
main ...
```
dove `main` potrebbe rappresentare sia il nome di una funzione che il nome di una variabile.

Quando si invoca una funzione è possibile specificare la modalità di passaggio dei parametri in ingresso : `val` (il default), `valres` e `ref`.
Infine, poiché il linguaggio non prevede il concetto di classi, le funzioni non possono essere invocate con la notazione a punto, come `.function()`.

== Gestione delle eccezioni
Il linguaggio supporta la gestione delle eccezioni tramite le parole chiave `try`, `catch` e `throw`. Le eccezioni possono essere lanciate utilizzando `throw` e catturate con i blocchi `try-catch`.
Si osserva che, in questo contesto, non vengono distinte possibili categorie di errore specifiche.

```blue
-- Simulazione del blocco try-catch
try
    _
catch
    _
end try

-- Simulazione del throw
_ throw error;
```



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
La grammatica definita presenta un solo conflitto, che si verifica nello stato 108, tra le produzioni di `LExp` e `RExp`.
Questo conflitto è di tipo shift/reduce e non crea problemi dal momento che le `LExp` sono un sottoinsieme delle `RExp`. 
Pertanto, è sufficiente scegliere sempre l'azione di shift.


// TODO: spiegare come lo abbiamo implementato, approccio pipeline: prima viene creato l'AST, poi vengono fatti i check di semantica statica ed infine viene generato il TAC.

= Definizione del lexer e del parser
Lexer e Parser sono stati realizzati modificando i file-prototipo generati automaticamente da BNFC, in modo tale da adattarli alle specifiche della consegna. La grammatica utilizzata per la costruzione di lexer e parser è quella presente nel file `grammar.cf` e genera il linguaggio descritto in precedenza.

== Modifiche al lexer
Oltre la posizione iniziale di un token è stata aggiunta anche la sua posizione finale, in modo da poter generare errori più precisi.

== Modifiche al Parser
Il parser è stato modificato in modo da utilizzare la sintassi astratta da noi definita nel file `AbsSyn.hs` (a differenza di quella generata da BNFC). La struttura delle regole non è stata alterata.

/*
= TODO:
- [ ] disambiguare funzioni con lo stesso nome nel tac
*/

= Definizione del Type system
== Gerargia dei tipi
Per il nostro linguaggio, abbiamo definito la seguente gerarchia dei tipi:\ \
#figure(
    image("img/types.png", width: 100%, ),
    caption: "Gerarchia dei tipi.")<sup>

== Regole formali per il type system

Per definire il tipo di ogni nodo dell'albero di sintassi astratta, di seguito sono
riportate le regole formali del type system.

Alcune note:
- L'environemnt, oltre al suo normale scopo, viene usato per propagare la necessità che
  un'espressione sia di tipo left-hand side. Nelle regole viene omesso per semplificare la
  notazione, ma tutte le espressioni non-left-hand side tornano un errore se nel loro
  environment è presente la keyword `LExp`.
- Come spiegato nel paragrafo @ty-check_env, nell'environemnt i namespace di simboli e
  routine sono separati. Per semplicità di scrittura nelle regole del type-system il
  namespace è considerato come unico. Le eventuali ambiguità generano errori o sono
  gestite usando il contesto (in alcuni casi sono permesse solo left-hand side expression,
  e quindi non le chiamate a routine).
- Ci sono due modalità con cui vengono mergiati due envirnoment $a$ e $b$:
    - $a union b$: i binding di $b$ sovrascrivono quelli di $a$;
    - $a union.plus b$: in caso di clash viene generato un errore.

=== Espressioni

Di seguito le regole di annotazione delle espressioni. Si noti che i nodi interni
propagano sempre gli errori senza greare "tipi composti da errori" per evitare errori
annidati indotti da errori precedenti. Per semplicità di notazione tale processo è omesso
nelle seguenti regole.

\ *`LITERAL`*

I literal sono foglie dell'AST. Il loro tipo dipende solo dal tipo di nodo.

$
space/ ("env" ⊢""_E "Literal" { "val" = "LitInt" {"intVal" = e} } : "INT")
$
$
space/ ("env" ⊢""_E "Literal" { "val" = "LitChar" {"charVal" = e} } : "CHAR")
$
$
space/ ("env" ⊢""_E "Literal" { "val" = "LitString" {"stringVal" = e} } : "STRING")
$
$
space/ ("env" ⊢""_E "Literal" { "val" = "LitBool" {"boolVal" = e} } : "BOOL")
$
$
space/ ("env" ⊢""_E "Literal" { "val" = "LitDouble" {"doubleVal" = e} } : "REAL")
$

\ *`IDENT`*

Anche gli ident sono foglie dell'AST. Possono essere:
- variabili;
- costanti;
- chiamate a funzioni senza parametri.
In tutti i casi assumono il tipo associato al loro nome nell'ambiente.

$
space/ ("env" ⊢""_E "ExpIdent" { "name" = id } : "env"(id))
$

\ *`FUNCALL`*

Le chiamate di funzione prendono il tipo dall'environment. Gli argomenti supportano il
casting implicito solo se passati per valore. Inoltre se usano un metodo di passaggio
diverso da quello per valore devono essere espressioni left-hand side.

$
("env"(id) : "mode"_1 tau_1 times ... times "mode"_n tau_n -> tau
space space space space space space
forall i = 1, ..., n "checkArg"("env", e_i, tau_i, "mode"_i))
/
("env" ⊢""_E "FunCall" { "routCallName" = id, "args" = [e_1, ..., e_n] } : "env"(id))
$

dove:
$
"checkArg"("env", e, tau, "mode") \
= \
"env'" ⊢""_E e : tau' \
and \
"mode" = "val" -> ("sup"(tau', tau) = tau and "env'" = "env") \
and \
"mode" != "val" -> (tau' = tau and "env'" = "env" union "\"LExp\"")
$

\ *`SIGN`*

Il segno prevede un'espressione numerica: un Integer o un Real. Siccome
`sup(Integer, Real) = Real` (come definito in @sup), si prova prima a convertire `e` ad
`Integer`:

$
("env" ⊢""_E "exp" : tau'
space space space space space space
"sup"(tau', "Integer") = "Integer")
/
("env" ⊢""_E "Sign" { e = "exp" } : "Integer")
$

Se `sup(tau', Integer) != Integer` si prova a convertire `e` a `Real`:

$
("env" ⊢""_E "exp" : tau'
space space space space space space
"sup"(tau', "Real") = "Real" and "sup"(tau', "Integer") != "Integer")
/
("env" ⊢""_E "Sign" { e = "exp" } : "Real")
$

\ *`BINOP`*

Le operazioni binarie supportate da `blue` si dividono in 4 categorie:
- le operazioni booleane:
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Boolean") = "Boolean"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "AND", "e1" = e_1, "e2" = e_2 } : "Boolean")
  $
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Boolean") = "Boolean"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "OR", "e1" = e_1, "e2" = e_2 } : "Boolean")
  $
- le operazioni aritmetiche intere:
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Integer") = "Integer"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "DIVINT", "e1" = e_1, "e2" = e_2 } : "Integer")
  $
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Integer") = "Integer"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "MOD", "e1" = e_1, "e2" = e_2 } : "Integer")
  $
- le operazioni aritmetiche reali:
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Real") = "Real"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "DIV", "e1" = e_1, "e2" = e_2 } : "Real")
  $
- le operazioni aritmetiche intere e reali:
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Real") = "Real"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "ADD", "e1" = e_1, "e2" = e_2 } : sup(sup(tau_1, tau_2), "Integer"))
  $
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Real") = "Real"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "SUB", "e1" = e_1, "e2" = e_2 } : sup(sup(tau_1, tau_2), "Integer"))
  $
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Real") = "Real"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "MUL", "e1" = e_1, "e2" = e_2 } : sup(sup(tau_1, tau_2), "Integer"))
  $
  $
  (forall i = 1,2 ("env" ⊢""_E e_i : tau_i and sup(tau_i, "Real") = "Real"))
  /
  ("env" ⊢""_E "BinOp" { "binOp" = "POW", "e1" = e_1, "e2" = e_2 } : sup(sup(tau_1, tau_2), "Integer"))
  $

\ *`BOOLNEG`*

La negazione prevede un'espressione booleana:

$
("env" ⊢""_E "exp" : tau
space space space space space space
"sup"(tau, "Boolean"))
/
("env" ⊢""_E "BoolNeg" { e = "exp" } : "Boolean")
$

\ *`RELOP`*

Le operazioni relazioni supportate sono quelle di uguaglianza e di confronto. Sono
supportate da tutti i tipi, purchè convertibili allo stesso supertipo.

$
("env" ⊢""_E e_1 : tau_1
space space space space space space
"env" ⊢""_E e_2 : tau_2
space space space space space space
sup(tau_1, tau_2) = tau)
/
("env" ⊢""_E "RelOp" { "e1" = e_1, "e2" = e_2 } : "Boolean")
$

\ *`REF`*

È consentito creare reference soltanto di espressioni di tipo left-hand side.

$
("env'" ⊢""_E "exp" : tau
space space space space space space
"env'" = "env" union "\"LExp\"")
/
("env" ⊢""_E "Ref" { e = "exp" } : \&tau)
$

\ *`DEREF`*

Nei nodi dereference si controlla che l'espressione dereferenziata sia di tipo $\&tau$.

$
("env" ⊢""_E "exp" : \&tau)
/
("env" ⊢""_E "Deref" { e = "exp" } : tau)
$

\ *`ARRAYINDEX`*

Nell'accesso agli array l'indice è vincolato ad essere un tipo convertibile ad intero, e
l'array dev'essere un'espressione di tipo left-hand side.

$
("env'" ⊢""_E "exp" : "Array"<tau, n> and "env'" = "env" union "\"LExp\""
space space space space space space
"env" ⊢""_E "idx" : tau' and "sup"(tau', "Integer") = "Integer")
/
("env" ⊢""_E "ArrayIndex" { e = "exp", "index" = "idx" } : tau)
$

\ *`ARRAY`*

Per creare un nodo array tutti gli elementi dell'array devono avere tipi compatibili tra
loro. Il tipo finale contine la grandezza dell'array ed il least common ancestor tra tutti
gli elementi.

$
(forall i = 1, ..., n ("env" ⊢""_E e_i : tau'_i)
and
tau_1 = tau'_1 and forall i = 2, ..., n  (tau_i = "sup"(tau_(i-1), tau'_i)))
/
("env" ⊢""_E "Array" { "elements" = [e_1, ..., e_n] } : "Array"<tau_n, n>)
$

\ *`ARRAY EXPRESSION`*

TODO: è giusto?  no

$
("env" ⊢""_E e : tau' and sup(tau', tau) = tau
space space space space space space
forall i = 1, ..., n ("env" ⊢""_E x_i : "Integer" )
)
/
("env" ⊢""_E "ArrayExpr" { "e" = e , "indexes" = [x_1, ..., x_n]} : "Array"<tau, (d_1, ..., d_n)>)
$

dove $ "Array"<tau, (d_1, ..., d_n)> = "Array"<"Array"<... "Array"<tau, d_n> ..., d_2)>, d_1> $

\ *`IF-THEN-ELSE`*

L'if-then-else prevede che condExp sia di tipo booleano e che il tipo dei due rami coincida o sia compatibile.

  $
  ("env" ⊢""_E c : tau_c and sup(tau_c, "Bool") = "Bool" 
  space space space space space space
  "env" ⊢""_E e_1 : tau_1 
  space space space space space space
  "env" ⊢""_E e_2 : tau_2)
  /
  ("env" ⊢""_E "IfElse" { "condExp" = "c", "thenExp" = e_1, "elseExp" = e_2 } : sup(tau_1, tau_2))
  $

=== Statements

In `blue` gli statements sono separati dalle dichiarazioni. Quindi non introducono nuovi
binding.

\ *`STMTASS`*

Negli assegnamenti viene controllato che:
- l'espressione a sinistra sia un'espressione left-hand side;
- l'espressione a destra possa essere convertita al tipo dell'espressione a sinistra.

$
("env" ⊢""_S l : tau_l and "isLExp"(l) and "isNotConst"(l)
space space space space space space
"env" ⊢""_S r : tau_r
space space space space space space
"sup"(tau_l, tau_r) == tau_l)
/
("env" ⊢""_S "StmtAss" { "left" = l , "right" = r } : nothing)
$

\ *`STMTROUTCALL`*

Gli statement chiamate di routine vengono annotati come le espressioni chiamate di
funzione. Il tipo di ritorno è inserito tra parentesi per indicare la compatibilità della
regola sia per chiamate a procedure che funzioni.

$
("env"(id) : "mode"_1 tau_1 times ... times "mode"_n tau_n (-> tau)
space space space space space space
forall i = 1, ..., n "checkArg"("env", e_i, tau_i, "mode"_i))
/
("env" ⊢""_S "StmtRoutCall" { "call" = "RoutineCall" { "routCallName" = id, "args" = [e_1, ..., e_n] } } : nothing)
$

\ *`STMTRETRUN`*, *`STMTRETRUNEMPTY`*, *`STMTCONTINUE`*, *`STMTBREAK`*

Per la keyword `return` si controlla il tipo (se presente) che ha nell'environment.

$
("env" ⊢""_E e : tau and sup(tau, "env"("return")) == "env"("return"))
/
("env" ⊢""_S "StmtReturn" { "valReturn" = e } : nothing)
$

$
space
("env"("return") : nothing)
/
("env" ⊢""_S "StmtReturnEmpty" { } : nothing)
$

Per le keyword `continue` e `break` si controlla che siano presenti nell'environment.

$
space
("env"("continue") : nothing)
/
("env" ⊢""_S "annStmtContinue" { } : nothing)
$

$
space
("env"("break") : nothing)
/
("env" ⊢""_S "annStmtContinue" { } : nothing)
$

\ *`STMTIF`* e *`STMTIFELSE`*

Negli statement `if _ then _` e `if _ then _ else _` si controlla che la condizione sia
compatibile con i booleani.

$
space
("env" ⊢""_E e : tau and "sup"(tau, "Boolean") == "Boolean"
space space space space space space
"env" ⊢""_S s_1, ..., s_n)
/
("env" ⊢""_S "StmtIf" { "cond" = e, "body" = [s_1, ..., s_n] } : nothing)
$

$
space
("env" ⊢""_E e : tau and "sup"(tau, "Boolean") == "Boolean"
space space space space space space
"env" ⊢""_S s_"t1", ..., s_"tn"
space space space space space space
"env" ⊢""_S s_"f1", ..., s_"fn")
/
("env" ⊢""_S "StmtIfElse" { "cond" = e, "bodyT" = [s_"t1", ..., s_"tn"] "bodyT" = [s_"f1", ..., s_"fn"] } : nothing)
$

\ *`STMTLOOP`* e *`STMTLOOPEXIT`*

I loop chiamano ricorsivamente l'annotazione del body e delle condizioni di uscita. Viene aggiunta all'environment la keyword "continue" e "break" per segnalare al modulo di analisi semantica statica la possibilità di usare tali keyword.

$
space
("env" union "\"continue\"" union "\"break\"" ⊢""_S s_1, ..., s_n_s
space space space space space space
"env" union "\"continue\"" union "\"break\"" ⊢""_S e_1, ..., e_n_e)
/
("env" ⊢""_S "StmtLoop" { "body" = [s_1, ..., s_n_s], "exit" = [e_1, ..., e_n_e] } : nothing)
$

Nelle condizioni di uscita dai loop si controlla che la condizione sia compatibile con i
booleani.

$
space
("env" ⊢""_E e : tau and "sup"(tau, "Boolean") == "Boolean"
space space space space space space
"env" ⊢""_S s_1, ..., s_n)
/
("env" ⊢""_S "LoopExit" { "cons" = e, "body" = [s_1, ..., s_n] } : nothing)
$

\ *`STMTFOREACH`*

Nei loop `for each` non possono essere utilizzate le keyword `break` e `continue`, a meno che il loop non sia contenuto all'interno di un loop indeterminato. 

$
space
(
"env"  ⊢""_S s_1, ..., s_n_s
space space space space space space
"env" ⊢""_E a : "Array"<tau, n>
space space space space space space
"env" ⊢""_E id : tau
)
/
("env" ⊢""_S "StmtForEach" { "varIterName" = id, "arrayExp" = a, "body" = [s_1, ..., s_n_s] } : nothing)
$

\ *`STMTTRYCATCH`* e *`STMTTHROW`*

Nel blocco `try-catch` si annotano ricorsivamente i due blocchi. Non ci sono vincoli particolari sugli statement che possono essere usati nei due blocchi.

$
space
(
  "env"  ⊢""_S a_1, ..., a_n_a
  space space space space space space
  "env"  ⊢""_S b_1, ..., b_n_b
)
/
("env" ⊢""_S "StmtTryCatch" { "try" : [a_1, ..., a_n_a], "catch" : [b_1, ..., b_n_b] } : nothing)
$

Lo statement `throw` può essere usato in qualsiasi contesto.

$
space
()
/
("env" ⊢""_S "StmtThrow" { } : nothing)
$

=== Dichiarazioni

\ *`PROGRAMDECL`*

Un blocco `ProgramDecl` introduce nuove dichiarazioni di costanti, variabili e routine.
I binding delle routine sono visibili in tutto il blocco, e segnalano errore in presenza
di conflitti ($union.plus$). La dichiarazione di costanti e variabile procede in ordine
mascherando eventuali conflitti ($union$). L'ordine di merge tra simboli (costanti e
variabili) e routine non è importante in quanto namespace separati.

$
space
(forall i=1,...,n_c (("env" union.plus "env"_r) union union_0^(i-1) "env"_c_i ⊢""_D c_i: "env"_c_i) \
space space space space space space
forall i=1,...,n_v ((("env" union.plus "env"_r) union "env"_c) union union_0^(i-1) "env"_v_i ⊢""_D v_i: "env"_c_i) \
space space space space space space
forall i=1,...,n_r ((("env" union.plus "env"_r) union "env"_c) union "env"_v ⊢""_D r_i: "env"_c_i))
/
("env" ⊢""_D "ProgramDecl" { "consts" = [c_1, ..., c_n_c], "vars" = [v_1, ..., v_n_v],\
"routines" = [r_1, ..., r_n_r] } : "env"_c union "env"_v union "env"_r)
$

dove:
- $"env"_c_0$, $"env"_v_0$ e $"env"_r_0$ sono environment vuoti (senza binding);
- $"env"_c = union_1^n_c "env"_c_i$, $"env"_v = union_1^n_v "env"_v_i$ e $"env"_r = union_1^n_r "env"_r_i$.

\ *`CONSTASS`*

Nelle dichiarazioni di costanti si possono dichiarare più costanti assieme. L'environment
non viene propagato perchè è come se venissero dichiarate "contemporaneamente".

$
(forall i=1,...,n ("env" ⊢""_E e_i: tau_i and sup(tau, tau_i) = tau))
/
("env" ⊢""_D "ConstAss" {"constIdents" = [id_1, ..., id_n], "constType" = tau,\
"constValues" = [e_1, ..., e_n] } : {id_1: tau_1,...,id_n: tau_n})
$

\ *`VARASS`*

Nelle dichiarazioni di variabili si possono dichiarare più variabili assieme. L'environment
non viene propagato perchè è come se venissero dichiarate "contemporaneamente".

$
(forall i=1,...,n ("env" ⊢""_E e_i: tau_i and sup(tau, tau_i) = tau))
/
("env" ⊢""_D "ConstAss" {"varIdents" = [id_1, ..., id_n], "varType" = tau, "varValues" = [e_1, ..., e_n] }\ : {id_1: tau_1,...,id_n: tau_n})
$

\ *`FUNCTION`* e *`PROCEDURE`* e *`ROUTBODY`*

Le funzioni e le procedure vengono annotate all stesso modo. In entrambi i casi viene aggiunta all'environment la keyword `return` per segnalare al modulo di analisi semantica statica la possibilità di usare tale keyword (nel caso delle funzioni alla keyword è associato il tipo di ritorno).

$
("env'" ⊢""_D b
space space space space space space
"env'" = "env" union {id_1: tau_1, ..., id_n: tau_n} union {"\"return\"": tau})
/
("env" ⊢""_D "Function" { "routName" = id, "params" = [ id_1: "mode"_1 tau_1, ..., id_n: "mode"_n tau_n],\
"returnType" = tau, "body" = b } : {id : "mode"_1 tau_1 times ... times "mode"_n tau_n -> tau})
$

$
("env'" ⊢""_D b
space space space space space space
"env'" = "env" union {id_1: tau_1, ..., id_n: tau_n} union {"\"return\""})
/
("env" ⊢""_D "Procedure" { "routName" = id, "params" = [ id_1: "mode"_1 tau_1, ..., id_n: "mode"_n tau_n],\
"body" = b } : {id : "mode"_1 tau_1 times ... times "mode"_n tau_n})
$

Il boby delle dichiarazioni di routine

$
("env" ⊢""_D d : "env'"
space space space space space space
"env'" ⊢""_S s_1,...,s_n )
/
("env" ⊢""_S "RoutBody" { "declarations" = d, "statements" = [ s_1, ..., s_n], } : nothing)
$

= Analisi Semantica Statica
L'analisi semantica statica è implementata nel modulo `StatSem`. Più nello specifico è
diviso in 4 sotto-moduli: `Exp`, `Decl` e `Stmt` rispettivamente responsabili
dell'annotazione delle espressioni, delle dichiarazioni e degli statement, e `Utils` in
cui sono definite utility e tipi di dato usati in tutto il modulo, tra cui le annotazioni,
l'environment ed il casting.

== Type-checker ed environment <ty-check_env>
L'annotazione dei tipi delle espressioni segue le regole del type system definito sopra.

Per poter annotare correttamente i tipi viene propagato un environment a partire dal nodo
radice alle foglie (al contrario rispetto a come procede l'annotazione). Il namespace tra
routine e simboli (variabili e costanti) è separato. Questo talvolta causa possibili
problemi di ambiguità perchè le routine senza parametri possono essere chiamate anche
senza `()` (scrivere `main` è zucchero sintattico per `main()`) come nella sintassi
concreta di `blue`.

Inoltre è stato aggiunto il namespace `keyword` in cui, oltre a segnalare la possibilità
di usare le keyword `return` e `continue`, si può richiedere che l'espressione sia di tipo
left-hand side. Quindi quando un nodo viene annotato controlla nel proprio ambiente la
necessità o meno di essere un'espressione left-hand side.

L'ambiente iniziale in cui è eseguito il programma contiene le funzioni predefinite.

== Errori
Il modulo di analisi semantica statica gestisce tutti gli errori a compile-time e
garantisce al modulo di generazione del codice TAC un AST privo di errori.

=== Propagazione degli errori
Gli errori vengono propagati fino alla radice dell'AST. Se la radice non è priva di errori
il codice non viene tradotto in TAC. Gli errori sono gestiti in due modi diversi in base
al tipo di nodo:
- le espressioni che presentano errori hanno il tipo `TError`. In particolare può capitare
  che un'espressione abbia questo tipo in due casi:
    - c'è un errore nell'annotazione di quell'espressione. Quindi si crea un nuovo errore
      con un messaggio di errore opportuno;
    - c'è un errore in una o più sotto-espressioni. In questo caso si propagano gli errori
      (per questo il tipo `TError` ha una lista di `String`) evitando di generare nuovi
      errori annidati indotti (dagli errori precedenti).
- gli statement e le dichiarazioni non hanno tipo. Per questto motivo nelle loro
  annotazioni c'è un campo `errs :: [String]` contenente gli errori generati durante
  l'annotazione del nodo in questione e dei sotto-nodi.

=== Routine parziali
La definizione di routine parziali è vietata dal modulo di analisi semantica statica. In
particolare è garantito che ogni flusso di esecuzione del `body` delle routine esegua
indefinitivamente o incontri un `return _`.

A tal fine è presente un campo `alwaysReturn :: Bool` nelle annotazioni degli statement.
Questo campo prende significato solo per statement annidati dentro funzioni, e, come
suggerisce il nome:
- è vero se ritorna per ogni esecuzione dello statement. Ad esempio:
  ```
  if true then
    return 1;
  end if
  ```
- è falso altrimenti. Ad esempio se `a` è una variabile booleana:
  ```
  if a then
    return 1;
  end if
  ```
Chiaramente il controllo è conservativo: nell'esempio sopra se `a` assume sempre valore
`true` per qualunque esecuzione (ma è una variabile), non è possibile saperlo durante
l'analisi semantica statica.

L'annotazione di `alwaysReturn` procede dalle foglie fino alla radice dell'AST:
- le uniche foglie con `alwaysReturn = true` sono le keyword `return _` e `continue` (
  quest'ultima perchè è permesso ciclare all'infinito senza ritornare);
- i nodi interni dell'AST sono annotati in base a come controllano il flusso di
  esecuzione. // TODO: magari è da scrivere nelle regole del type system

Per compilare correttamente il campo `alwaysReturn` è stato aggiunto un campo `canExit`.
In generale questo campo è vero se lo statement può impedire l'esecuzione degli eventuali
statement che lo seguono (allo stesso livello). Ci sono due "eccezioni":
- il return ha `canExit = false` perchè ci interessano solo i flussi che escono senza
  ritornare;
- il continue ha `canExit = false` perchè, come specificato sopra, `continue` è
  considerato come una foglia che ritorna.

La stessa logica di controllo del flusso è stata usata per annotare un campo `unreachable`
utile per evitare di generare codice inutile in TAC. // TODO: al momento non funziona per gli exit?

=== Dangling references
Per evitare puntatori pendenti a variabili deallocate si è deciso di vietare il return di
reference nelle routine.

=== Presenza del main
L'esecuzione del programma corrisponde all'esecuzione della routine `main`. In particolare
dev'essere una procedura senza parametri in input. Se tale routine non è presente nella
dichiarazione più esterna del programma, viene generato un opportuno errore.

== Casting
È permesso il casting implicito. Il modulo di analisi semantica statica lo rende esplicito
per la successiva traduzione in codice TAC.

Per farlo è stata implementata una funzione di supertipo `sup` che, dati due tipi, ritorna
il least common ancestor in @sup. Il casting da $tau$ a $tau'$ è supportato sse
$"sup"(tau,tau') = tau'$. Tale regola vale dappertutto dove il casting è supportato (si
noti che il casting non è supportato per le modalità di passaggio per riferimento e per
value-result, in quanto richiedono delle espressioni di tipo left-hand side).

Il casting delle costanti avviene durante l'analisi semantica statica per risparmiare
tempo a run-time.

Il casting non è esteso ai tipi composti: ad esempio non è possibile convertire
direttamente un puntatore a `Integer` ad un puntatore a `Real`.

== Costanti
Nelle annotazioni delle espressioni un opportuno campo `ctVal` propaga finchè
determinabile a tempo di analisi semantica statica il valore costante dell'espressione. Le
costanti non possono essere array o reference.

== Inferenza di tipo.
TODO:

/*
== Altro che non so se è da dire
- durante gli assegnamenti (e dichiarazioni) il numeri di L-expr e di R-expr deve essere lo stesso
- Il numero di argomenti della funzione deve essere giusto
- Controllare che nessuna costante sia a sinistra di un assegnamento O in un parametro ref o valres
*/

= Generazione del Three Address Code (TAC)

La generazione del Three Address Code (TAC) è affidata ai moduli contenuti nella cartella `TAC`.

== Estensioni del linguaggio TAC
Oltre alle istruzioni presentate durante le lezioni, sono state introdotte le seguenti estensioni al linguaggio TAC:

- `ArrayGen addr size`, utilizzato per allocare un array di dimensione `size` e memorizzarne l'indirizzo in `addr`;
- `PushExceptionHandler label`, Il supporto a runtime inserisce il label `label` nello stack degli handler delle eccezioni;
- `PopExceptionHandler`, rimuove l'handler delle eccezioni più in alto nello stack;
- `Throw`, salta al label dell'handler delle eccezioni più in alto nello stack;
- `Custom str`, permette di generare istruzioni personalizzate; ad esempio `END_PROGRAM` che segnala la fine del programma;
- `Comment`, consente di inserire commenti nel codice TAC.
\

== Design e convenzioni del TAC
Alcuni aspetti rilevanti sono i seguenti:
- Le stringhe literal vengono allocate in una sezione statica e sono accessibili mediante un indirizzo statico. In generale una stringa è implementata come un puntatore ad un indirizzo di memoria contenente i caratteri della stringa terminati da un carattere nullo.
- Le espressioni e i parametri sono generati da sinistra verso destra.
- Negli assegnamenti multipli se una variabile appare alla sinistra e in una posizione successiva alla destra, il valore assegnato è quello precedente all'assegnamento. Ad esempio:
  ```
  x, y :=  y, x;
  ```
  Risulterà nello scambio dei valori di `x` e `y`, il codice generato è il seguente:

  ```
  xOld' = x;
  x = y;
  y = xOld';
  ```

  dove `<varName>Old'` è il nome di una nuova variabile che copia il valore locale del valore di `<varName>` prima dell'assegnamento.

- Le funzioni di relazione tra stringhe e array vengono generate direttamente nel TAC, seguendo la convenzione di denominazione:
  - `string<relop>'` per le relazioni tra stringhe;
  - `array<relop><type>'` per le relazioni tra array, dove `<relop>` rappresenta l'operatore di relazione e `<type>` il tipo degli elementi dell'array.

  I parametri locali di tali funzioni prendono il nome della funzione seguito da "val1", "val2" o "size" (nel caso degli array)

- Le espressioni booleane, le guardie degli if-then-else nelle espressioni e le condizioni di controllo del flusso (negli statement If e ExitOn) sono valutate mediante short-circuit usando il fall-trough.

- La variabile di iterazione nei loop `for each` viene trattata come un passaggio per reference: ad ogni iterazione la variabile contiene la reference dell'elemento corrente dell'array, se necessario nel corpo vengono effettuate delle dereferenziazioni per ottenere o aggiornare il valore dell'elemento.

- Le eccezioni vengono gestite mediante le istruzioni `PushExceptionHandler`, `PopExceptionHandler` e `Throw`. Quando viene raggiunto un blocco `try-catch` il codice TAC viene generato nel seguente modo:
  - prima del codice del blocco `try` viene inserita l'istruzione `PushExceptionHandler labelCatch`, dove `labelCatch` è il label che identifica l'inizio del blocco `catch`;
  - viene generato il codice del corpo del `try`;
  - alla fine del blocco `try` viene inserita l'istruzione `PopExceptionHandler`;
  - il codice dell'handler viene generato dopo il corpo della funzione corrente, viene quindi estratta l'etichetta dallo stack tramite un comando `PopExceptionHandler`, viene generato il corpo del blocco `catch` e infine viene inserita un'istruzione di salto incondizionato per il proseguimento del flusso di esecuzione dopo il blocco `try-catch`.

  Quando viene raggiunta l'istruzione `Throw`, il supporto a runtime ha il compito di saltare all'handler delle eccezioni più in alto nello stack. In questo modo quando viene generata un eccezione il flusso di esecuzione salta direttamente al blocco `catch` più interno che la gestisce.

  E' presente un handler delle eccezioni di default quando un eccezione noi viene mai gestita: viene stampato un messaggio di errore e il programma termina.

\

== Passaggio dei parametri
La gestione delle modalità di passaggio dei parametri nelle chiamate di funzione varia a seconda della modalità utilizzata:

- *Passaggio per valore* - i parametri formali vengono copiati nei corrispondenti parametri locali, dopodiché viene eseguita la chiamata alla funzione.

- *Passaggio per reference* - ai parametri locali viene assegnata la reference dei parametri formali. All'interno del corpo della funzione, ogni utilizzo dei parametri locali viene preceduto da un'istruzione di dereferenziazione.

- *Passaggio per valore di risultato* - prima della chiamata di funzione ai parametri locali viene assegnata la reference dei parametri formali. Prima del corpo della funzione i parametri locali vengono deferenziati n copie locali (con un apice alla fine del nome). Prima di ogni istruzione `return` il valore della copia locale viene assegnato al parametro locale deferenziato.

- *Passaggio per costante* - Se il tipo del parametro è un array allora il paramento viene passato per reference. Altrimenti viene passato per valore.

==  Dettagli implementativi

Il modulo di generazione del TAC (reperibile nella cartella `TAC`) utilizza una monade di stato per generare il codice, per la costruzione del codice, consentendo di mantenere informazioni di contesto quali:
- Contatori per la creazione di temporanei, label, indirizzi statici e label delle eccezioni;
- Lo stream di output per il codice generato (al contrario, come tutti gli altri stream);
- Uno stack di stream di codice per la gestione delle routine annidate;
- Uno stack contenete lo stream di codice delle funzione e rispettivi handler delle eccezioni, per la gestione delle funzioni annidate e delle eccezioni;
- Uno stream per i dati statici (stringhe);
- Una lista di label che viene associata alla prossima istruzione;
- Una lista di coppie di label per la gestione di istruzioni `exit`, `continue` e `break` all'interno di loop, anche annidati;
- Un insieme che tiene traccia delle funzioni di comparazione tra stringhe e array già generate.

Nel file `TAC/State.hs`, oltre alla definizione dello stato appena descritto, sono presenti anche funzioni di supporto per la gestione dello stato.

== Pretty printer
Il pretty printer del TAC utilizza la seguente notazione:
- I label sono del tipo `L<number>`;
- I label delle eccezioni sono del tipo `H<number>`;
- I temporanei sono del tipo `t<number>`;
- Gli identificatori e i nomi di variabili sono annotati con la posizione nel file originale secondo la sintassi: `<ident>@<line>:<column>`;
- I commenti sono preceduti da `#`;
- Le copie locali dei parametri passati per `valres` hanno il nome del parametro seguito da un apice `param'`;
- Anche le funzioni di relazione tra stringhe e array presentano un apice alla fine del nome `rel'`;
- Le variabili che puntano a dati statici sono precedute da `$`.

// Cose che potrebbe chiedere:
// - Switch
// - break
// - loop determinato
// - array checked
// - try/catch