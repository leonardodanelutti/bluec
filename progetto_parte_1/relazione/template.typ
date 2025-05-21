
// FIXME: workaround for the lack of `std` scope
#let std-bibliography = bibliography

#let template(
  // Your thesis title
  title: [Parte 1],

  // The academic year you're graduating in
  academic-year: [2024/2025],

  // Your thesis subtitle, should be something along the lines of
  // "Bachelor's Thesis", "Tesi di Laurea Triennale" etc.
  course: [INFORMATICA],

  // The paper size, refer to https://typst.app/docs/reference/layout/page/#parameters-paper for all the available options
  paper-size: "a4",

  // Candidate's informations. You should specify a `name` key and
  // `matricola` key
  candidate1: (
    name: "Francesco Bortuzzo",
    matricola: 157430
  ),

  candidate2: (
    name: "Leonardo Danelutti",
    matricola: 158786 
  ),

  candidate3: (
    name: "Lorenzo Della Giustina",
    matricola: 158854 
  ),

  // Set to "it" for the italian template
  lang: "it",

  // The thesis' bibliography, should be passed as a call to the
  // `bibliography` function or `none` if you don't need
  // to include a bibliography
  bibliography: none,

  // Abstract of the thesis, set to none if not needed
  abstract: none,

  // The thesis' content
  body
) = {
  // Set document matadata.
  set document(title: title, author: candidate1.name)

  // Set the body font, "New Computer Modern" gives a LaTeX-like look
  set text(font: "New Computer Modern", lang: lang, size: 10pt)

  // Configure equation numbering and spacing.
  set math.equation(numbering: "(1)")
  show math.equation: set block(spacing: 0.65em)

  // Configure raw text/code blocks
  show raw.where(block: true): set text(size: 0.8em)
  show raw.where(block: true): set par(justify: false)
  show raw.where(block: true): set align(left)
  show raw.where(block: true): block.with(
    fill: gradient.linear(luma(240), luma(245), angle: 270deg),
    inset: 10pt,
    radius: 4pt,
    width: 100%,
  )
  show raw.where(block: false): box.with(
    fill: gradient.linear(luma(240), luma(245), angle: 270deg),
    inset: (x: 3pt, y: 0pt),
    outset: (y: 3pt),
    radius: 2pt,
  )

  // Configure figure's captions
  show figure.caption: set text(size: 0.8em)

  // Configure lists and enumerations.
  set enum(indent: 10pt, body-indent: 9pt)
  set list(indent: 10pt, body-indent: 9pt, marker: ([•], [--]))

  // Configure headings
  set heading(numbering: "1.a.I.")
  show heading.where(level: 1): it => {
    if it.body not in ([References], [Riferimenti]) {
      block(width: auto, height: 20%)[
        #set align(center + horizon)
        #set text(1.3em, weight: "bold")
        #smallcaps(it)
      ]
    } else {
      block(width: auto, height: 8%)[
        #set align(center + horizon)
        #set text(1.1em, weight: "bold", hyphenate: true)
        #smallcaps(it)
      ]
    
    }
  }
  show heading.where(level: 2): it => block(width: 100%)[
    #set align(center)
    #set text(1.1em, weight: "bold")
    #v(1em)
    #smallcaps(it)
    #v(1em)
  ]
  show heading.where(level: 3): it => block(width: 100%)[
    #set align(left)
    #set text(1em, weight: "bold")
    #smallcaps(it)
  ]

  // Title page
  // Configure the page
  set page(
    paper: paper-size,

    margin: (right: 1.5cm, left: 2cm, top: 1cm, bottom: 3cm)
  )

  set align(center)

  rect(
    fill: rgb("#0000FF"),
    width: 100%,
    height: 55pt,
    grid(
      columns: (140pt, 1fr),
      align: left + horizon,
      pad(
        left: 8pt,
        top: 4pt,
        bottom: 2pt,
        image("img/logo.svg"),
      ),
      text(0.67em, font:"Arial", weight: 700, fill: white)[ DIPARTIMENTO DI SCIENZE MATEMATICHE, INFORMATICHE E FISICHE ]
    )
  )


  v(5fr)

  text(1.5em, [PROGETTO DI LINGUAGGI E COMPILATORI])
  v(1.5fr, weak: true)
  text(2em, weight: 700, title)

  v(8fr)

  grid(
    // set cell padding from both sides
    columns: 2,
    column-gutter: 45%,
    align: left,
    text(1.2em, weight: 700, fill: rgb("#000000"),)[
      #candidate1.name\
      #candidate2.name\
      #candidate3.name
    ],
    text(1.2em, weight: 700, fill: rgb("#000000"))[
      #candidate1.matricola\
      #candidate2.matricola\
      #candidate3.matricola
    ],
  )

  v(5fr)

  text(1.2em, [
    #if lang == "en" {
      "Academic Year "
    } else {
      "Anno Accademico "
    }
    #academic-year
  ])

  set par(justify: true, first-line-indent: 1.25em)
  set align(center + horizon)


  // Configure the page
  set page(
    paper: paper-size,

    // Margins are taken from the university's guidelines
    margin: (right: 3cm, left: 3.5cm, top: 3.5cm, bottom: 3.5cm)
  )

  // pagebreak for index
  pagebreak(weak: true, to: "odd")

  // Table of contents
  // Outline customization
   // Abstract
  if abstract != none {
    heading(
      level: 2,
      numbering: none,
      outlined: false,
      "Abstract"
    )
    abstract
  }

  show outline.entry.where(level: 3): it => {
    text(size: 0.8em, it)
  }
  
  outline(depth: 3, indent: auto)


  // Main body

  show link: underline
  set page(numbering: "1")
  set align(top + left)
  counter(page).update(1)

  body

  pagebreak(to: "odd")

}

//define taables for productions and parsing tables
#let dataSize(dataSize, other) = {
  tablex(
    
  )
}