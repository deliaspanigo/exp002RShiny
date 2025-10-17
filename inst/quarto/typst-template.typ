#let article(
  // The book's title.
  title: "Book title",
  // The book's author.
  author: "Author",
  // The book's author.
  species: "default species",
  // The paper size to use.
  paper: "a4",
  // The book's content.
  body,
) = {
  // Set the document's metadata.
  set document(title: title, author: author)

  // Set the body font.
  set text(font: "Source Sans Pro")

  // Configure the page properties.
  set page(
    paper: paper,
    background: place(
      top,
      rect(
        width: 2cm,
        height: 100%,
        fill: rgb("#0D2C54"),
      ),
    ),
    margin: (bottom: 1cm, top: 1cm, left: 2.5cm, right: 2.5cm),
  )

  page(align(left + top)[
    #h(-1cm)
    #box(
      fill: rgb("#339989"),
      width: 0.7fr,
      inset: 10pt,
    )[
      #text(size: 2.5em, fill: white)[*#title*]
    ]
    #v(2em, weak: true)
    #body
  ])
}
