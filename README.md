# datalogr

`datalogr` is an R package to query a Datomic database from R. It provides the following functionality:

- It communicates with a database remote instance
- It contains a DSL to write queries from R that are syntactically very similar to the dialect of Datalog used in Datomic
- It serves the query results as R data structures

## Design

Queries are represented as R data structures which are composed using the functionality provided by `datalogr`. Once ready, queries are serialized to JSON, and sent to the server.

JSON results from the server are in turn de-serialized into vanilla R datastructures such as data frames, matrices, vectors and lists

## Database access

You can decide which database to query by passing the `db.name` argument to the `do_query` function. Alternatively you can set a database globally for the entire session using `set_dbname`, this saves you from having to explicitly pass the database name with every query

## Query DSL

Queries are composed using the functions `query`, `find`, `where`, and optionally `args`. Individual `where` clauses are wrapped using the `d` function. The `query` function returns a data structure representing the query (queries can be manipulated as data), which is then passed to the `do_query` function.

Query arguments are written using the following syntax
```R
args(?arg-name <- value)
```
This causes `value` to be bound to the symbol `?arg-name` within the query context


For instance the following query

```clojure
(d/q '[:find ?release-name
       :in $ ?artist-name
       :where [?artist :artist/name ?artist-name]
              [?release :release/artists ?artist]
              [?release :release/name ?release-name]]
       db "Bruce Springsteen")
```

is written as follows

```R
qq <- query(
        find(?release-name),
        where(
            d(?artist, artist/name, ?artist-name),
            d(?release, release/artists, ?artist),
            d(?release, release/name, ?release-name)
        ),
        args(?artist-name <- "Bruce Springsteen")
)

do_query(qq)
```



The following rules apply when converting expression to Datalog:


|               |Datalog (Clojure)                   |R                                           |
|---------------|------------------------------------|--------------------------------------------|
|operators      |prefix, e.g. `(+ 2 3)`              |infix, e.g. `(2 + 3)`                       |
|function calls |S-expressions, e.g. `(max ?dur)`    |regular R syntax, e.g. `max(?dur)`      |
|blank variable |`_`                                 |`.`                                         |
|attribute names|start with `:`, e.g. `:artist/name`   |the initial `:` is omitted, e.g. `artist/name`|

Additionally the R DSL provides a mechanism for quasi-quotation. Symbols preceded by `!!` are replaced with their bound value before the query is parsed. For instance the following queries:

```R
query(
    find(?a),
    where(d(?a, artist/name, ?artist-name)),
    args(?artist-name <- "Bruce Springsteen")
)
```


```R
artist.name <- "Bruce Springsteen"

query(
    find(?a),
    where(d(?a, artist/name, ?artist-name)),
    args(?artist-name <- !!artist.name)
)
```
are equivalent

## Combining queries

Queries are data and can be combined using the `c_query` function. This is useful for assembling queries from fragments programmatically.

## Pull expressions

`datalogr` also support the Datomic pull expression syntax (see [here](https://docs.datomic.com/on-prem/pull.html) for more information).
Pull expressions can be included in the `find` clause of a query (e.g. `query(find(pull(...))))` As with the query syntax described above, there are minor differences between the Clojure and R syntax

|               |Clojure                                   |R                                                  |
|---------------|------------------------------------------|---------------------------------------------------|
|vectors        |`[]`                                      |`c()`                                              |
|maps           |`{key val}`                               |`{key = val}`                                      |
|back references|`_`  e.g.  `artist/_country`                |`.` e.g. `artist/.country`                           |
|wildcard       |`*`                                       |`.` (isolated, not part of an attribute name)      |
|attribute names|start with `:`, e.g `:artist/name`          |the initial `:` is omitted, e.g. `artist/name`       |
|`as` syntax    |start with `:`, e.g. `:db/ident :as ident`|the initial `:` is omitted, e.g `db/ident as ident`|

For instance the following pull expression in Clojure

```clojure
(pull ?release '[* {:release/labels [*]}])
```

is equivalent to the R

```R
pull(?release, c(., {release/labels = c(.)}))
```

which can be used in the context of a query as follows

```R
qq <- query(
    find(pull(?release, c(., {release/labels = c(.)}))),
    where(
        d(?artist, artist/name, ?artist-name),
        d(?release, release/artists, ?artist)
    ),
    args(?artist-name <- "Bruce Springsteen")
)

do_query(qq)
```
