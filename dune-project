(lang dune 3.9)

(name lucid)

(generate_opam_files true)

(source
 (github CharlesAverill/lucid))

(authors "Charles Averill")

(maintainers "Charles Averill")

(license LICENSE)

(documentation https://github.com/CharlesAverill/lucid)

(package
 (name lucid)
 (synopsis "CIC Interpreter")
 (description "An interpreter for the Calculus of Inductive Constructions")
 (depends ocaml dune graphics))

