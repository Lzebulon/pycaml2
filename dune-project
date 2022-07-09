(lang dune 3.1)

(name pycaml)

(generate_opam_files true)

(source
 (github Lzebulon/pycaml2))

(authors "Lzebulon")

(maintainers "Lzebulon")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name pycaml)
 (synopsis "ocaml with the mandatory indentations of python")
 (description "ocaml with the mandatory indentations of python for learn how to create a compiler with a bullshit")
 (depends ocaml dune)
 (tags
  (pycaml indentation compiler python ocaml)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
