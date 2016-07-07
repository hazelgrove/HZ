# Js_of_ocaml HZ (Holes and Zippers)

> Js_of_ocaml (abbreviated jsoo) is a compiler from OCaml bytecode to Javascript.

## Resources

- [Website](http://ocsigen.org/js_of_ocaml/)
- [GitHub](https://github.com/ocsigen/js_of_ocaml)
- [Try Js_of_ocaml](http://try.ocamlpro.com/js_of_ocaml/)

## Support

Js_of_ocaml is part of the [Ocsigen project](http://ocsigen.org/).

- [Mailing list](https://sympa.inria.fr/sympa/subscribe/ocsigen)
- IRC : #ocsigen on irc.freenode.net


## Installing js_of_ocaml and tyxml

An easy way is to use [opam](https://opam.ocaml.org/). After having installed `opam`, follow these steps:

  - If you are using `opam` for the first time, you have to initialize it:

    ```sh
    > opam init
    > eval `opam config env`
    ```

    This will create a `.opam` directory in your home.

  - You need a recent version of the OCaml compiler. First check the current version used by `opam`:

    ```sh
    > opam switch
    --     -- 3.11.2  Official 3.11.2 release
    --     -- 3.12.1  Official 3.12.1 release
    --     -- 4.00.0  Official 4.00.0 release
    --     -- 4.00.1  Official 4.00.1 release
    --     -- 4.01.0  Official 4.01.0 release
    --     -- 4.02.0  Official 4.02.0 release
    --     -- 4.02.1  Official 4.02.1 release
    --     -- 4.02.2  Official 4.02.2 release
    system  C system  System compiler (4.02.1)
    ```

    The `C` marks the current compiler. Here version 4.02.1 is installed. We can see that a more recent version is available (4.02.2). So we will install it with `opam switch 4.02.2`. This won't remove the system compiler as `opam` will install the files in your `.opam` directory.

    The following command switches out the current compiler with the newly installed one and sets up your path to use it permanently.

    ```sh
    > opam switch 4.02.2
    > eval `opam config env`
    ```

  - We can now install Js_of_ocaml and tyxml, including optional dependencies.
 
    ```sh
    > opam install js_of_ocaml tyxml deriving ppx_deriving reactiveData ocp-indent
    ```

  - To make sure you have the latest version, ask `opam` to upgrade the packages if needed:

    ```sh
    > opam update
    > opam upgrade
    ````

  Congratulations, you now have all the required packages! We can now build the application.

## Compilation

You can execute build.sh to compile hz.ml.

```sh
> /bin/sh build.sh
```

It consists of two steps:

1. Compile the `hz.ml` file to OCaml bytecode with the `ocamlbuild` command:

  ```sh
  > ocamlbuild -use-ocamlfind \
  -pkgs lwt.ppx,js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml,react,reactiveData \
  hz.byte ;
  ```

2. Build the Javascript file from the `hz.byte` file with the `js_of_ocaml` command:

  ```sh
  > js_of_ocaml +weak.js --opt 3 -o www/js/hz.js hz.byte
  ```

  The command options are:
  - `+weak.js` to include the necessary `weak` package.
  - `-o hello.js` to set output file name.

  You can add `--opt 3` to optimize more heavily.

## Results
You can now open hz.html in a browser to see it working.

## Credit

Based on the readme.md file for the [TodoMVC example](https://github.com/slegrand45/examples_ocsigen/tree/master/jsoo/todomvc-react) by [Stéphane Legrand](https://github.com/slegrand45).

