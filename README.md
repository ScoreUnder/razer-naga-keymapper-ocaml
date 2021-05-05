Razer Naga keypad mapper
========================

Mostly a translation of [@RaulPPelaez's `Naga_KeypadMapper`][orig].

Partly to get myself more comfortable with writing more "real-world" ocaml.

Compiling
---------

1. Install opam, as well as ensuring you have the x11 & xtest development
   libraries and linux kernel headers installed.
2. Switch into an OCaml 4.12.0 switch.
   ```
   opam switch install 4.12.0
   eval "$(opam env)"
   ```
3. Install dune
   ```
   opam install dune
   ```
4. Find dependencies and install them
   ```
   dune external-lib-deps --missing @all
   ```
   ...followed by, for example...
   ```
   dune install gen
   ```
5. Build
   ```
   dune build --release
   ```

The file should be built as `./razer_naga_keymapper.exe`. Yes, even on linux,
dune uses `.exe` to distinguish between bytecode and native builds.

Running
-------

If you invoke it as a user with read permissions on the right devices in
`/dev`, it should work out of the box. Bear in mind that if you use sudo, any
programs you put in the config will also be invoked with the same permissions,
which may prevent them from using dbus or x11 correctly. It is probably
preferable to give your user read access to the specific devices before running
it, or sudo back to your original user in the config. (Maybe I'll add privilege
dropping later).

Configuring
-----------

Same as upstream, except the config directory is located in the current working
directory rather than the user's XDG config directory.

Some actions have been removed:

* `workspace`, `workspace_r`: Invoked a command, which can be done with `run`. A more accurate command (e.g. `i3-msg`) can be used in different desktop environments intead.
* `position`: Again invoked a command, but also I did not see much use for this.
* `media`: Just use `key` and prefix it with `XF86`.
* `run2`: Use `1 press release repeat - run` for example.

Some have been added:

* `keytap`: Quickly presses and releases a chord of keys.
* `type`: Types out a string.

[orig]: https://github.com/RaulPPelaez/Naga_KeypadMapper
