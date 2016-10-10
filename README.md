# Emacs Configuration
### Primary Focus
- Clojure
- SuperCollider
- Javascript
- HTML
- CSS / SCSS
- Rust
- Lua
- Haskell

### Assumptions
- <a href="https://github.com/supercollider/supercollider/">SuperCollider</a> is <a href="http://paullucas.github.io/2016/Supercollider-on-Ubuntu-16.04.html">compiled</a>  (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L139">here</a>)
- 2 space indentation preference (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L193">here</a>) (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L199">here</a>)
- Using Zsh (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L124">here</a>)
- Rust installed (<a href="https://www.rustup.rs/">rustup.rs</a>) (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L266">here</a>)
- Rust source-code downloaded (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L267">here</a>)
- Racer package installed (cargo install racer)

### Quick Install (Linux)
```bash <(curl -fsSL https://raw.githubusercontent.com/paullucas/emacs/master/install.sh)```

### Disabled
- <a href="https://github.com/politza/pdf-tools">Pdf-tools</a> (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L90">here</a>) (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L233">here</a>)<br>
  Install dependencies:
    <pre>sudo apt-get install gcc g++ make automake autoconf libpng-dev libz-dev libpoppler-glib-dev libpoppler-private-dev</pre>
- Autocomplete (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L243">here</a>)
- Tidal (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L124">here</a>)
