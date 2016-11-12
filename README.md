# Emacs Configuration
### Primary Focus
- Clojure
- Rust
- SuperCollider
- Javascript
- HTML
- CSS / SCSS
- Lua
- Haskell

### Quick Install
```bash <(curl -fsSL https://raw.githubusercontent.com/paullucas/emacs/master/install.sh)```

## Configuration
There are a few steps you will need to take to fully configure all of the features.

### PATH Variables
You will need to make sure Emacs is aware of your PATH variables. 
#### Are you using <a href="http://ohmyz.sh/">Oh My Zsh</a> (or <a href="http://www.zsh.org/">Zsh</a>) on Linux?
The current configuration will work. 
#### Are you using <a href="https://www.gnu.org/software/bash/">Bash</a> on Linux? 
You will need to change <a href="https://github.com/paullucas/emacs/blob/master/init.el#L112">line 112</a> to:
``` emacs-lisp
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
```
#### Are you using OSX?
You will need to configure the <a href="https://github.com/purcell/exec-path-from-shell">exec-path-from-shell</a> package.
<br>
Delete the following block from init.el (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L111#L117">line 111 - 117</a>):
``` emacs-lisp
;; PATH Variables
(let ((path (shell-command-to-string ". ~/.zshrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))
```
<br>
On <a href="https://github.com/paullucas/emacs/blob/master/init.el#L101">line 101</a>, add the following:
``` emacs-lisp
(use-package exec-path-from-shell :ensure exec-path-from-shell)
```
<br>
On <a href="https://github.com/paullucas/emacs/blob/master/init.el#L280">line 280</a>, add the following:
``` emacs-lisp
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
```
<br>
Restart Emacs and your PATH variables should now be loaded.

### Rust
Install <a href="https://www.rust-lang.org/">Rust</a> using <a href="https://www.rustup.rs/">rustup.rs</a>
<br>
Find the current version of Rust installed on your system (eg. 1.13.0)
<br>
```rustc --version```
<br>
<a href="https://www.rust-lang.org/downloads.html">Download</a> the source code for the corresponding version and place it in your home folder (eg ```~/rustc-1.13.0/```)
<br>
Install the <a href="https://crates.io/crates/racer">Racer</a> package:
<br>
```cargo install racer```
<br>
Install the <a href="https://crates.io/crates/rustfmt">Rustfmt</a> package:
<br>
```cargo install rustfmt```

### SuperCollider
You will need to compile <a href="https://github.com/supercollider/supercollider">SuperCollider</a>.

### Tidal
Once <a href="http://tidalcycles.org/">Tidal</a> is installed, create a tidal directory in your home folder:
<br>
```mkdir ~/tidal```
<br>
Download <a href="https://github.com/tidalcycles/Tidal/blob/master/tidal.el">tidal.el</a> and place it inside the tidal folder (```~/tidal/tidal.el```)

### Javascript / JSX
Install <a href="https://nodejs.org/">Node.js</a> (I recommend using <a href="https://github.com/creationix/nvm">NVM</a>)
<br>
Install the required <a href="http://eslint.org/">ESLint</a> packages globally:
<br>
```sudo npm install -g eslint babel-eslint eslint-plugin-react```
<br>
Create a .eslintrc file in your home folder:
<br>
```touch ~/.eslintrc```
<br>
Configure the .eslintrc file:
``` json
{
  "parser": "babel-eslint",
  "plugins": [ "react" ],
  "env": {
    "browser": true,
    "es6": true,
    "node": true
  },
  "ecmaFeatures": {
    "arrowFunctions": true,
    "blockBindings": true,
    "classes": true,
    "defaultParams": true,
    "destructuring": true,
    "forOf": true,
    "generators": true,
    "modules": true,
    "spread": true,
    "templateStrings": true,
    "jsx": true
  },
  "rules": {
    "consistent-return": [0],
    "key-spacing": [0],
    "quotes": [0],
    "new-cap": [0],
    "no-multi-spaces": [0],
    "no-shadow": [0],
    "no-unused-vars": [1],
    "no-use-before-define": [2, "nofunc"],
    "react/jsx-no-undef": 1,
    "react/jsx-uses-react": 1,
    "react/jsx-uses-vars": 1
  }
}
```

### Indentation
By default there is a 2 space indentation preference for Javascript, JSX, CSS, HTML, and text files.
<br>
If you prefer 4 space indentation, simply edit <a href="https://github.com/paullucas/emacs/blob/master/init.el#L180">line 180</a> & <a href="https://github.com/paullucas/emacs/blob/master/init.el#L186">line 186</a>.

### Disabling Features
#### Disable Rust Features
Remove the following block from init.el (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L258#L171">line 258 - 271</a>)
``` emacs-lisp
;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
(setq racer-rust-src-path (file-truename "~/rustc-1.13.0/src"))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(setq company-tooltip-align-annotations t)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
```

#### Disable SuperCollider Features
Remove the following block from init.el (line 273 - 275)
``` emacs-lisp
;; SuperCollider
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/SuperCollider")
(require 'sclang)
```

#### Disable Tidal Features
Remove the following block from init.el (line 277 - 279)
``` emacs-lisp
;; Tidal
(add-to-list 'load-path "~/tidal")
(require 'tidal)
```

## Thanks
Javascript configuration: <a href="http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html">codewinds.com</a>
<br>
Rust configuration: <a href="http://emacsist.com/10425">emacsist.com</a>
<br>
Clojure configuration: <a href="https://github.com/ftravers">ftravers</a>
