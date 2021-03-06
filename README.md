#한국판 변경 이맥스 프로젝트.
# Emacs Configuration
## Primary Focus
- Clojure / ClojureScript
- Rust
- SuperCollider
- Javascript (React.js & Vue.js support)
- HTML
- CSS / SCSS
- Lua
- Haskell

# Install
1. Run the installation script:
<br>
```bash <(curl -fsSL https://raw.githubusercontent.com/paullucas/emacs/master/install.sh)```
<br>
2. Once the installation script completes, Emacs will open. 
<br>Wait until Emacs is finished downloading packages & compiling.
<br>Hint: You will see a Compile-Log buffer
<br>
3. Close Emacs and follow the Setup guide bellow
4. Once you have completed the Setup guide, launch Emacs

# Setup
There are a few steps you will need to take to complete the configuration process.

## PATH Variables
You will need to make sure Emacs is aware of your <a href="https://en.wikipedia.org/wiki/PATH_(variable)">PATH variables</a>. 
### Are you using <a href="http://ohmyz.sh/">Oh My Zsh</a> (or <a href="http://www.zsh.org/">Zsh</a>) on GNU/Linux?
The current configuration will work. 
### Are you using <a href="https://www.gnu.org/software/bash/">Bash</a> on GNU/Linux? 
You will need to change <a href="https://github.com/paullucas/emacs/blob/master/init.el#L106">line 106</a> to:
``` emacs-lisp
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
```
### Are you using OSX?
You will need to configure the <a href="https://github.com/purcell/exec-path-from-shell">exec-path-from-shell</a> package.
<br>
<br>
1. Delete the following block from init.el (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L105#L111">line 105 - 111</a>):
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
2. On <a href="https://github.com/paullucas/emacs/blob/master/init.el#L104">line 104</a>, add the following:
``` emacs-lisp
(use-package exec-path-from-shell :ensure exec-path-from-shell)
```
<br>
3. On <a href="https://github.com/paullucas/emacs/blob/master/init.el#L280">line 280</a>, add the following:
``` emacs-lisp
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
```
<hr>

## Rust
1. Install <a href="https://www.rust-lang.org/">Rust</a> using <a href="https://www.rustup.rs/">rustup.rs</a>
<br>
2. Find the current version of Rust installed on your system (example: ```1.13.0```)
<br>
```rustc --version```
<br>
3. <a href="https://www.rust-lang.org/downloads.html">Download</a> the source code for your version, place it in your home folder (example: ```~/rustc-1.13.0/```)
<br>
4. Install the <a href="https://crates.io/crates/racer">Racer</a> package:
<br>
```cargo install racer```
<br>
5. Install the <a href="https://crates.io/crates/rustfmt">Rustfmt</a> package:
<br>
```cargo install rustfmt```
6. Install the <a href="https://crates.io/crates/cargo-check">Cargo-check</a> package:
<br>
```cargo install cargo-check```

<hr>

## SuperCollider
You will need to compile <a href="https://github.com/supercollider/supercollider">SuperCollider</a>.

<hr>

## Tidal
1. Once <a href="http://tidalcycles.org/">Tidal</a> is installed, create a tidal directory in your home folder:
<br>
```mkdir ~/tidal```
<br>
2. Download <a href="https://github.com/tidalcycles/Tidal/blob/master/tidal.el">tidal.el</a> and place it inside the tidal folder (```~/tidal/tidal.el```)

<hr>

## Javascript / JSX
1. Install <a href="https://nodejs.org/">Node.js</a> (I recommend using <a href="https://github.com/creationix/nvm">NVM</a>)
<br>
2. Install the required <a href="http://eslint.org/">ESLint</a> packages globally:
<br>
```sudo npm install -g eslint babel-eslint eslint-plugin-react eslint-plugin-html```
<br>
3. Create an .eslintrc file in your home folder:
<br>
```touch ~/.eslintrc```
<br>
4. Configure the .eslintrc file:
``` json
{
  "parser": "babel-eslint",
  "plugins": [ "react", "html" ],
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

<hr>

## Indentation
By default there is a 2 space indentation preference for Javascript, JSX, CSS, HTML, and text files.
<br>
If you prefer 4 space indentation, edit <a href="https://github.com/paullucas/emacs/blob/master/init.el#L175">line 175</a> & <a href="https://github.com/paullucas/emacs/blob/master/init.el#L181">line 181</a>.

<hr>

# Remove Features (optional)
### Disable Rust
Remove the following block from init.el (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L260#L271">line 260 - 271</a>)
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
(add-hook 'rust-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c <tab>") #'rust-format-buffer)))
```

<hr>

### Disable SuperCollider
Remove the following block from init.el (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L273#L275">line 273 - 275</a>)
``` emacs-lisp
;; SuperCollider
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/SuperCollider")
(require 'sclang)
```

<hr>

### Disable Tidal
Remove the following block from init.el (<a href="https://github.com/paullucas/emacs/blob/master/init.el#L277#L279">line 277 - 279</a>)
``` emacs-lisp
;; Tidal
(add-to-list 'load-path "~/tidal")
(require 'tidal)
```

<hr>

# Update
1. Change current directory:
<br>
```cd ~/emacs```
<br>
2. Update to latest version:
<br>
```git pull```

# Uninstall
Remove the following files and directories:
<br>
```rm -rf ~/.emacs ~/.emacs.d ~/emacs```

<hr>

# Thanks
- Javascript configuration: <a href="http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html">codewinds.com</a>
<br>
- Rust configuration: <a href="http://emacsist.com/10425">emacsist.com</a>
<br>
- Clojure configuration: <a href="https://github.com/ftravers">ftravers</a>
