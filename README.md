

# nyxt.el

`nyxt.el` consists of custom logic to interact with [Nyxt](https://nyxt.atlas.engineer/) from Emacs. In contrast to [emacs-with-nyxt](https://github.com/ag91/emacs-with-nyxt), it doesn't bundle any Nyxt configuration for you by default. Instead, it aims at providing a rich feature set of custom interactive functions via the `nyxt-run` entry function. Contributions are welcome!  

This package requires the [SLY](https://github.com/joaotavora/sly) Emacs package to interact with the underlying Nyxt Lisp image. Optionally, if you're an [EXWM](https://github.com/ch11ng/exwm) user, you'll have the ability to make commands focus on the corresponding Nyxt window for a more seamless experience with the browser.  

To install the package, simply point to the package in your `load-path`.  

    (add-to-list 'load-path "/path/to/nyxt.el")

If you'd like to contribute to the package and get the project set up quickly, I suggest you install the [GNU Guix](https://guix.gnu.org/) package manager and start developing on the local checkout by invoking the following commands:  

    cd /path/to/nyxt.el
    guix shell --pure

An example configuration might look like this:  

    (define-key global-map (kbd "C-c y") 'nyxt-map)
    (with-eval-after-load 'nyxt
      (setq nyxt-path (executable-find "guix"))
      (setq nyxt-startup-flags '("shell" "-D" "-f" "path/to/nyxt/build-scripts/nyxt.scm" "--" "path/to/nyxt/nyxt" "-e" "(start-slynk)")))

Above, we set the default bindings included in the `nyxt-map` to the `C-c y` global binding. By default, this package will spawn a Nyxt process with the `-e (start-slynk)` flags so that it can launch and connect to a Slynk process. However, you're free to modify the executable path and the flags supplied to it, such as if you're developing on Nyxt using the [GNU Guix](https://guix.gnu.org) package manager.  

Feel free to open an issue or send a pull request if you encounter a bug or wish to add a new feature.  

