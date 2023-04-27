

# nyxt.el

`nyxt.el` consists of custom logic to interact with [Nyxt](https://nyxt.atlas.engineer/) from Emacs. In contrast to [emacs-with-nyxt](https://github.com/ag91/emacs-with-nyxt), it doesn't bundle any Nyxt configuration and only includes a couple of interactive helpers for some common Nyxt operations. Users are encouraged to use the entry point function `nyxt-run` to build their own functionality.  

This package requires the [SLY](https://github.com/joaotavora/sly) Emacs package to interact with the underlying Nyxt Lisp image. If you're an [EXWM](https://github.com/ch11ng/exwm) user, you'll also have the ability to make commands focus on the corresponding Nyxt window for a more seamless experience.  

To install the package, simply point to the package in your `load-path`.  

    (add-to-list 'load-path "/path/to/nyxt.el")

The easiest way to contribute to this project is to use the [GNU Guix](https://guix.gnu.org/) package manager and start developing on the local checkout by invoking the following commands:  

    cd /path/to/nyxt.el
    guix shell --pure

An example configuration might look like this:  

    (define-key global-map (kbd "C-c y") 'nyxt-map)
    (with-eval-after-load 'nyxt
      (setopt nyxt-path (executable-find "guix"))
      (setopt nyxt-startup-flags '("shell" "-D" "-f" "path/to/nyxt/build-scripts/nyxt.scm" "--" "path/to/nyxt/nyxt" "-e" "(start-slynk)")))

Above, we first set the default bindings included in the `nyxt-map` to the `C-c y` global binding. By default, this package will spawn a Nyxt process with the `-e (start-slynk)` flags so that it can launch and connect to a Slynk process. However, as shown above, you're free to modify the executable path and the flags supplied to it, such as if you're developing on Nyxt using the [GNU Guix](https://guix.gnu.org) package manager.  

You can send feedback, patches, or bug reports to [public@mianmoreno.com](mailto:public@mianmoreno.com).  

