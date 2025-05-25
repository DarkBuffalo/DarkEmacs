(require 'core-straight)

(package! scad-mode)

(package! scad-dbus
  :after scad-mode
  :straight (:host github :repo "Lenbok/scad-dbus" :branch "master")
  :bind (:map scad-mode-map ("C-c o" . 'hydra-scad-dbus/body)))

(package! scad-preview)

(package! scad-el
  :after scad-mode
  :straight (:host gitlab :repo "korhadris/scad-el"))
