(require 'derived)

(define-derived-mode edn-mode clojure-mode "edn"
  "Major mode for editing edn data
Special commands:
\\{edn-mode-map}"
  )

(provide 'edn)

