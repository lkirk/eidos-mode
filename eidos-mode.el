;; Thanks to Vince Buffalo
;; https://github.com/vsbuffalo/eidos.vim/blob/master/syntax/eidos.vim

(defvar eidos-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; c-style comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    table))

(defvar eidos-builtins
  '("abs" "acos" "asin" "atan" "atan2" "ceil" "cos" "cumProduct" "cumSum" "exp"
    "floor" "integerDev" "integerMod" "isFinite" "isInfinite" "isNAN" "log" "log2"
    "log10" "product" "round" "setDifference" "setIntersection"
    "setSymmetricDifference" "setUnion" "sin" "sqrt" "sum" "sumExacttan" "trunc"
    "cor" "cov" "max" "mean" "min" "pmax" "pmin" "range" "sd" "ttest" "var"
    "dmvnorm" "dnorm" "rbinom" "rcauchy" "rdunif" "rexp" "rgamma" "rgeom" "rlnorm"
    "rmvnorm" "rnorm" "rpois" "runif" "rweibull" "c" "float" "integer" "logical"
    "object" "rep" "repEach" "sample" "seq" "seqAlong" "seqLen" "string" "all" "any"
    "cat" "catn" "format" "identical" "ifelse" "match" "nchar" "order" "paste"
    "paste0" "print" "rev" "size" "sort" "sortBy" "str" "strsplit" "substr" "unique"
    "which" "whichMax" "whichMin" "asFloat" "asInteger" "asLogical" "asString"
    "elementType" "isFloat" "isInteger" "isLogical" "isNULL" "isObject" "isString"
    "type" "apply" "array" "cbind" "dim" "drop" "matrix" "matrixMult" "ncol" "nrow"
    "rbind" "t" "createDirectory" "deleteFile" "filesAtPath" "getwd" "readFile"
    "setwd" "writeFile" "writeTempFile" "cmColors" "color2rgb" "heatColors"
    "hsv2rgb" "rainbow" "rgb2color" "rgb2hsv" "terrainColors" "beep" "citation"
    "clock" "date" "defineConstant" "doCall" "executeLambda" "exists"
    "functionSignature" "getSeed" "license" "ls" "rm" "sapply" "setSeed" "source"
    "stop" "suppressWarnings" "system" "time" "usage" "version" "str" "length"
    "methodSignature" "propertySignature" "size" "initializeGeneConversion"
    "initializeGenomicElement" "initializeGenomicElementType"
    "initializeInteractionType" "initializeMutationRate" "initializeMutationType"
    "initializeRecombinationRate" "initializeSex" "initializeSex"
    "initializeSLiMOptions" "initializeTreeSeq" "initialize" "fitness" "mateChoice"
    "mateChoice" "recombination" "interaction" "reproduction"))

(defvar eidos-keywords
  '("if" "else" "while" "do" "for" "in" "next" "break" "return" "function"))

(defvar eidos-number-regexp "\\_<[0-9]+\\_>\\|\\_<[0-9]+\\.[0-9]+\\_>\\|\\_<[0-9]*\\.?[0-9]+[Ee-]?[0-9]+\\_>")
(defvar eidos-boolean-regexp "\\_<\\(T\\|F\\|NULL\\)\\_>")

(setq eidos-font-lock-keywords
      `(
	("//.*$" . font-lock-comment-face)
	("/\\*\\(\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)" . font-lock-comment-face)
	("\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" . font-lock-string-face)
	(,(regexp-opt eidos-keywords 'symbols) . font-lock-keyword-face)
	(,(regexp-opt eidos-builtins 'symbols) . font-lock-builtin-face)
	(,eidos-number-regexp . font-lock-number-face)
	(,eidos-boolean-regexp . font-lock-constant-face)))

(define-derived-mode eidos-mode prog-mode "eidos mode"
  "Major mode for SLiM's Eidos language."
  (setq-local font-lock-defaults '(eidos-font-lock-keywords))
  (setq-local comment-start "// ")
  )

(provide 'eidos-mode)

;; eidos-mode.el

;; (defvar eidos-keywords-regexp (regexp-opt eidos-keywords 'words))
;; (defvar eidos-functions-regexp (regexp-opt eidos-functions 'words))

;; (setq eidos-font-lock-keywords
;;       `(
;;         (,eidos-keywords-regexp . font-lock-keyword-face)
;;         (,eidos-functions-regexp . font-lock-function-name-face)
;;         (,eidos-number-regexp . font-lock-constant-face)
;;         (,eidos-boolean-regexp . font-lock-constant-face)
;;         ("//.*$" . font-lock-comment-face)
;;         ("/\\*\\(\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)" . font-lock-comment-face)
;;         ("\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" . font-lock-string-face)))

;; (define-derived-mode eidos-mode prog-mode "eidos mode"
;;   "Major mode for editing SLiM's eidos language."
;;   (setq font-lock-defaults '(eidos-font-lock-keywords)))

;; (provide 'eidos-mode)
