;;; eidos-mode.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for editing eidos, a scripting language for the SLiM simulator.
;; Thanks to Vince Buffalo, this began as an adaptation from his vim mode.
;; https://github.com/vsbuffalo/eidos.vim/blob/master/syntax/eidos.vim

;; TODO: indentation code...
;; https://github.com/dominikh/go-mode.el/blob/master/go-mode.el
;; https://www.omarpolo.com/post/writing-a-major-mode.html
;; https://www.emacswiki.org/emacs/ModeTutorial
;; https://stackoverflow.com/questions/22989800/creating-emacs-mode-defining-indentation
;; https://www.emacswiki.org/emacs/IndentationBasics


;;; Code:
(require 'font-lock)

(defvar eidos-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; c-style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; string delimiter
    (modify-syntax-entry ?\" "\"" table)

    ;; operators (punctuation)
    (modify-syntax-entry ?. "." table) ;; member access
    (modify-syntax-entry ?, "." table) ;; separator
    (modify-syntax-entry ?= "." table) ;; assignment
    (modify-syntax-entry ?+ "." table) ;; unary +
    (modify-syntax-entry ?- "." table) ;; unary -
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?/ "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?^ "." table) ;; exponentiation
    (modify-syntax-entry ?: "." table) ;; sequence construction
    (modify-syntax-entry ?< "." table) ;; comparison
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table) ;; logical and
    (modify-syntax-entry ?| "." table) ;; logical or
    (modify-syntax-entry ?! "." table) ;; logical not
    (modify-syntax-entry ?? "." table) ;; ternary op
    (modify-syntax-entry ?\; "." table) ;; line ending

    ;; open/close Delimiters
    (modify-syntax-entry ?\( "()" table) ;; paren: func call, grouping
    (modify-syntax-entry ?\) ")(" table)

    (modify-syntax-entry ?\{ "(}" table) ;; braces: block delimiters
    (modify-syntax-entry ?\} "){" table)

    (modify-syntax-entry ?\[ "(]" table) ;; brackets: indexing, vectors
    (modify-syntax-entry ?\] ")[" table)

    table))

(defvar eidos-builtins
  '("abs"
    "acos"
    "asin"
    "atan"
    "atan2"
    "ceil"
    "cos"
    "cumProduct"
    "cumSum"
    "exp"
    "floor"
    "integerDev"
    "integerMod"
    "isFinite"
    "isInfinite"
    "isNAN"
    "log"
    "log2"
    "log10"
    "product"
    "round"
    "setDifference"
    "setIntersection"
    "setSymmetricDifference"
    "setUnion"
    "sin"
    "sqrt"
    "sum"
    "sumExacttan"
    "trunc"
    "cor"
    "cov"
    "max"
    "mean"
    "min"
    "pmax"
    "pmin"
    "range"
    "sd"
    "ttest"
    "var"
    "dmvnorm"
    "dnorm"
    "rbinom"
    "rcauchy"
    "rdunif"
    "rexp"
    "rgamma"
    "rgeom"
    "rlnorm"
    "rmvnorm"
    "rnorm"
    "rpois"
    "runif"
    "rweibull"
    "c"
    "float"
    "integer"
    "logical"
    "object"
    "rep"
    "repEach"
    "sample"
    "seq"
    "seqAlong"
    "seqLen"
    "string"
    "all"
    "any"
    "cat"
    "catn"
    "format"
    "identical"
    "ifelse"
    "match"
    "nchar"
    "order"
    "paste"
    "paste0"
    "print"
    "rev"
    "size"
    "sort"
    "sortBy"
    "str"
    "strsplit"
    "substr"
    "unique"
    "which"
    "whichMax"
    "whichMin"
    "asFloat"
    "asInteger"
    "asLogical"
    "asString"
    "elementType"
    "isFloat"
    "isInteger"
    "isLogical"
    "isNULL"
    "isObject"
    "isString"
    "type"
    "apply"
    "array"
    "cbind"
    "dim"
    "drop"
    "matrix"
    "matrixMult"
    "ncol"
    "nrow"
    "rbind"
    "t"
    "createDirectory"
    "deleteFile"
    "filesAtPath"
    "getwd"
    "readFile"
    "setwd"
    "writeFile"
    "writeTempFile"
    "cmColors"
    "color2rgb"
    "heatColors"
    "hsv2rgb"
    "rainbow"
    "rgb2color"
    "rgb2hsv"
    "terrainColors"
    "beep"
    "citation"
    "clock"
    "date"
    "defineConstant"
    "doCall"
    "executeLambda"
    "exists"
    "functionSignature"
    "getSeed"
    "license"
    "ls"
    "rm"
    "sapply"
    "setSeed"
    "source"
    "stop"
    "suppressWarnings"
    "system"
    "time"
    "usage"
    "version"
    "str"
    "length"
    "methodSignature"
    "propertySignature"
    "size"
    "initializeGeneConversion"
    "initializeGenomicElement"
    "initializeGenomicElementType"
    "initializeInteractionType"
    "initializeMutationRate"
    "initializeMutationType"
    "initializeRecombinationRate"
    "initializeSex"
    "initializeSex"
    "initializeSLiMOptions"
    "initializeTreeSeq"
    "initialize"
    "fitness"
    "mateChoice"
    "mateChoice"
    "recombination"
    "interaction"
    "reproduction"))

(defvar eidos-keywords
  '("if" "else" "while" "do" "for" "in" "next" "break" "return" "function"))

;; (defvar eidos-number-regexp
;;   "\\_<[0-9]+\\_>\\|\\_<[0-9]+\\.[0-9]+\\_>\\|\\_<[0-9]*\\.?[0-9]+[Ee-]?[0-9]+\\_>")
(defvar eidos-boolean-regexp "\\_<\\(T\\|F\\|NULL\\)\\_>")

(setq eidos-font-lock-keywords
      `(("//.*$" . font-lock-comment-face)
        ("/\\*\\(\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)" . font-lock-comment-face)
        ("\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" . font-lock-string-face)
        (,(regexp-opt eidos-keywords 'symbols) . font-lock-keyword-face)
        (,(regexp-opt eidos-builtins 'symbols) . font-lock-builtin-face)
        ;; for some reason, only number face needs to be quoted
        (,eidos-number-regexp . 'font-lock-number-face)
        (,eidos-boolean-regexp . font-lock-constant-face)))

(define-derived-mode
 eidos-mode
 prog-mode
 "eidos mode"
 "Major mode for SLiM's Eidos language."
 (setq-local indent-line-function 'eidos-indent-line)
 (setq-local font-lock-defaults '(eidos-font-lock-keywords))
 (setq-local comment-start "// "))


(defun eidos-opening ()
  "Are we pointing at an opening character?  Either a { or (.
Allows for trailing comments/nonprinting"
  (looking-at
   "^.*\\((\\|{\\)[ \t]*\\(\\(//.*\\)\\|\\(/\\*.*\\*/\\)\\)?[ \t]*$"))

(defun eidos-else-openclose ()
  "Are we in an else block that closes and opens?
This would be considered a \"close\" brace. We allow for arbitrary tabs
and block comments flanking the else keyword (perhaps its a bit much)."
  (looking-at
   "[ \t]*}[ \t]*\\(/\\*.*\\*/\\)?[ \t]*else[ \t]*\\(/\\*.*\\*/\\)?[ \t]*{"))

(defun eidos-closing ()
  "Are we pointing at a closing character?  Either a } or ).
Allows for trailing comments/nonprinting"
  (looking-at
   "^[ \t]*\\()\\|}\\|);\\)[ \t]*\\(\\(//.*\\)\\|\\(/\\*.*\\*/\\)\\)?[ \t]*$"))

;; (defun my-in-function-arguments-p ()
;;   "Check if the point is inside function call arguments."
;;   (save-excursion
;;     (let ((pos (point)))
;;       (when (re-search-backward "([^(]*" (line-beginning-position) t)
;;         (looking-at "(")))))

(defun match-ivls (regexp str pos)
  (let (matches)
    (while (string-match regexp str pos)
      (push (match-data) matches)
      (setq pos (match-end 0)))
    (reverse matches)))

(defun invert-selection (pairs len)
  (let ((last '(nil 0))
        (pairs
         (if (= len (cadr (car (last pairs))))
             pairs
           (append pairs `((,len nil))))))
    (seq-map
     (lambda (p)
       (let (tmp)
         (setq tmp last)
         (setq last p)
         `(,(cadr tmp) ,(car p))))
     pairs)))

;; TODO: consider getting rid of substring calls, they make copies. Can avoid
;;       copying buffer strings by
(defun eidos-line-funcall-level (line)
  "Determine the current funcall level for the given LINE."
  (let* (in-comment
         (len
          (cond
           ((string-match "//" line)
            (match-beginning 0))
           ((string-match "/\\*" line)
            (setq in-comment t)
            (match-beginning 0))
           (t
            (length line))))
         (pmatch
          (seq-map
           (lambda (p)
             (- (length (match-ivls "(" (substring line (car p) (cadr p)) 0))
                (length (match-ivls ")" (substring line (car p) (cadr p)) 0))))
           (if-let (m
                    (match-ivls "/\\*.*?\\*/" line 0))
             (invert-selection m len)
             `((0 ,len))))))
    `(,(apply #'+ pmatch) ,in-comment)))
;; TODO: do something with in-comment?

;; (defun my-get-first-arg-indent ()
;;   "Return indentation level based on the first argument of a function call."
;;   (save-excursion
;;     (re-search-backward "(" (line-beginning-position) t)
;;     (forward-char)
;;     (skip-chars-forward " \t")
;;     (current-column)))

;; (defun my-calculate-block-indent ()
;;   "Default block indentation logic."
;;   (save-excursion
;;     (forward-line -1)
;;     (if (looking-at ".*{")
;;         (+ (current-indentation) tab-width)
;;       (current-indentation))))

(defun eidos-indent-line ()
  "Indent the current line.  Perhaps one day we'll have an eidos-indent-region."
  (beginning-of-line)
  (let ((not-indented t)
        (cur-indent 0))
    (save-excursion
      (cond
       ((bobp)
        (indent-line-to 0))
       ((or (eidos-closing) (eidos-else-openclose))
        (forward-line -1)
        (setq cur-indent (max 0 (- (current-indentation) tab-width))))
       (t
        (while not-indented
          (forward-line -1)
          (cond
           ((eidos-closing)
            (setq cur-indent (current-indentation))
            (setq not-indented nil))
           ((eidos-opening)
            (setq cur-indent (+ (current-indentation) tab-width))
            (setq not-indented nil))
           ((bobp)
            (setq not-indented nil)))))))
    (if cur-indent
        (indent-line-to cur-indent)
      (indent-line-to 0))))

(provide 'eidos-mode)
;;; eidos-mode.el ends here
