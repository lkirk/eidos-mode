;;; eidos-mode.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for editing eidos, a scripting language for the SLiM simulator.
;; Thanks to Vince Buffalo, this began as an adaptation from his vim mode.
;; https://github.com/vsbuffalo/eidos.vim/blob/master/syntax/eidos.vim

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
    ;; (modify-syntax-entry ?* "." table)
    ;; (modify-syntax-entry ?/ "." table)
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

;; obtained with (note fish shell)
;; begin
;;     cd ~/repo/SLiM
;;     git grep -Eo 'new EidosFunctionSignature\("[a-zA-Z]+",' \
;;         | sed -re's/.*(".+").*/\1/' \
;;         | sort -u
;; end
;; additional SLiM builtins obtained with
;; for l in
;; (git grep 'EidosRegisteredString("' | sed -re's/.*&(g(Eidos|)Str_.+) = EidosRegisteredString.*/\1/')
;;     grep -q EidosFunctionSignature (git grep "$l" | psub) && echo "$l"
;; end | sed -re's/^g(Eidos|)Str_(.+)$/\2/'
;; With a little bit of manual pruning...
;; Still missing some, see SLiM Manual pg. 824

;; think about highlighting types with $ or <>$
(defvar eidos-types
  '("string"
    "integer"
    "float"
    "logical"
    "object"
    "string$"
    "integer$"
    "float$"
    "logical$"
    "object$"))

(defvar eidos-builtins
  '("abs"
    "acos"
    "all"
    "any"
    "apply"
    "array"
    "asFloat"
    "asin"
    "asInteger"
    "asLogical"
    "assert"
    "asString"
    "atan"
    "atan2"
    "beep"
    "c"
    "calcFST"
    "calcHeterozygosity"
    "calcInbreedingLoad"
    "calcPairHeterozygosity"
    "calcPi"
    "calcTajimasD"
    "calcVA"
    "calcWattersonsTheta"
    "cat"
    "catn"
    "cbind"
    "ceil"
    "citation"
    "clock"
    "cmColors"
    "color2rgb"
    "codonsToAminoAcids"
    "codonsToNucleotides"
    "color"
    "colors"
    "cor"
    "cos"
    "cov"
    "createDirectory"
    "cumProduct"
    "cumSum"
    "DataFrame"
    "date"
    "dbeta"
    "debugIndent"
    "defineConstant"
    "defineGlobal"
    "deleteFile"
    "dexp"
    "dgamma"
    "diag"
    "Dictionary"
    "dim"
    "dmvnorm"
    "dnorm"
    "do"
    "doCall"
    "drop"
    "early"
    "elementType"
    "end"
    "executeLambda"
    "exists"
    "exp"
    "fileExists"
    "filePath"
    "filesAtPath"
    "fitness"
    "findInterval"
    "first"
    "fitnessEffect"
    "floor"
    "flushFile"
    "format"
    "functionSignature"
    "functionSource"
    "getSeed"
    "getwd"
    "grep"
    "heatColors"
    "hsv2rgb"
    "identical"
    "ifelse"
    "Image"
    "in"
    "initialize"
    "initializeAncestralNucleotides"
    "initializeGeneConversion"
    "initializeGenomicElement"
    "initializeGenomicElementType"
    "initializeHotspotMap"
    "initializeInteractionType"
    "initializeMutationRate"
    "initializeMutationType"
    "initializeMutationTypeNuc"
    "initializeRecombinationRate"
    "initializeSex"
    "initializeSLiMModelType"
    "initializeSLiMOptions"
    "initializeSpecies"
    "initializeTreeSeq"
    "integerDiv"
    "integerMod"
    "interaction"
    "isFinite"
    "isFloat"
    "isInfinite"
    "isInteger"
    "isLogical"
    "isNAN"
    "isNULL"
    "isObject"
    "isString"
    "late"
    "length"
    "license"
    "log"
    "log10"
    "log2"
    "lowerTri"
    "ls"
    "match"
    "mateChoice"
    "matrix"
    "matrixMult"
    "max"
    "mean"
    "methodSignature"
    "min"
    "mmJukesCantor"
    "mmKimura"
    "modifyChild"
    "mutation"
    "mutationEffect"
    "nchar"
    "ncol"
    "nrow"
    "nucleotideCounts"
    "nucleotideFrequencies"
    "nucleotidesToCodons"
    "order"
    "parallelGetMaxThreads"
    "parallelGetNumThreads"
    "parallelGetTaskThreadCounts"
    "parallelSetNumThreads"
    "parallelSetTaskThreadCounts"
    "paste"
    "paste0"
    "pmax"
    "pmin"
    "pnorm"
    "print"
    "product"
    "propertySignature"
    "qnorm"
    "quantile"
    "rainbow"
    "randomNucleotides"
    "range"
    "rank"
    "rbeta"
    "rbind"
    "rbinom"
    "rcauchy"
    "rdunif"
    "readCSV"
    "readFile"
    "recombination"
    "rep"
    "repEach"
    "reproduction"
    "rev"
    "rexp"
    "rf"
    "rgamma"
    "rgb2color"
    "rbg2hsv"
    "rgeom"
    "rlnorm"
    "rm"
    "rmvnorm"
    "rnbinom"
    "rnorm"
    "round"
    "rpois"
    "runif"
    "rweibull"
    "sample"
    "sapply"
    "sd"
    "seq"
    "seqAlong"
    "seqLen"
    "setDifference"
    "setIntersection"
    "setSeed"
    "setSymmetricDifference"
    "setUnion"
    "setwd"
    "sin"
    "size"
    "sort"
    "sortBy"
    "source"
    "SpatialMap"
    "sqrt"
    "start"
    "stop"
    "str"
    "strcontains"
    "strfind"
    "strprefix"
    "strsplit"
    "strsuffix"
    "substr"
    "sum"
    "sumExact"
    "summarizeIndividuals"
    "suppressWarnings"
    "survival"
    "sysinfo"
    "system"
    "t"
    "tabulate"
    "tan"
    "tempdir"
    "terrainColors"
    "time"
    "treeSeqMetadata"
    "trunc"
    "ttest"
    "type"
    "unique"
    "upperTri"
    "usage"
    "var"
    "version"
    "weights"
    "which"
    "whichMax"
    "whichMin"
    "writeFile"
    "writeTempFile"))

(defvar eidos-keywords
  '("if" "else" "while" "do" "for" "in" "next" "break" "return" "function"))

(defvar eidos-number-regexp
  "\\_<[0-9]+\\_>\\|\\_<[0-9]+\\.[0-9]+\\_>\\|\\_<[0-9]*\\.?[0-9]+[Ee-]?[0-9]+\\_>")

(defvar eidos-boolean-regexp "\\_<\\(T\\|F\\|NULL\\)\\_>")

(setq eidos-font-lock-keywords
      `(("//.*$" . font-lock-comment-face)
        ("/\\*\\(\\(?:[^*]\\|\\*[^/]\\)*\\*/\\)" . font-lock-comment-face)
        ("\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"" . font-lock-string-face)
        (,(regexp-opt eidos-keywords 'symbols) . font-lock-keyword-face)
        (,(regexp-opt eidos-builtins 'symbols) . font-lock-builtin-face)
        (,(regexp-opt eidos-types 'symbols) . font-lock-type-face)
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

(defun eidos-fundepth-correction ()
  "Find lines with multiple open parens that haven't been closed."
  (save-excursion
    (let ((pos (point))
          (last-line (line-number-at-pos))
          (corr 0))
      (while-let ((pos-last-open (nth 1 (syntax-ppss pos))))
        (when (= (char-after pos-last-open) ?\()
          (let ((tmp (line-number-at-pos pos-last-open)))
            (when (= tmp last-line)
              (setq corr (1+ corr)))
            (setq last-line tmp)))
        (setq pos pos-last-open))
      corr)))

(defun eidos-hanging-indent ()
  "Find open parens with non comment characters after, return col of args."
  (save-excursion
    (when-let ((curr-line (line-number-at-pos))
               (last-paren-open (nth 1 (syntax-ppss))))
      (goto-char last-paren-open)
      (unless (eobp)
        (forward-char 1)
        (when (not (= curr-line (line-number-at-pos)))
          (skip-chars-forward " \t")
          (when (and (not (= (point) (line-end-position)))
                     (not (= (char-after (point)) ?/)))
            (current-column)))))))

(defun eidos-indent-line ()
  "Indent the current line."
  ;; no need to save-excursion, indent-line-to moves cursor
  (beginning-of-line)
  (let ((depth (syntax-ppss-depth (syntax-ppss)))
        (corr (eidos-fundepth-correction)))
    (cond
     ((or (eidos-closing) (eidos-else-openclose))
      (indent-line-to (* (1- depth) tab-width)))
     (t
      (if-let ((hanging (eidos-hanging-indent)))
        ;; if we ever want to mix in tabs, this could be bad?
        (indent-line-to hanging)
        (indent-line-to (* (- depth corr) tab-width)))))))

(provide 'eidos-mode)
;;; eidos-mode.el ends here
