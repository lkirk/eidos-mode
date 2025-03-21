;;; eidos-mode.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Major mode for editing eidos, a scripting language for the SLiM simulator.
;; Thanks to Vince Buffalo, this began as an adaptation from his vim mode.
;; https://github.com/vsbuffalo/eidos.vim/blob/master/syntax/eidos.vim

;;; Code:
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
    ;; types ending in dollar are singletons (vec w/ one val)
    (modify-syntax-entry ?$ "." table)

    ;; open/close Delimiters
    (modify-syntax-entry ?\( "()" table) ;; paren: func call, grouping
    (modify-syntax-entry ?\) ")(" table)

    (modify-syntax-entry ?\{ "(}" table) ;; braces: block delimiters
    (modify-syntax-entry ?\} "){" table)

    (modify-syntax-entry ?\[ "(]" table) ;; brackets: indexing, vectors
    (modify-syntax-entry ?\] ")[" table)

    table))

;; There isn't an easily accessible centralized list of functions/objects, so I
;; had to do a bit of scraping

;; obtained builtin functions with (note fish shell):
;; begin
;;     cd ~/repo/SLiM
;;     git grep -Eo 'new EidosFunctionSignature\("[a-zA-Z]+",' \
;;         | sed -re's/.*(".+").*/\1/'
;;     git grep '\*\*\*\*\*.*(' \
;;         | sed -re's/.*\)([A-Za-z_]*)\(.*/"\1"/g' | grep -v '^"_' \
;;         | grep -v '<<'
;;     git grep -Eoh 'Eidos_ExecuteFunction_[a-zA-Z0-9_]+' \
;;         | sed -re's/Eidos_ExecuteFunction_([a-zA-Z0-9_]+)/"\1"/' \
;;         | grep -v '^"_'
;;     cd -
;; end | sort -u

;; Builtin objects
;; git grep -hEo 'g(Eidos|SLiM)[0-9_A-Za-z]+_Class' \
;;     | sed -re's/(Retained|Unretained)//g' \
;;     | grep -v -e Test -e Object \
;;     | sed -re's/g(SLiM_|Eidos)([a-zA-Z0-9]+)_Class/\2/g' \
;;     | sort -u

;; think about highlighting types with $ or <>$
(defvar eidos-types
  '(
    ;; atomic types
    "void"
    "string"
    "integer"
    "float"
    "logical"
    "object"
    "numeric"
    ;; Classes can be types in function signatures
    "Chromosome"
    "Community"
    "DataFrame"
    "Dictionary"
    "Genome"
    "GenomicElement"
    "GenomicElementType"
    "Image"
    "Individual"
    "InteractionType"
    "LogFile"
    "Mutation"
    "MutationType"
    "Plot"
    "SLiMEidosBlock"
    "SLiMgui"
    "SpatialMap"
    "Species"
    "Subpopulation"
    "Substitution"))

(defvar eidos-builtins
  '("DataFrame"
    "Dictionary"
    "Image"
    "SpatialMap"
    "source" ;; not found by my search
    ;; all discovered by code scraping
    "abline"
    "abs"
    "acos"
    "add"
    "addCloned"
    "addCrossed"
    "addCustomColumn"
    "addCycle"
    "addCycleStage"
    "addEmpty"
    "addKeysAndValuesFrom"
    "addLegend"
    "addMeanSDColumns"
    "addMutations"
    "addNewDrawnMutation"
    "addNewMutation"
    "addPopulationSexRatio"
    "addPopulationSize"
    "addRecombinant"
    "addSelfed"
    "addSpatialMap"
    "addSubpop"
    "addSubpopSplit"
    "addSubpopulationSexRatio"
    "addSubpopulationSize"
    "addSuppliedColumn"
    "addTick"
    "all"
    "ancestralNucleotides"
    "any"
    "appendKeysAndValuesFrom"
    "apply"
    "array"
    "asFloat"
    "asin"
    "asInteger"
    "asLogical"
    "asMatrix"
    "assert"
    "asString"
    "atan"
    "atan2"
    "axis"
    "beep"
    "blend"
    "c"
    "cachedFitness"
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
    "changeColors"
    "changeValues"
    "citation"
    "clearKeysAndValues"
    "clippedIntegral"
    "clock"
    "cmColors"
    "codonsToAminoAcids"
    "codonsToNucleotides"
    "color2rgb"
    "colors"
    "compactIndices"
    "configureDisplay"
    "containsMarkerMutation"
    "containsMutations"
    "cor"
    "cos"
    "countOfMutationsOfType"
    "cov"
    "createDirectory"
    "createLogFile"
    "createPlot"
    "cumProduct"
    "cumSum"
    "date"
    "dbeta"
    "debugIndent"
    "defineConstant"
    "defineGlobal"
    "defineSpatialMap"
    "deleteFile"
    "deregisterScriptBlock"
    "deviatePositions"
    "dexp"
    "dgamma"
    "diag"
    "dim"
    "distance"
    "distanceFromPoint"
    "divide"
    "dmvnorm"
    "dnorm"
    "doCall"
    "drawBreakpoints"
    "drawByStrength"
    "drawSelectionCoefficient"
    "drop"
    "early"
    "elementType"
    "estimatedLastTick"
    "evaluate"
    "executeLambda"
    "exists"
    "exp"
    "fileExists"
    "filesAtPath"
    "findInterval"
    "first"
    "fitnessEffect"
    "float"
    "floor"
    "flush"
    "flushFile"
    "format"
    "functionSignature"
    "functionSource"
    "genomicElementForPosition"
    "genomicElementTypesWithIDs"
    "getRowValues"
    "getSeed"
    "getValue"
    "getwd"
    "grep"
    "gridValues"
    "hasGenomicElementForPosition"
    "heatColors"
    "hsv2rgb"
    "identical"
    "identicalContents"
    "ifelse"
    "individualsWithPedigreeIDs"
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
    "integer"
    "integerDiv"
    "integerMod"
    "interactingNeighborCount"
    "interaction"
    "interactionDistance"
    "interactionTypesWithIDs"
    "interpolate"
    "isFinite"
    "isFloat"
    "isInfinite"
    "isInteger"
    "isLogical"
    "isNAN"
    "isNULL"
    "isObject"
    "isString"
    "killIndividuals"
    "late"
    "legendLineEntry"
    "legendPointEntry"
    "legendSwatchEntry"
    "length"
    "license"
    "lines"
    "localPopulationDensity"
    "log"
    "log10"
    "log2"
    "logFileData"
    "logical"
    "logRow"
    "lowerTri"
    "ls"
    "mapColor"
    "mapValue"
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
    "multiply"
    "mutation"
    "mutationCounts"
    "mutationCountsInGenomes"
    "mutationEffect"
    "mutationFrequencies"
    "mutationFrequenciesInGenomes"
    "mutationsOfType"
    "mutationTypesWithIDs"
    "nchar"
    "ncol"
    "nearestInteractingNeighbors"
    "nearestNeighbors"
    "nearestNeighborsOfPoint"
    "neighborCount"
    "neighborCountOfPoint"
    "nrow"
    "nucleotideCounts"
    "nucleotideFrequencies"
    "nucleotides"
    "nucleotidesToCodons"
    "object"
    "openDocument"
    "order"
    "output"
    "outputFixedMutations"
    "outputFull"
    "outputMS"
    "outputMSSample"
    "outputMutations"
    "outputSample"
    "outputUsage"
    "outputVCF"
    "outputVCFSample"
    "parallelGetMaxThreads"
    "parallelGetNumThreads"
    "parallelGetTaskThreadCounts"
    "parallelSetNumThreads"
    "parallelSetTaskThreadCounts"
    "paste"
    "paste0"
    "pauseExecution"
    "plotWithTitle"
    "pmax"
    "pmin"
    "pnorm"
    "pointDeviated"
    "pointInBounds"
    "pointPeriodic"
    "pointReflected"
    "points"
    "pointStopped"
    "pointUniform"
    "positionsOfMutationsOfType"
    "power"
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
    "readFromMS"
    "readFromPopulationFile"
    "readFromVCF"
    "recalculateFitness"
    "recombination"
    "registerEarlyEvent"
    "registerFirstEvent"
    "registerFitnessEffectCallback"
    "registerInteractionCallback"
    "registerLateEvent"
    "registerMateChoiceCallback"
    "registerModifyChildCallback"
    "registerMutationCallback"
    "registerMutationEffectCallback"
    "registerRecombinationCallback"
    "registerReproductionCallback"
    "registerSurvivalCallback"
    "relatedness"
    "removeMutations"
    "removeSpatialMap"
    "removeSubpopulation"
    "rep"
    "repEach"
    "reproduction"
    "rescale"
    "rescheduleScriptBlock"
    "rev"
    "rexp"
    "rf"
    "rgamma"
    "rgb2color"
    "rgb2hsv"
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
    "sampleImprovedNearbyPoint"
    "sampleIndividuals"
    "sampleNearbyPoint"
    "sapply"
    "scriptBlocksWithIDs"
    "sd"
    "seq"
    "seqAlong"
    "seqLen"
    "serialize"
    "setAncestralNucleotides"
    "setCloningRate"
    "setConstraints"
    "setDifference"
    "setDistribution"
    "setFilePath"
    "setGeneConversion"
    "setGenomicElementType"
    "setHotspotMap"
    "setInteractionFunction"
    "setIntersection"
    "setLogInterval"
    "setMigrationRates"
    "setMutationFractions"
    "setMutationMatrix"
    "setMutationRate"
    "setMutationType"
    "setRecombinationRate"
    "setSeed"
    "setSelectionCoeff"
    "setSelfingRate"
    "setSexRatio"
    "setSpatialBounds"
    "setSpatialPosition"
    "setSubpopulationSize"
    "setSuppliedValue"
    "setSymmetricDifference"
    "setUnion"
    "setValue"
    "setwd"
    "sharedParentCount"
    "simulationFinished"
    "sin"
    "size"
    "size_length"
    "skipTick"
    "smooth"
    "sort"
    "sortBy"
    "spatialMapColor"
    "spatialMapValue"
    "speciesWithIDs"
    "sqrt"
    "stop"
    "str"
    "strcontains"
    "strength"
    "strfind"
    "string"
    "stringRepresentation"
    "strprefix"
    "strsplit"
    "strsuffix"
    "subpopulationsWithIDs"
    "subpopulationsWithNames"
    "subset"
    "subsetColumns"
    "subsetIndividuals"
    "subsetMutations"
    "subsetRows"
    "substr"
    "subtract"
    "sum"
    "sumExact"
    "summarizeIndividuals"
    "sumOfMutationsOfType"
    "suppressWarnings"
    "survival"
    "sysinfo"
    "system"
    "t"
    "tabulate"
    "takeMigrants"
    "tan"
    "tempdir"
    "terrainColors"
    "testConstraints"
    "text"
    "time"
    "totalOfNeighborStrengths"
    "treeSeqCoalesced"
    "treeSeqMetadata"
    "treeSeqOutput"
    "treeSeqRememberIndividuals"
    "treeSeqSimplify"
    "trunc"
    "ttest"
    "type"
    "unevaluate"
    "unique"
    "uniqueMutationsOfType"
    "upperTri"
    "usage"
    "var"
    "version"
    "which"
    "whichMax"
    "whichMin"
    "willAutolog"
    "write"
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
        ("^[ \t]*function[ \t]*(<?"
         ,(regexp-opt eidos-types 'symbols)
         nil
         nil
         (0 font-lock-type-face))
        (,(regexp-opt eidos-builtins 'symbols) . font-lock-builtin-face)
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
