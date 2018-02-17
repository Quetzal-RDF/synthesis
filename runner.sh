for FILE in expressionsNew.txt; do /Applications/Racket\ v6.8/bin/racket <<AAA
(require "benchmark.rkt")                                                       (define MAX-BYTES (* 1 1024 1024 1024))                                         (custodian-limit-memory (current-custodian) MAX-BYTES)                          (benchmark-synthesis "$FILE")
AAA
done
