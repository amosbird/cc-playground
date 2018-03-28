((c++-mode
  (mode . cc-playground)
  (cc-exec . "g++")
  (cc-flags . " \
-std=c++17 \
-fopenmp \
")
  (cc-links . " \
-lpthread \
-ldl \
")))
