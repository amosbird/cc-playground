((c++-mode
  (mode . cc-playground)
  (cc-exec . "g++")
  (cc-flags . " \
-std=c++17 \
-fopenmp \
-I/usr/local/include \
")
  (cc-links . " \
-lpthread \
-ldl \
")))
