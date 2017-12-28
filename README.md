# c++ playground in emacs

based on [go-playground](https://github.com/grafov/go-playground)

# sample configuration

``` emacs-lisp
(defvar +amos/default-include-headers
  '("algorithm" "any" "array" "atomic" "bitset" "cassert" "ccomplex" "cctype" "cerrno" "cfenv" "cfloat" "chrono" "cinttypes" "ciso646" "climits" "clocale" "cmath" "codecvt" "complex" "complex.h" "condition_variable" "csetjmp" "csignal" "cstdalign" "cstdarg" "cstdbool" "cstddef" "cstdint" "cstdio" "cstdlib" "cstring" "ctgmath" "ctime" "cuchar" "cwchar" "cwctype" "cxxabi.h" "deque" "exception" "fenv.h" "forward_list" "fstream" "functional" "future" "initializer_list" "iomanip" "ios" "iosfwd" "iostream" "istream" "iterator" "limits" "list" "locale" "map" "math.h" "memory" "mutex" "new" "numeric" "optional" "ostream" "queue" "random" "ratio" "regex" "scoped_allocator" "set" "shared_mutex" "sstream" "stack" "stdexcept" "stdlib.h" "streambuf" "string" "string_view" "system_error" "tgmath.h" "thread" "tuple" "type_traits" "typeindex" "typeinfo" "unordered_map" "unordered_set" "utility" "valarray" "variant" "vector" "auto_ptr.h" "backward_warning.h" "binders.h" "hash_fun.h" "hash_map" "hash_set" "hashtable.h" "strstream" "adxintrin.h" "altivec.h" "ammintrin.h" "arm_acle.h" "arm_neon.h" "armintr.h" "avx2intrin.h" "avx512bwintrin.h" "avx512cdintrin.h" "avx512dqintrin.h" "avx512erintrin.h" "avx512fintrin.h" "avx512ifmaintrin.h" "avx512ifmavlintrin.h" "avx512pfintrin.h" "avx512vbmiintrin.h" "avx512vbmivlintrin.h" "avx512vlbwintrin.h" "avx512vlcdintrin.h" "avx512vldqintrin.h" "avx512vlintrin.h" "avx512vpopcntdqintrin.h" "avxintrin.h" "bmi2intrin.h" "bmiintrin.h" "clflushoptintrin.h" "clzerointrin.h" "cpuid.h" "cuda_wrappers" "emmintrin.h" "f16cintrin.h" "float.h" "fma4intrin.h" "fmaintrin.h" "fxsrintrin.h" "htmintrin.h" "htmxlintrin.h" "ia32intrin.h" "immintrin.h" "intrin.h" "inttypes.h" "iso646.h" "limits.h" "lwpintrin.h" "lzcntintrin.h" "mm3dnow.h" "mm_malloc.h" "mmintrin.h" "module.modulemap" "msa.h" "mwaitxintrin.h" "nmmintrin.h" "opencl-c.h" "pkuintrin.h" "pmmintrin.h" "popcntintrin.h" "prfchwintrin.h" "rdseedintrin.h" "rtmintrin.h" "s390intrin.h" "sanitizer" "shaintrin.h" "smmintrin.h" "stdalign.h" "stdarg.h" "stdatomic.h" "stdbool.h" "stddef.h" "stdint.h" "stdnoreturn.h" "tbmintrin.h" "tgmath.h" "tmmintrin.h" "unwind.h" "vadefs.h" "varargs.h" "vecintrin.h" "wmmintrin.h" "x86intrin.h" "xmmintrin.h" "xopintrin.h" "xray" "xsavecintrin.h" "xsaveintrin.h" "xsaveoptintrin.h" "xsavesintrin.h" "xtestintrin.h" "unistd.h" "libaio.h"))

(defun +amos/add-include (header)
  "Add an #include line for `header' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (let ((incl (format "#include <%s>" header)))
    (save-excursion
      (if (search-backward incl nil t)
          (message "You already have %s." incl)
        (when (search-backward "#include" nil 'stop-at-top)
          (forward-line)
          (beginning-of-line))
        (insert incl)
        (newline)))))

(defun +amos/ivy-add-include ()
  (interactive)
  (ivy-read "Include: " (append +amos/default-include-headers
                                (split-string
                                 (shell-command-to-string "cd /usr/local/include ; find . -type f | sed 's=^./=='")))
            :require-match t
            :action #'+amos/add-include))

(defun +amos/add-library-link (library)
  "Add an -llibrary line for `library' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (let ((lib (format "%s \\" library)))
    (save-excursion
      (if (search-forward lib nil t)
          (message "You already have %s." lib)
        (when (re-search-forward "^-l.*\\\\$" nil 'stop-at-the-end 1)
          (forward-line)
          (beginning-of-line))
        (insert lib)
        (newline)))))

(defun +amos/ivy-add-library-link ()
  (interactive)
  (ivy-read "Library: " (split-string (shell-command-to-string "ldconfig -p | awk ' $1 ~ /^lib.*so$/ { print gensub(/^lib(.*).so$/, \"-l\\\\1\", 1, $1)}'"))
            :require-match t
            :action #'+amos/add-library-link))

(use-package cc-playground
  :commands (cc-playground cc-playground-mode)
  :config
  (add-hook 'cc-playground-hook (lambda () (interactive) (shell-command (format "rc --project-root=%s -c clang++ -std=c++17 -x c++ %s" (file-name-directory buffer-file-name) buffer-file-name))))
  (add-hook 'cc-playground-rm-hook (lambda () (interactive) (shell-command (format "rc -W %s" (file-name-directory buffer-file-name))))))
```

