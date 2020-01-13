#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP R_bit_all(SEXP, SEXP);
extern SEXP R_bit_and(SEXP, SEXP, SEXP);
extern SEXP R_bit_any(SEXP, SEXP);
extern SEXP R_bit_as_hi(SEXP, SEXP, SEXP);
extern SEXP R_bit_done();
extern SEXP R_bit_equal(SEXP, SEXP, SEXP);
extern SEXP R_bit_extract(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_get(SEXP, SEXP, SEXP);
extern SEXP R_bit_get_integer(SEXP, SEXP, SEXP);
extern SEXP R_bit_init(SEXP);
extern SEXP R_bit_max(SEXP, SEXP);
extern SEXP R_bit_min(SEXP, SEXP);
extern SEXP R_bit_not(SEXP);
extern SEXP R_bit_or(SEXP, SEXP, SEXP);
extern SEXP R_bit_replace(SEXP, SEXP, SEXP);
extern SEXP R_bit_set(SEXP, SEXP, SEXP);
extern SEXP R_bit_set_attr(SEXP, SEXP, SEXP);
extern SEXP R_bit_set_integer(SEXP, SEXP, SEXP);
extern SEXP R_bit_shiftcopy(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_sum(SEXP, SEXP);
extern SEXP R_bit_vecseq(SEXP, SEXP);
extern SEXP R_bit_which(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_xor(SEXP, SEXP, SEXP);
extern SEXP first_zero(SEXP);
extern SEXP int_check_ascending(SEXP);
extern SEXP int_check_descending(SEXP);
extern SEXP int_rle(SEXP);
extern SEXP r_ram_truly_identical(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"R_bit_all",             (DL_FUNC) &R_bit_all,             2},
    {"R_bit_and",             (DL_FUNC) &R_bit_and,             3},
    {"R_bit_any",             (DL_FUNC) &R_bit_any,             2},
    {"R_bit_as_hi",           (DL_FUNC) &R_bit_as_hi,           3},
    {"R_bit_done",            (DL_FUNC) &R_bit_done,            0},
    {"R_bit_equal",           (DL_FUNC) &R_bit_equal,           3},
    {"R_bit_extract",         (DL_FUNC) &R_bit_extract,         4},
    {"R_bit_get",             (DL_FUNC) &R_bit_get,             3},
    {"R_bit_get_integer",     (DL_FUNC) &R_bit_get_integer,     3},
    {"R_bit_init",            (DL_FUNC) &R_bit_init,            1},
    {"R_bit_max",             (DL_FUNC) &R_bit_max,             2},
    {"R_bit_min",             (DL_FUNC) &R_bit_min,             2},
    {"R_bit_not",             (DL_FUNC) &R_bit_not,             1},
    {"R_bit_or",              (DL_FUNC) &R_bit_or,              3},
    {"R_bit_replace",         (DL_FUNC) &R_bit_replace,         3},
    {"R_bit_set",             (DL_FUNC) &R_bit_set,             3},
    {"R_bit_set_attr",        (DL_FUNC) &R_bit_set_attr,        3},
    {"R_bit_set_integer",     (DL_FUNC) &R_bit_set_integer,     3},
    {"R_bit_shiftcopy",       (DL_FUNC) &R_bit_shiftcopy,       5},
    {"R_bit_sum",             (DL_FUNC) &R_bit_sum,             2},
    {"R_bit_vecseq",          (DL_FUNC) &R_bit_vecseq,          2},
    {"R_bit_which",           (DL_FUNC) &R_bit_which,           4},
    {"R_bit_xor",             (DL_FUNC) &R_bit_xor,             3},
    {"first_zero",            (DL_FUNC) &first_zero,            1},
    {"int_check_ascending",   (DL_FUNC) &int_check_ascending,   1},
    {"int_check_descending",  (DL_FUNC) &int_check_descending,  1},
    {"int_rle",               (DL_FUNC) &int_rle,               1},
    {"r_ram_truly_identical", (DL_FUNC) &r_ram_truly_identical, 2},
    {NULL, NULL, 0}
};

void R_init_bit(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
