// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

SEXP C_unwrap_image (SEXP);
static int is_single_numeric(SEXP _obj);

static const R_CallMethodDef callMethods[]  = {
  { "C_unwrap_image", (DL_FUNC) &C_unwrap_image, 3 },
  { NULL, NULL, 0 }
};

void R_init_subprocess(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

SEXP C_unwrap_image (SEXP _array, SEXP _dAlpha, SEXP _rMax, SEXP _dR) {
  if (!isArray(_array) || !isReal(_array)) {
    Rf_error("`_array` has to be a two-dimensional numeric array");
  }

  SEXP dims = PROTECT(Rf_getAttrib(_array, R_DimSymbol));
  if (!isInteger(dims) || Rf_length(dims) != 2) {
    UNPROTECT(1);
    Rf_error("`_array` has to have exactly two dimensions");
  }
  UNPROTECT(1);

  if (!is_single_numeric(_dAlpha)) {
    Rf_error("`_dAlpha` needs to be a single numeric value");
  }
  if (!is_single_numeric(_rMax)) {
    Rf_error("`_rMax` needs to be a single numeric value");
  }
  if (!is_single_numeric(_dR)) {
    Rf_error("`_dR` needs to be a single numeric value");
  }

  double * array = NUMERIC_DATA(_array);
  double dAlpha = NUMERIC_DATA(_dAlpha)[0];
  double rMax = NUMERIC_DATA(_rMax)[0];
  double dR = NUMERIC_DATA(_dR)[0];

  int cols = Rf_ncols(_array);
  int rows = Rf_nrows(_array);

  // iterate over alpha and r
  for (double alpha = 0; alpha <= 2*M_PI; alpha += dAlpha) {
    double cosAlpha = cos(alpha);
    double sinAlpha = sin(alpha);

    for (double r = 0; r <= rMax; r += dR) {
      int col = cols/2 + cosAlpha * r;
      int row = rows/2 + sinAlpha * r;
      array[col + row * cols];
    }
  }


  return R_NilValue;
}


static int is_single_numeric (SEXP _obj) {
  return isReal(_obj) && (LENGTH(_obj) == 1);
}
