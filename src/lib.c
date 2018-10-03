// RegisteringDynamic Symbols

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

SEXP C_unwrap_array (SEXP _array, SEXP _dAlpha, SEXP _rMax, SEXP _dR);
static int is_single_numeric(SEXP _obj);

static const R_CallMethodDef callMethods[]  = {
  { "C_unwrap_array", (DL_FUNC) &C_unwrap_array, 3 },
  { NULL, NULL, 0 }
};

void R_init_subprocess(DllInfo* info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

SEXP C_unwrap_array (SEXP _array, SEXP _dAlpha, SEXP _rMax, SEXP _dR) {
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

  int oCols = rMax/dR, oRows = 2*M_PI/dAlpha;
  SEXP ans = PROTECT(allocMatrix(REALSXP, oCols, oRows));
  double * output = NUMERIC_DATA(ans);

  // iterate over alpha and r
  for (int oCol = 0; oCol <= oCols; ++oCol) {
    double alpha = (M_PI * 2 * oCol) / oCols;
    double cosAlpha = cos(alpha), sinAlpha = sin(alpha);

    for (int oRow = 0; oRow < oRows; ++oRow) {
      double r = (oRow * rMax) / oRows;
      int col = cols/2 + cosAlpha * r,
          row = rows/2 + sinAlpha * r;
      output[oCol + oRow * oCols] = array[col + row * cols];
    }
  }

  UNPROTECT(1);
  return ans;
}


static int is_single_numeric (SEXP _obj) {
  return isReal(_obj) && (LENGTH(_obj) == 1);
}
