extern "C" {
#include <R.h>
#include <Rdefines.h>
#include <pcre.h>
#include "fixsel.hpp"


SEXP _pcre(const char *spat, const char *s, pcre *re_pcre, pcre_extra *re_pe, int subpattern)
{
    int matchIndex = -1, st = 0, foundAll = 0, foundAny = 0, j, start=0;
    SEXP ans, matchlen;         /* return vect and its attribute */
    SEXP matchbuf, matchlenbuf; /* buffers for storing multiple matches */
    int bufsize = 1024;         /* starting size for buffers */
    PROTECT(matchbuf = allocVector(INTSXP, bufsize));
    PROTECT(matchlenbuf = allocVector(INTSXP, bufsize));
    while (!foundAll) {
    int rc, ovector[60], slen = strlen(s);
    rc = pcre_exec(re_pcre, re_pe, s, slen, start, 0, ovector, 60); 
    if (rc >= 0 && subpattern < rc) {
        if ((matchIndex + 1) == bufsize) {
        /* Reallocate match buffers */
        int newbufsize = bufsize * 2;
        SEXP tmp;
        tmp = allocVector(INTSXP, 2 * bufsize);
        for (j = 0; j < bufsize; j++)
            INTEGER(tmp)[j] = INTEGER(matchlenbuf)[j];
        UNPROTECT(1);
        matchlenbuf = tmp;
        PROTECT(matchlenbuf);
        tmp = allocVector(INTSXP, 2 * bufsize);
        for (j = 0; j < bufsize; j++)
            INTEGER(tmp)[j] = INTEGER(matchbuf)[j];
        matchbuf = tmp;
        UNPROTECT(2);
        PROTECT(matchbuf);
        PROTECT(matchlenbuf);
        bufsize = newbufsize;
        }
        matchIndex++;
        foundAny = 1;
        st = ovector[subpattern*2]; 
        INTEGER(matchbuf)[matchIndex] = st + 1; /* index from one */
        INTEGER(matchlenbuf)[matchIndex] = ovector[subpattern*2+1] - st; 
        /* we need to advance 'start' in bytes */
        if (INTEGER(matchlenbuf)[matchIndex] == 0)
        start = ovector[0] + 1;
        else
        start = ovector[1];
        if (start >= slen) foundAll = 1;
    } else {
        foundAll = 1;
        if (!foundAny) matchIndex = 0;
    }
    }
    PROTECT(ans = allocVector(INTSXP, matchIndex + 1));
    PROTECT(matchlen = allocVector(INTSXP, matchIndex + 1));
    if (foundAny) {
    /* copy from buffers */
    for (j = 0; j <= matchIndex; j++) {
        INTEGER(ans)[j] = INTEGER(matchbuf)[j];
        INTEGER(matchlen)[j] = INTEGER(matchlenbuf)[j];
    }
    } else INTEGER(ans)[0] = INTEGER(matchlen)[0] = -1;
    setAttrib(ans, install("match.length"), matchlen);
    UNPROTECT(4);
    
    return ans;
    //return(R_NilValue);
}


SEXP _gregexpr(SEXP _pattern, SEXP _text, SEXP _subpattern)
{
    SEXP ans;
    pcre *re_pcre;
    pcre_extra *re_pe;
    int erroffset;
    const char *errorptr;
    //int options = 0;
    const unsigned char* tables = pcre_maketables();
    
    const char* spat = as_string(_pattern);
    re_pcre = pcre_compile(spat, 0, &errorptr, &erroffset, tables);
    if (!re_pcre)
    {
        if (errorptr)
        Rprintf("PCRE pattern compilation error\n\t'%s'\n\tat '%s'\n",
            errorptr, spat+erroffset);
        Rprintf("invalid regular expression '%s'\n", spat);
    }
    re_pe = pcre_study(re_pcre, 0, &errorptr);
    
    int n = LENGTH(_text);
    SEXP elt;
    PROTECT(ans = allocVector(VECSXP, n));
    const void *vmax = vmaxget();
    
    for (int i = 0 ; i < n ; i++) {
        if (STRING_ELT(_text, i) == NA_STRING) {
        elt = gregexpr_NAInputAns();
        } else {
            const char* s = as_string(_text,i);
            elt = _pcre(spat, s, re_pcre, re_pe, *REAL(_subpattern));
        }
        SET_VECTOR_ELT(ans, i, elt);
        vmaxset(vmax);
    }
    
    if (re_pe) pcre_free(re_pe);
    pcre_free(re_pcre);
    pcre_free((void *)tables);
    
    UNPROTECT(1);
    return ans;
    
}




} // extern "C"
